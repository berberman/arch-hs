{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import qualified Algebra.Graph.AdjacencyMap.Algorithm as G
import qualified Algebra.Graph.Labelled.AdjacencyMap as GL
import Args
import Control.Monad (filterM, forM_, unless)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as LBS
import Data.Conduit.Process (system)
import Data.Containers.ListUtils (nubOrd)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.List.NonEmpty (toList)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Distribution.ArchHs.Aur (Aur, aurToIO, isInAur)
import Distribution.ArchHs.CommunityDB
import Distribution.ArchHs.Core
import Distribution.ArchHs.Exception
import Distribution.ArchHs.FilesDB
import Distribution.ArchHs.Hackage
import Distribution.ArchHs.Internal.Prelude
import Distribution.ArchHs.Local
import Distribution.ArchHs.Name
import Distribution.ArchHs.Options
import Distribution.ArchHs.PP
import qualified Distribution.ArchHs.PkgBuild as N
import Distribution.ArchHs.Types
import Distribution.ArchHs.Utils
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Json
import Network.HTTP.Client (Manager)
import Network.HTTP.Client.TLS (newTlsManager)
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.FilePath (takeFileName)

app ::
  Members '[Embed IO, State (Set.Set PackageName), KnownGHCVersion, CommunityEnv, HackageEnv, FlagAssignmentsEnv, DependencyRecord, Trace, Aur, WithMyErr] r =>
  PackageName ->
  FilePath ->
  Bool ->
  [String] ->
  Bool ->
  Bool ->
  Bool ->
  FilePath ->
  (DBKind -> IO FilesDB) ->
  Sem r ()
app target path aurSupport skip uusi force installDeps jsonPath loadFilesDB' = do
  (deps, sublibs, sysDeps) <- getDependencies (fmap mkUnqualComponentName skip) Nothing target

  inCommunity <- isInCommunity target

  when inCommunity $
    if force
      then printWarn $ "Target has been provided by" <+> ppCommunity <> comma <+> "but you passed --force"
      else throw $ TargetExist target ByCommunity

  when aurSupport $ do
    inAur <- isInAur target
    when inAur $
      if force
        then printWarn $ "Target has been provided by" <+> ppAur <> comma <+> "but you passed --force"
        else throw $ TargetExist target ByAur

  let removeSublibs pkgs =
        pkgs ^.. each . filtered (\x -> x ^. pkgName `notElem` sublibs) & each %~ (\x -> x & pkgDeps %~ filter (\d -> d ^. depName `notElem` sublibs))
      grouped = removeSublibs $ groupDeps deps
      namesFromSolved x = x ^.. each . pkgName <> x ^.. each . pkgDeps . each . depName
      allNames = nubOrd $ namesFromSolved grouped
  communityProvideList <- (<> ghcLibList) <$> filterM (\x -> if x == target && force then return False else isInCommunity x) allNames

  let providedPackages = filter (\x -> x ^. pkgName `elem` communityProvideList) grouped
      abnormalDependencies =
        mapMaybe
          ( \x -> case filter (`notElem` communityProvideList) (x ^. pkgDeps ^.. each . depName) of
              [] -> Nothing
              pkgs -> Just (x ^. pkgName, pkgs)
          )
          providedPackages
      -- all missing transitive dependencies, excluding direct dependencies of the target
      missingChildren = mconcat $ snd <$> filter (\x -> fst x /= target) abnormalDependencies

  embed $
    forM_ abnormalDependencies $ \(T.pack . unPackageName -> parent, children) -> do
      printWarn $ "Package" <+> dquotes (pretty parent) <+> "is provided without" <> colon
      forM_ children $ putStrLn . unPackageName

  let fillProvidedPkgs provideList provider = map (\x -> if (x ^. pkgName) `elem` provideList then ProvidedPackage (x ^. pkgName) provider else x)
      fillProvidedDeps provideList provider = map (pkgDeps %~ each %~ (\y -> if y ^. depName `elem` provideList then y & depProvider ?~ provider else y))
      filledByCommunity = fillProvidedPkgs communityProvideList ByCommunity . fillProvidedDeps communityProvideList ByCommunity $ grouped
      toBePacked1 = filledByCommunity ^.. each . filtered (not . isProvided)
  (filledByBoth, toBePacked2) <- do
    when aurSupport $ printInfo "Start searching AUR..."
    aurProvideList <-
      if aurSupport
        then filterM (\n -> do printInfo ("Searching" <+> viaPretty n); isInAur n) $ filter (\x -> not $ x == target && force) $ toBePacked1 ^.. each . pkgName
        else return []
    let a = fillProvidedPkgs aurProvideList ByAur . fillProvidedDeps aurProvideList ByAur $ filledByCommunity
        b = a ^.. each . filtered (not . isProvided)
    return (a, b)

  when (null filledByBoth) $
    throw $ TargetDisappearException target

  printInfo "Solved:"
  embed $ T.putStrLn . prettySolvedPkgs $ filledByBoth

  printInfo "Recommended package order:"
  -- remove missingChildren from the graph, so that we don't need package them if they are not our target
  let vertexesToBeRemoved = missingChildren <> filledByBoth ^.. each . filtered isProvided ^.. each . pkgName
      removeSelfCycle g = foldr (\n acc -> GL.removeEdge n n acc) g $ toBePacked2 ^.. each . pkgName
      newGraph = GL.induce (`notElem` vertexesToBeRemoved) deps
  flattened <- case G.topSort . GL.skeleton $ removeSelfCycle newGraph of
    Left c -> throw . CyclicExist $ toList c
    Right x -> return $ filter (`notElem` sublibs) x

  embed . putDoc $ (prettyDeps . reverse $ flattened) <> line <> line

  let sysDepsToBePacked = Map.filterWithKey (\k _ -> k `elem` flattened) sysDeps

  unless (null sysDepsToBePacked) $ do
    printInfo "Detected pkgconfig or extraLib from target(s):"
    embed $ T.putStrLn $ ppSysDependencies sysDepsToBePacked

  sysDepsRef <- embed . newIORef $ toUnsolved <$> nubOrd (Map.foldMapWithKey (\_ x -> x) sysDepsToBePacked)

  embed $
    isAllSolvedM sysDepsRef >>= \b -> unless b $ do
      printInfo "Now finding corresponding system package(s) using files db:"
      coreFiles <- loadFilesDB' Core
      modifyIORef' sysDepsRef $ fmap (trySolve coreFiles)
      b' <- isAllSolvedM sysDepsRef
      unless b' $ do
        extraFiles <- loadFilesDB' Extra
        modifyIORef' sysDepsRef $ fmap (trySolve extraFiles)
        b'' <- isAllSolvedM sysDepsRef
        unless b'' $ do
          communityFiles <- loadFilesDB' Community
          modifyIORef' sysDepsRef $ fmap (trySolve communityFiles)

  sysDepsResult <- embed $ readIORef sysDepsRef

  embed . unless (null sysDepsToBePacked) $ do
    printInfo "Done:"
    T.putStrLn . align2col $ ppEmergedSysDep <$> sysDepsResult
    unless (isAllSolved sysDepsResult) $ printWarn "Unable to obtain all required system packages"

  let sysDepsMapping = collectAllSolved sysDepsResult
      getSysDeps name = nubOrd $ catMaybes [sysDepsMapping Map.!? file | (SystemDependency file) <- fromMaybe [] $ sysDeps Map.!? name]

  flags <- filter (\(_, l) -> not $ null l) <$> mapM (\n -> (n,) <$> getPackageFlag n) flattened

  embed $
    unless (null flags) $ do
      printInfo "Detected flag(s) from targets:"
      putDoc $ prettyFlags flags <> line <> line

  let jsonOutput =
        ArchHSOutput
          (fromAbnormalDependency <$> abnormalDependencies)
          (fromSolvedPackage <$> filledByBoth)
          (reverse flattened)
          (fromEmergedSysDep <$> sysDepsResult)
          (fromFlag <$> flags)

  embed $
    unless (null jsonPath) $ do
      LBS.writeFile jsonPath $ A.encode jsonOutput
      printInfo $ "Write file" <> colon <+> pretty jsonPath

  unless (null path) $
    mapM_
      ( \solved -> do
          pkgBuild <- cabalToPkgBuild solved uusi $ getSysDeps (solved ^. pkgName)
          let pName = N._pkgName pkgBuild
              dir = path </> pName
              fileName = dir </> "PKGBUILD"
              txt = N.applyTemplate pkgBuild
          embed $ do
            createDirectoryIfMissing True dir
            writeFile fileName txt
            printInfo $ "Write file" <> colon <+> pretty fileName
      )
      toBePacked2

  when installDeps $ do
    let providedDepends pkg =
          pkg ^. pkgDeps
            ^.. each
              . filtered (\x -> depNotMyself (pkg ^. pkgName) x && depNotInGHCLib x && x ^. depProvider == Just ByCommunity)
        toStr = unArchLinuxName . toArchLinuxName . _depName
        depends = unwords . nubOrd . fmap toStr . mconcat $ providedDepends <$> toBePacked2
        flattened' = filter (/= target) flattened
    case flattened' of
      [] -> pure ()
      [x] -> printWarn $ "The following dependency is missing in" <+> ppCommunity <> colon <+> pretty (unPackageName x)
      xs -> printWarn $ "Following dependencies are missing in" <+> ppCommunity <> colon <+> hsep (punctuate comma (pretty . unPackageName <$> xs))
    embed $ putDoc line
    case depends of
      [] -> printInfo "No extra dependency to install"
      xs ->
        embed (system $ "sudo pacman --needed -S " <> xs) >>= \case
          ExitSuccess -> printSuccess "Installed successfully"
          ExitFailure c -> printError $ "pacman exited with" <+> pretty c

-----------------------------------------------------------------------------
data EmergedSysDep = Solved File ArchLinuxName | Unsolved File
  deriving stock (Eq, Ord, Show)

toUnsolved :: SystemDependency -> EmergedSysDep
toUnsolved (SystemDependency x) = Unsolved x

trySolve :: FilesDB -> EmergedSysDep -> EmergedSysDep
trySolve db dep
  | (Unsolved x) <- dep,
    (pkg : _) <- lookupPkg x db =
    Solved x pkg
  | otherwise = dep

isAllSolved :: [EmergedSysDep] -> Bool
isAllSolved xs = null [() | (Unsolved _) <- xs]

isAllSolvedM :: IORef [EmergedSysDep] -> IO Bool
isAllSolvedM ref = isAllSolved <$> readIORef ref

collectAllSolved :: [EmergedSysDep] -> Map.Map File ArchLinuxName
collectAllSolved xs = Map.fromList [(file, name) | (Solved file name) <- xs]

ppEmergedSysDep :: EmergedSysDep -> (Doc AnsiStyle, Doc AnsiStyle)
ppEmergedSysDep (Solved file (ArchLinuxName name)) = (annGreen . pretty $ file, "   ⇒   " <> (annCyan . pretty $ name))
ppEmergedSysDep (Unsolved file) = (annYellow . annBold . pretty $ file, indent 19 cuo)

fromEmergedSysDep :: EmergedSysDep -> SysDepsS
fromEmergedSysDep (Unsolved file) = SysDepsS file Nothing
fromEmergedSysDep (Solved file pkg) = SysDepsS file (Just pkg)

-----------------------------------------------------------------------------

runApp ::
  HackageDB ->
  CommunityDB ->
  Map.Map PackageName FlagAssignment ->
  Bool ->
  FilePath ->
  IORef (Set.Set PackageName) ->
  Manager ->
  Sem '[CommunityEnv, HackageEnv, FlagAssignmentsEnv, DependencyRecord, Trace, State (Set.Set PackageName), Aur, WithMyErr, Embed IO, Final IO] a ->
  IO (Either MyException a)
runApp hackage community flags traceStdout tracePath ref manager =
  runFinal
    . embedToFinal
    . errorToIOFinal
    . aurToIO manager
    . runStateIORef ref
    . runTrace traceStdout tracePath
    . evalState Map.empty
    . runReader flags
    . runReader hackage
    . runReader community

runTrace :: Member (Embed IO) r => Bool -> FilePath -> Sem (Trace ': r) a -> Sem r a
runTrace stdout path = interpret $ \case
  Trace m -> do
    when stdout (embed $ putStrLn m)
    unless (null path) (embed $ appendFile path (m ++ "\n"))

-----------------------------------------------------------------------------

main :: IO ()
main = printHandledIOException $
  do
    setLocaleEncoding utf8
    Options {..} <- runArgsParser

    unless (null optFileTrace) $ do
      printInfo $ "Trace will be dumped to" <+> pretty optFileTrace
      writeFile optFileTrace ""

    unless (null optJson) $ do
      printInfo $ "Output will be dumped to" <+> pretty optJson <+> "as json"
      writeFile optJson ""

    let isFlagEmpty = Map.null optFlags
        isSkipEmpty = null optSkip

    unless isFlagEmpty $ do
      printInfo "You assigned flags:"
      putDoc $ prettyFlagAssignments optFlags <> line <> line

    unless isSkipEmpty $ do
      printInfo "You chose to skip:"
      putDoc $ prettySkip optSkip <> line <> line

    when optAur $ printInfo "You passed -a, searching AUR may takes a long time"

    when optUusi $ printInfo "You passed --uusi, uusi will become makedepends of each package"

    hackage <- loadHackageDBFromOptions optHackage

    let isExtraEmpty = null optExtraCabalDirs
    optExtraCabal <- mapM findCabalFile optExtraCabalDirs

    unless isExtraEmpty $
      printInfo $ "You added" <+> hsep (punctuate comma $ pretty . takeFileName <$> optExtraCabal) <+> "as extra cabal file(s), starting parsing right now"

    parsedExtra <- mapM parseCabalFile optExtraCabal

    let newHackage = foldr insertDB hackage parsedExtra

    community <- loadCommunityDBFromOptions optCommunityDB

    printInfo "Start running..."

    ref <- newIORef Set.empty

    manager <- newTlsManager

    runApp
      newHackage
      community
      optFlags
      optStdoutTrace
      optFileTrace
      ref
      manager
      (subsumeGHCVersion $ app optTarget optOutputDir optAur optSkip optUusi optForce optInstallDeps optJson (loadFilesDBFromOptions optFilesDB))
      & printAppResult

-----------------------------------------------------------------------------

groupDeps :: GL.AdjacencyMap (Set.Set DependencyType) PackageName -> [SolvedPackage]
groupDeps graph =
  fmap
    ( \(name, deps) ->
        SolvedPackage name $ fmap (uncurry . flip $ SolvedDependency Nothing) deps
    )
    $ result <> aloneChildren
  where
    result =
      fmap ((\(a, b, c) -> (head b, zip a c)) . unzip3)
        . groupBy (\x y -> uncurry (==) (getTwo _2 x y))
        . fmap (_1 %~ Set.toList)
        . GL.edgeList
        $ graph
    parents = fmap fst result
    children = mconcat $ fmap (\(_, ds) -> fmap snd ds) result
    -- Maybe 'G.vertexSet' is a better choice
    aloneChildren = nubOrd $ zip (filter (`notElem` parents) children) (repeat [])
