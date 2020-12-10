{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import qualified Algebra.Graph.AdjacencyMap.Algorithm as G
import qualified Algebra.Graph.Labelled.AdjacencyMap as GL
import Args
import qualified Colourista as C
import Control.Monad (filterM, forM_, unless)
import Data.Containers.ListUtils (nubOrd)
import Data.IORef (IORef, newIORef)
import Data.List.NonEmpty (toList)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import qualified Data.Text as T
import Distribution.ArchHs.Aur (Aur, aurToIO, isInAur)
import Distribution.ArchHs.Community
import Distribution.ArchHs.Core
import Distribution.ArchHs.Exception
import Distribution.ArchHs.Hackage
import Distribution.ArchHs.Internal.Prelude
import Distribution.ArchHs.Local
import Distribution.ArchHs.Name
import Distribution.ArchHs.PP
import qualified Distribution.ArchHs.PkgBuild as N
import Distribution.ArchHs.Types
import Distribution.ArchHs.Utils
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeFileName)

app ::
  Members '[Embed IO, State (Set.Set PackageName), CommunityEnv, HackageEnv, FlagAssignmentsEnv, DependencyRecord, Trace, Aur, WithMyErr] r =>
  PackageName ->
  FilePath ->
  Bool ->
  [String] ->
  Bool ->
  Bool ->
  FilePath ->
  Sem r ()
app target path aurSupport skip uusi force metaPath = do
  (deps, sublibs) <- getDependencies (fmap mkUnqualComponentName skip) Nothing target

  inCommunity <- isInCommunity target

  when inCommunity $ 
    if force
      then embed $ C.warningMessage $ "Target has been provided by [community], ignore it"
      else throw $ TargetExist target ByCommunity

  when aurSupport $ do
    inAur <- isInAur target
    when inAur $ 
      if force
      then embed $ C.warningMessage $ "Target has been provided by [aur], ignore it"
      else throw $ TargetExist target ByAur

  let removeSublibs list =
        list ^.. each . filtered (\x -> x ^. pkgName `notElem` sublibs) & each %~ (\x -> x & pkgDeps %~ filter (\d -> d ^. depName `notElem` sublibs))
      grouped = removeSublibs $ groupDeps deps
      namesFromSolved x = x ^.. each . pkgName <> x ^.. each . pkgDeps . each . depName
      allNames = nubOrd $ namesFromSolved grouped
  communityProvideList <- (<> ghcLibList) <$> filterM (\x -> if x == target && force then return False else isInCommunity x) allNames

  let providedPackages = filter (\x -> x ^. pkgName `elem` communityProvideList) grouped
      abnormalDependencies =
        mapMaybe
          ( \x -> case filter (`notElem` communityProvideList) (x ^. pkgDeps ^.. each . depName) of
              [] -> Nothing
              list -> Just (x ^. pkgName, list)
          )
          providedPackages

  embed $
    forM_ abnormalDependencies $ \(T.pack . unPackageName -> parent, childs) -> do
      C.warningMessage $ "Package \"" <> parent <> "\" is provided without:"
      forM_ childs $ putStrLn . unPackageName

  let fillProvidedPkgs provideList provider = map (\x -> if (x ^. pkgName) `elem` provideList then ProvidedPackage (x ^. pkgName) provider else x)
      fillProvidedDeps provideList provider = map (pkgDeps %~ each %~ (\y -> if y ^. depName `elem` provideList then y & depProvider ?~ provider else y))
      filledByCommunity = fillProvidedPkgs communityProvideList ByCommunity . fillProvidedDeps communityProvideList ByCommunity $ grouped
      toBePacked1 = filledByCommunity ^.. each . filtered (not . isProvided)
  (filledByBoth, toBePacked2) <- do
    embed . when aurSupport $ C.infoMessage "Start searching AUR..."
    aurProvideList <-
      if aurSupport
        then filterM (\n -> do embed $ C.infoMessage ("Searching " <> T.pack (unPackageName n)); isInAur n) $ filter (\x -> not $ x == target && force) $ toBePacked1 ^.. each . pkgName
        else return []
    let a = fillProvidedPkgs aurProvideList ByAur . fillProvidedDeps aurProvideList ByAur $ filledByCommunity
        b = a ^.. each . filtered (not . isProvided)
    return (a, b)

  when (null filledByBoth) $
    throw $ TargetDisappearException target

  embed $ C.infoMessage "Solved:"
  embed $ putStrLn . prettySolvedPkgs $ filledByBoth

  embed $ C.infoMessage "Recommended package order:"
  let vertexesToBeRemoved = filledByBoth ^.. each . filtered isProvided ^.. each . pkgName
      removeSelfCycle g = foldr (\n acc -> GL.removeEdge n n acc) g $ toBePacked2 ^.. each . pkgName
      newGraph = GL.induce (`notElem` vertexesToBeRemoved) deps
  flattened <- case G.topSort . GL.skeleton $ removeSelfCycle newGraph of
    Left c -> throw . CyclicExist $ toList c
    Right x -> return $ filter (`notElem` sublibs) x
  embed $ putStrLn . prettyDeps . reverse $ flattened
  flags <- filter (\(_, l) -> not $ null l) <$> mapM (\n -> (n,) <$> getPackageFlag n) flattened

  embed $
    unless (null flags) $ do
      C.infoMessage "Detected flag(s) from targets:"
      putStrLn . prettyFlags $ flags

  unless (null path) $
    mapM_
      ( \solved -> do
          pkgBuild <- cabalToPkgBuild solved uusi
          let pName = N._pkgName pkgBuild
              dir = path </> pName
              fileName = dir </> "PKGBUILD"
              txt = N.applyTemplate pkgBuild
          embed $ do
            createDirectoryIfMissing True dir
            writeFile fileName txt
            C.infoMessage $ "Write file: " <> T.pack fileName
      )
      toBePacked2

  unless (null metaPath) $ do
    cabal <- getLatestCabal target
    let url = getUrl $ packageDescription cabal
        name = unPackageName target
        template = N.metaTemplate (T.pack url) (T.pack name)
        providedDepends pkg =
          pkg ^. pkgDeps
            ^.. each
              . filtered (\x -> depNotMyself (pkg ^. pkgName) x && depNotInGHCLib x && x ^. depProvider == Just ByCommunity)
        toStr x = "'" <> (unCommunityName . toCommunityName . _depName) x <> "'"
        depends = case unwords . nubOrd . fmap toStr . mconcat $ providedDepends <$> toBePacked2 of
          [] -> ""
          xs -> " " <> xs
        flattened' = filter (/= target) flattened
        comment = case flattened' of
          [] -> "\n"
          [x] -> "# The following dependency is missing in community: " <> unPackageName x
          _ -> "# Following dependencies are missing in community:" <> intercalate ", " (unPackageName <$> flattened')
        txt = template (T.pack comment) (T.pack depends)
        dir = metaPath </> "haskell-" <> name <> "-meta"
        fileName = dir </> "PKGBUILD"
    embed $ do
      createDirectoryIfMissing True dir
      writeFile fileName (T.unpack txt)
      C.infoMessage $ "Write file: " <> T.pack fileName

-----------------------------------------------------------------------------

runApp ::
  HackageDB ->
  CommunityDB ->
  Map.Map PackageName FlagAssignment ->
  Bool ->
  FilePath ->
  IORef (Set.Set PackageName) ->
  Sem '[CommunityEnv, HackageEnv, FlagAssignmentsEnv, DependencyRecord, Trace, State (Set.Set PackageName), Aur, WithMyErr, Embed IO, Final IO] a ->
  IO (Either MyException a)
runApp hackage community flags traceStdout tracePath ref =
  runFinal
    . embedToFinal
    . errorToIOFinal
    . aurToIO
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
    Options {..} <- runArgsParser

    unless (null optFileTrace) $ do
      C.infoMessage $ "Trace will be dumped to " <> T.pack optFileTrace <> "."
      writeFile optFileTrace ""

    let useDefaultHackage = "YOUR_HACKAGE_MIRROR" `isInfixOf` optHackagePath
    when useDefaultHackage $ C.skipMessage "You didn't pass -h, use hackage index file from default path."

#ifndef ALPM
    let useDefaultCommunity = "/var/lib/pacman/sync/community.db" == optCommunityPath
    when useDefaultCommunity $ C.skipMessage "You didn't pass -c, use community db file from default path."
#endif

    let isFlagEmpty = Map.null optFlags
        isSkipEmpty = null optSkip

    when isFlagEmpty $ C.skipMessage "You didn't pass -f, different flag assignments may make difference in dependency resolving."
    unless isFlagEmpty $ do
      C.infoMessage "You assigned flags:"
      putStrLn . prettyFlagAssignments $ optFlags

    unless isSkipEmpty $ do
      C.infoMessage "You chose to skip:"
      putStrLn $ prettySkip optSkip

    when optAur $ C.infoMessage "You passed -a, searching AUR may takes a long time."

    when optUusi $ C.infoMessage "You passed --uusi, uusi will become makedepends of each package."

    hackage <- loadHackageDB =<< if useDefaultHackage then lookupHackagePath else return optHackagePath
    C.infoMessage "Loading hackage..."

    let isExtraEmpty = null optExtraCabalPath

    unless isExtraEmpty $
      C.infoMessage $ "You added " <> (T.pack . intercalate ", " $ map takeFileName optExtraCabalPath) <> " as extra cabal file(s), starting parsing right now."

    parsedExtra <- mapM parseCabalFile optExtraCabalPath

    let newHackage = foldr insertDB hackage parsedExtra

#ifdef ALPM
    when optAlpm $ C.infoMessage "Using alpm."
    community <- if optAlpm then loadCommunityFFI else loadProcessedCommunity defaultCommunityPath
#else
    community <- loadProcessedCommunity $ if useDefaultCommunity then defaultCommunityPath else optCommunityPath
#endif

    C.infoMessage "Loading community.db..."

    C.infoMessage "Start running..."

    empty <- newIORef Set.empty

    runApp newHackage community optFlags optStdoutTrace optFileTrace empty (app optTarget optOutputDir optAur optSkip optUusi optForce optMetaDir) & printAppResult

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
