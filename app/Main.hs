{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import qualified Algebra.Graph.AdjacencyMap.Algorithm as G
import qualified Algebra.Graph.Labelled.AdjacencyMap as G
import Conduit
import Control.DeepSeq (NFData)
import Control.Exception
import Control.Monad.Except
import Control.Monad.Reader
import Data.Char (toLower)
import qualified Data.Conduit.Tar as Tar
import qualified Data.Conduit.Zlib as Zlib
import Data.List (delete, groupBy, intercalate, isInfixOf)
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import qualified Data.Set as S
import qualified Debug.Trace as D
import Distribution.Compiler
import qualified Distribution.Hackage.DB as DB
import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration
import Distribution.System
import Distribution.Types.CondTree
import Distribution.Types.Dependency
import Distribution.Types.ExeDependency
import Distribution.Types.GenericPackageDescription
import qualified Distribution.Types.PackageId as I
import Distribution.Types.PackageName
import Distribution.Types.Version
import Distribution.Types.VersionRange
import GHC.Generics (Generic)
import Lens.Micro
import Lens.Micro.Mtl
import Lens.Micro.TH (makeLenses)
import PkgBuild
import System.Environment

{- For ghc 8.8.3, the following modules are included in
  GenericPackageDescription.
import Distribution.Types.ConfVar
import Distribution.Types.Flag -}

data MyException
  = PkgNotFound PackageName
  | VersionError
  | UrlError PackageName
  | TargetExist PackageName
  deriving stock (Show, Eq)
  deriving anyclass (Exception)

data DependencyType
  = Exe
  | Lib
  | Test
  | Benchmark
  | LibBuildTools
  | TestBuildTools
  | BenchmarkBuildTools
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

type CollectedDependencies = [PackageName]

type CommunityDB = S.Set String

data SolvedDependency = SolvedDependency {_provided :: Bool, _depName :: PackageName, _depType :: [DependencyType]}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

data SolvedPackage = ProvidedPackage {_pkgName :: PackageName} | SolvedPackage {_pkgName :: PackageName, _pkgDeps :: [SolvedDependency]}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

data HsEnv = HsEnv {_hackage :: DB.HackageDB, _community :: CommunityDB, _flags :: FlagAssignment}

type HsM m = ExceptT MyException (ReaderT HsEnv m)

runHsM :: r -> ExceptT e (ReaderT r m) a -> m (Either e a)
runHsM = flip (runReaderT . runExceptT)

makeLenses ''SolvedDependency
makeLenses ''SolvedPackage
makeLenses ''HsEnv

-----------------------------------------------------------------------------

getHsEnv :: IO HsEnv
getHsEnv = do
  _hackage <- defaultHackageDB
  _community <- fmap S.fromList $ runConduitRes $ loadCommunity defaultCommunityPath .| cookCommunity .| sinkList
  let _flags = mkFlagAssignment []
  return HsEnv {..}

h :: MonadIO m => HsEnv -> m (Either MyException ())
h env = runHsM env $ do
  name <- liftIO $ head <$> getArgs
  let target = mkPackageName name
  deps <- getDependencies S.empty 0 True target
  liftIO $ putStrLn $ "Build target: " ++ show target
  exist <- isInCommunity target
  when exist $ throwError $ TargetExist target
  liftIO $ putStrLn . prettyDeps $ G.reachable target $ G.skeleton deps

  let d = groupDeps $ deps
      v = S.toList $ S.fromList (d ^.. each . pkgName ++ d ^.. each . pkgDeps . each . depName)
  providedList <- fmap (++ ghcLibList) $ filterM (isInCommunity) v
  let q = d <&> (\x -> if (x ^. pkgName) `elem` providedList then ProvidedPackage (x ^. pkgName) else x)
      r = q <&> pkgDeps %~ each %~ (\y -> if y ^. depName `elem` providedList then y & provided .~ True else y)
      s = r ^.. each . filtered (\case ProvidedPackage {..} -> False; _ -> True)
  liftIO $ putStrLn . prettySolvedPkgs $ r
  return ()

main = do
  env <- getHsEnv
  h env >>= print

-----------------------------------------------------------------------------

-- h = runHsM getHsEnv $ do
--

--   let
--       target = mkPackageName name
--       deps = getDependencies hackage S.empty 0 True target
--   putStrLn $ "Build target: " ++ show target
--   guard . not $ isInCommunity community target
--   putStrLn . prettyDeps $ G.reachable target $ G.skeleton deps
--   let d = groupDeps $ deps
--       v = S.toList $ S.fromList (d ^.. each . pkgName ++ d ^.. each . pkgDeps . each . depName)
--       providedList = filter (isInCommunity community) v ++ ghcLibList

--   putStrLn . prettySolvedPkgs $ r

--   mapM (\solved -> writeFile ("/home/berberman/Desktop/test/" <> (solved ^. pkgName & unPackageName) <> ".PKGBUILD") . applyTemplate $ cabalToPkgBuild hackage solved) s
--   return ()

-- data F
--  = Version
--    deriving Show

-- opts :: [OptDescr F]
-- opts =
--  [ Option ['V','?'] ["version"] (NoArg Version)       "show version number"]

-- compilerOpts :: [String] -> IO ([F], [String])
-- compilerOpts argv =
--    case getOpt Permute opts argv of
--       (o,n,[]  ) -> return (o,n)
--       (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header opts))
--   where header = "Usage: stack exec -- [OPTION...] files..."

-- main :: IO ()
-- main = do
--   hackage <- defaultHackageDB
--   let target = mkPackageName "process"
--       cabal = getLatestCabal' hackage target
--   -- print $ cabal & packageDescription
--   -- print $ cabal & genPackageFlags
--   -- print $ cabal & fmap condTreeComponents.condLibrary
--   -- print $ cabal & fmap condTreeConstraints.condLibrary
--   -- print $ cabal & fmap condTreeData.condLibrary
--   print $ cabal & fmap (simplifyCondTree (\case OS x -> D.traceShow x Right True; x -> Left x)) . condLibrary

archEnv :: FlagAssignment -> ConfVar -> Either ConfVar Bool
archEnv _ (OS Linux) = Right True
archEnv _ (OS _) = Right False
archEnv _ (Arch X86_64) = Right True
archEnv _ (Arch _) = Right False
archEnv _ (Impl GHC range) = Right $ withinRange (mkVersion [8, 10, 2]) range
archEnv _ (Impl _ _) = Right False
archEnv assignment f@(Flag f') = go f $ lookupFlagAssignment f' assignment
  where
    go _ (Just r) = Right r
    go x Nothing = Left x

evalConditionTree :: (Semigroup k, Monad m) => CondTree ConfVar [Dependency] k -> HsM m ([Dependency], k)
evalConditionTree cond = do
  flg <- view flags
  return $ simplifyCondTree (archEnv flg) cond

getLatestCabal' :: DB.HackageDB -> PackageName -> GenericPackageDescription
getLatestCabal' db name = case Map.lookup name db of
  (Just m) -> case Map.lookupMax m of
    Just (_, vdata) -> vdata & DB.cabalFile
    Nothing -> throw VersionError
  Nothing -> throw $ PkgNotFound name

-----------------------------------------------------------------------------
groupDeps :: G.AdjacencyMap (S.Set DependencyType) PackageName -> [SolvedPackage]
groupDeps =
  fmap (\(name, deps) -> SolvedPackage name $ fmap (uncurry . flip $ SolvedDependency False) deps)
    . fmap ((\(a, b, c) -> (head b, zip a c)) . unzip3)
    . groupBy (\x y -> uncurry (==) ((x, y) & both %~ (^. _2)))
    . fmap (_1 %~ S.toList)
    . S.toList
    . G.edgeSet

prettySolvedPkgs :: [SolvedPackage] -> String
prettySolvedPkgs =
  mconcat
    . fmap
      ( \case
          SolvedPackage {..} -> "⊢ " ++ show _pkgName ++ "\n" ++ mconcat (fmap (\SolvedDependency {..} -> "    ⊢ " ++ show _depName ++ " " ++ show _depType ++ (if _provided then " ✔" else "") ++ "\n") _pkgDeps)
          ProvidedPackage {..} -> "⊢ " ++ show _pkgName ++ " ✔" ++ "\n"
      )

prettyDeps :: [PackageName] -> String
prettyDeps list =
  mconcat $
    fmap (\(i, n) -> show i ++ ". " ++ show n ++ "\n") $
      zip [1 ..] $
        reverse list

-----------------------------------------------------------------------------
ignoreList :: CollectedDependencies
ignoreList =
  fmap
    mkPackageName
    [ "unbuildable",
      "invalid-cabal-flag-settings",
      "par-classes",
      "fail",
      "integer-simple",
      "bytestring-builder",
      "nats",
      "old-time",
      "old-locale",
      "integer",
      "unsupported-ghc-version",
      "base",
      "ghc-prim",
      "ghcjs-prim",
      "ghc-bignum",
      "hans",
      "Win32"
    ]

ghcLibList :: CollectedDependencies
ghcLibList =
  fmap
    mkPackageName
    [ "array",
      "base",
      "binary",
      "bytestring",
      "Cabal",
      "containers",
      "deepseq",
      "directory",
      "exceptions",
      "filepath",
      "ghc-boot",
      "ghc-boot-th",
      "ghc-compact",
      "ghc-heap",
      "ghci",
      "ghc-prim",
      "haskeline",
      "hpc",
      "integer-gmp",
      "libiserv",
      "mtl",
      "parsec",
      "pretty",
      "process",
      "stm",
      "template-haskell",
      "terminfo",
      "text",
      "time",
      "transformers",
      "unix",
      "xhtml"
    ]

-----------------------------------------------------------------------------
-- const' _ b = b

-- const'
--     ( "At "
--         ++ show n
--         ++ ": Get dep for pkg ["
--         ++ show name
--         ++ "], with direct deps: "
--         ++ show (ignored $ libDeps <> testDeps <> exeDeps)
--         ++ ""
--     )
--     $

getDependencies ::
  (Monad m) =>
  S.Set PackageName ->
  Int ->
  Bool ->
  PackageName ->
  HsM m (G.AdjacencyMap (S.Set DependencyType) PackageName)
getDependencies resolved n buildExe name = do
  cabal <- getLatestCabal name
  let libDeps = uniq . delete name $ collectLibDeps cabal
      exeDeps = uniq . delete name $ collectExeDeps cabal
      testDeps = uniq . delete name $ collectTestDeps cabal
      benchDeps = uniq . delete name $ collectBenchmarkDeps cabal
      libbDeps = uniq . delete name $ collectLibBuildToolsDeps cabal
      testbDeps = uniq . delete name $ collectTestBuildToolsDeps cabal
      benchbDeps = uniq . delete name $ collectBenchmarkBuildToolsDeps cabal
      cats Lib = libDeps
      cats Exe = exeDeps
      cats Test = testDeps
      cats Main.Benchmark = benchDeps
      cats LibBuildTools = libbDeps
      cats TestBuildTools = testbDeps
      cats BenchmarkBuildTools = benchbDeps
      current x =
        G.edges
          ( zip3 (repeat $ S.singleton x) (repeat name) $
              filterNot (`elem` ignoreList) $
                cats x
          )
      next x = mapM (getDependencies (S.insert name (resolved)) (n + 1) buildExe) $ ignored (cats x)
      ignored = filter (\x -> not $ x `elem` ignoreList || x == name || x `elem` resolved)
      filterNot p = filter (not . p)
      uniq = map head . groupBy (==)

  nextLibs <- next Lib
  nextLibAndBuildTools <- next Lib
  nextExe <- next Exe
  return $
    if name `elem` ignoreList
      then G.empty
      else
        ( if buildExe
            then (current Exe) `G.overlay` G.overlays (nextExe)
            else G.empty
        )
          `G.overlay` (current Lib)
          `G.overlay` (current Test)
          `G.overlay` (current LibBuildTools)
          `G.overlay` (current TestBuildTools)
          `G.overlay` G.overlays (nextLibs)
          `G.overlay` G.overlays (nextLibAndBuildTools)
  where

getLatestCabal :: (Monad m) => PackageName -> HsM m PackageDescription
getLatestCabal name = do
  db <- view hackage
  case Map.lookup name db of
    (Just m) -> case Map.lookupMax m of
      Just (_, vdata) -> return $ flattenPackageDescription $ vdata & DB.cabalFile
      Nothing -> throwError VersionError
    Nothing -> throwError $ PkgNotFound name

collectLibDeps :: PackageDescription -> CollectedDependencies
collectLibDeps cabal = case library cabal of
  Just lib -> libDeps lib
  Nothing -> []
  where
    info lib = libBuildInfo lib
    libDeps lib = fmap depPkgName $ targetBuildDepends $ info lib

collectLibBuildToolsDeps :: PackageDescription -> CollectedDependencies
collectLibBuildToolsDeps cabal = case library cabal of
  Just lib -> toolDeps lib
  Nothing -> []
  where
    info lib = libBuildInfo lib
    toolDeps lib = fmap unExe $ buildToolDepends $ info lib

collectExeDeps :: PackageDescription -> CollectedDependencies
collectExeDeps cabal = mconcat $ exeDeps <> toolDeps
  where
    info = fmap buildInfo $ executables cabal
    exeDeps = fmap (fmap depPkgName . targetBuildDepends) info
    toolDeps = fmap (fmap unExe . buildToolDepends) info

collectTestDeps :: PackageDescription -> CollectedDependencies
collectTestDeps cabal =
  mconcat . exeDeps $ tInfo
  where
    tInfo = fmap testBuildInfo $ testSuites cabal
    exeDeps = fmap $ fmap depPkgName . targetBuildDepends

collectTestBuildToolsDeps :: PackageDescription -> CollectedDependencies
collectTestBuildToolsDeps cabal =
  mconcat . toolDeps $ tInfo
  where
    tInfo = fmap testBuildInfo $ testSuites cabal
    toolDeps = fmap $ fmap unExe . buildToolDepends

collectBenchmarkDeps :: PackageDescription -> CollectedDependencies
collectBenchmarkDeps cabal =
  mconcat . exeDeps $ bInfo
  where
    bInfo = fmap benchmarkBuildInfo $ benchmarks cabal
    exeDeps = fmap $ fmap depPkgName . targetBuildDepends

collectBenchmarkBuildToolsDeps :: PackageDescription -> CollectedDependencies
collectBenchmarkBuildToolsDeps cabal =
  mconcat . toolDeps $ tInfo
  where
    tInfo = fmap benchmarkBuildInfo $ benchmarks cabal
    toolDeps = fmap $ fmap unExe . buildToolDepends

unExe :: ExeDependency -> PackageName
unExe (ExeDependency name _ _) = name

-----------------------------------------------------------------------------
defaultHackagePath :: FilePath
defaultHackagePath = "/home/berberman/.stack/pantry/hackage/00-index.tar"

defaultCommunityPath :: FilePath
defaultCommunityPath = "/var/lib/pacman/sync/community.db"

defaultHackageDB :: IO DB.HackageDB
defaultHackageDB = loadHackageDB defaultHackagePath

loadHackageDB :: FilePath -> IO DB.HackageDB
loadHackageDB path = do
  putStrLn $ "Hackage index: " ++ path
  DB.readTarball Nothing path

loadCommunity ::
  (MonadResource m, PrimMonad m, MonadThrow m) =>
  FilePath ->
  ConduitT i FilePath m ()
loadCommunity path = do
  liftIO . putStrLn $ "Community db: " ++ path
  sourceFileBS path .| Zlib.ungzip .| Tar.untarChunks .| Tar.withEntries action
  where
    action header =
      when (Tar.headerFileType header == Tar.FTNormal) $
        yield $ Tar.headerFilePath header

cookCommunity :: (Monad m) => ConduitT FilePath FilePath m ()
cookCommunity = mapC (go . (splitOn "-"))
  where
    go list = case length list of
      3 -> list !! 0
      s ->
        if list !! 0 == "haskell"
          then intercalate "-" . fst . splitAt (s - 3) . tail $ list
          else intercalate "-" . fst . splitAt (s - 2) $ list

isInCommunity :: (Monad m) => PackageName -> HsM m Bool
isInCommunity name = do
  db <- view community
  return $ case splitOn "-" . unPackageName $ name of
    ("haskell" : xs) -> intercalate "-" xs `elem` db
    _ -> (fmap toLower $ unPackageName name) `elem` db

-----------------------------------------------------------------------------

cabalToPkgBuild :: (Monad m) => SolvedPackage -> HsM m PkgBuild
cabalToPkgBuild pkg = do
  cabal <- getLatestCabal $ pkg ^. pkgName
  let _hkgName = pkg ^. pkgName & unPackageName
      _pkgName = fmap toLower _hkgName
      _pkgVer = intercalate "." . fmap show . versionNumbers . I.pkgVersion . package $ cabal
      _pkgDesc = synopsis cabal
      _license = show . license $ cabal
      _depends = pkg ^. pkgDeps ^.. each . filtered (\x -> notInGHCLib x && selectDepType [Lib, Exe] x) & depsToString
      _makeDepends = pkg ^. pkgDeps ^.. each . filtered (\x -> notInGHCLib x && selectDepType [LibBuildTools, Test, TestBuildTools] x) & depsToString
      depsToString deps = deps <&> (wrap . fixName . unPackageName . _depName) & intercalate " "
      wrap s = '\'' : s ++ "\'"
      fromJust (Just x) = return x
      fromJust _ = throwError . UrlError $ pkg ^. pkgName
      notInGHCLib x = not ((x ^. depName) `elem` ghcLibList)
      selectDepType list x = (x ^. depType) `isInfixOf` list
      fixName s = case splitOn "-" s of
        ("haskell" : _) -> fmap toLower s
        _ -> "haskell-" ++ fmap toLower s
  _url <- case homepage cabal of
    "" -> fromJust . repoLocation . head $ sourceRepos cabal
    x -> return x
  return PkgBuild {..}