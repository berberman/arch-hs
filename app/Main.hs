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
import qualified Distribution.Compat.Lens as L
import Distribution.Compiler
import qualified Distribution.Hackage.DB as DB
import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration
import Distribution.SPDX
import Distribution.System
import qualified Distribution.Types.BuildInfo.Lens as L
import Distribution.Types.CondTree
import Distribution.Types.Dependency
import Distribution.Types.ExeDependency
import Distribution.Types.GenericPackageDescription
import qualified Distribution.Types.PackageId as I
import Distribution.Types.PackageName
import Distribution.Types.UnqualComponentName
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
  | LicenseError PackageName
  deriving stock (Show, Eq)

-----------------------------------------------------------------------------

data DependencyType
  = Exe UnqualComponentName
  | ExeBuildTools UnqualComponentName
  | Lib
  | Test UnqualComponentName
  | Benchmark UnqualComponentName
  | LibBuildTools
  | TestBuildTools UnqualComponentName
  | BenchmarkBuildTools UnqualComponentName
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

isExe :: DependencyType -> Bool
isExe (Exe _) = True
isExe _ = False

isExeBuildTools :: DependencyType -> Bool
isExeBuildTools (ExeBuildTools _) = True
isExeBuildTools _ = False

isLib :: DependencyType -> Bool
isLib Lib = True
isLib _ = False

isTest :: DependencyType -> Bool
isTest (Test _) = True
isTest _ = False

isBenchmark :: DependencyType -> Bool
isBenchmark (Main.Benchmark _) = True
isBenchmark _ = False

isLibBuildTools :: DependencyType -> Bool
isLibBuildTools LibBuildTools = True
isLibBuildTools _ = False

isTestBuildTools :: DependencyType -> Bool
isTestBuildTools (TestBuildTools _) = True
isTestBuildTools _ = False

isBenchmarkBuildTools :: DependencyType -> Bool
isBenchmarkBuildTools (BenchmarkBuildTools _) = True
isBenchmarkBuildTools _ = False

-----------------------------------------------------------------------------

type PkgList = [PackageName]

type ComponentPkgList = [(UnqualComponentName, PkgList)]

type CommunityDB = S.Set String

data SolvedDependency = SolvedDependency {_provided :: Bool, _depName :: PackageName, _depType :: [DependencyType]}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

data SolvedPackage = ProvidedPackage {_pkgName :: PackageName} | SolvedPackage {_pkgName :: PackageName, _pkgDeps :: [SolvedDependency]}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

data HsEnv = HsEnv {_hackage :: DB.HackageDB, _community :: CommunityDB, _flags :: Map.Map PackageName FlagAssignment}

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
  let _flags = Map.empty
  return HsEnv {..}

h :: MonadIO m => HsEnv -> m (Either MyException ())
h env = runHsM env $ do
  name <- liftIO $ head <$> getArgs
  let target = mkPackageName name
  deps <- getDependencies S.empty 0 target
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
  mapM (\solved -> (liftIO . writeFile ("/home/berberman/Desktop/test/" <> (solved ^. pkgName & unPackageName) <> ".PKGBUILD") . applyTemplate) =<< cabalToPkgBuild solved) s
  return ()

main = do
  env <- getHsEnv
  h env >>= print

-----------------------------------------------------------------------------
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

archEnv :: FlagAssignment -> ConfVar -> Either ConfVar Bool
archEnv _ (OS Windows) = Right True
archEnv _ (OS _) = Right False
archEnv _ (Arch X86_64) = Right True
archEnv _ (Arch _) = Right False
archEnv _ (Impl GHC range) = Right $ withinRange (mkVersion [8, 10, 2]) range
archEnv _ (Impl _ _) = Right False
archEnv assignment f@(Flag f') = go f $ lookupFlagAssignment f' assignment
  where
    go _ (Just r) = Right r
    go x Nothing = Left x

evalConditionTree :: (Semigroup k, L.HasBuildInfo k, Monad m) => PackageName-> CondTree ConfVar [Dependency] k -> HsM m BuildInfo
evalConditionTree name cond = do
  flg <- view flags
  let thisFlag = case Map.lookup name flg of
        Just f -> f
        Nothing -> mkFlagAssignment []
  return $ (L.^. L.buildInfo) . snd $ simplifyCondTree (archEnv thisFlag) cond

getPackageFlag :: (Monad m) => PackageName -> HsM m [Flag]
getPackageFlag name = do
  cabal <- getLatestCabal name
  return $ cabal & genPackageFlags

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
ignoreList :: PkgList
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
      "ghc",
      "ghc-prim",
      "ghcjs-prim",
      "ghc-bignum",
      "hans",
      "Win32"
    ]

ghcLibList :: PkgList
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
  PackageName ->
  HsM m (G.AdjacencyMap (S.Set DependencyType) PackageName)
getDependencies resolved n name = do
  cabal <- getLatestCabal name
  (libDeps, libToolsDeps) <- collectLibDeps cabal
  (exeDeps, exeToolsDeps) <- collectExeDeps cabal
  (testDeps, testToolsDeps) <- collectTestDeps cabal
  (benchDeps, benchToolsDeps) <- collectBenchMarkDeps cabal
  let uname :: (UnqualComponentName -> DependencyType) -> ComponentPkgList -> [(DependencyType, PkgList)]
      uname cons list = zip (fmap (cons . fst) list) (fmap snd list)

      flatten :: [(DependencyType, PkgList)] -> [(DependencyType, PackageName)]
      flatten list = mconcat $ fmap (\(t, pkgs) -> zip (repeat t) pkgs) list

      withThisName :: [(DependencyType, PackageName)] -> [(DependencyType, PackageName, PackageName)]
      withThisName = fmap (\(t, pkg) -> (t, name, pkg))

      ignored = filter (\x -> not $ x `elem` ignoreList || x == name || x `elem` resolved)
      filterNot p = filter (not . p)

      currentLib = G.edges $ zip3 (repeat $ S.singleton Lib) (repeat name) $ filterNot (`elem` ignoreList) libDeps
      currentLibDeps = G.edges $ zip3 (repeat $ S.singleton LibBuildTools) (repeat name) $ filterNot (`elem` ignoreList) libToolsDeps

      runnableEdges k l = G.edges $ fmap (\(x, y, z) -> (S.singleton x, y, z)) . withThisName . filterNot (\(_, x) -> x `elem` ignoreList) . flatten . uname k $ l

      currentExe = runnableEdges Exe exeDeps
      currentExeTools = runnableEdges ExeBuildTools exeToolsDeps
      currentTest = runnableEdges Test testDeps
      currentTestTools = runnableEdges TestBuildTools testToolsDeps
      currentBench = runnableEdges Main.Benchmark benchDeps
      currentBenchTools = runnableEdges BenchmarkBuildTools benchToolsDeps

      (<+>) = G.overlay
  -- Only solve lib & exe deps recursively.
  nextLib <- mapM (getDependencies (S.insert name (resolved)) (n + 1)) $ ignored (libDeps)
  nextExe <- mapM (getDependencies (S.insert name (resolved)) (n + 1)) $ ignored . fmap snd . flatten . uname Exe $ exeDeps
  return $
    currentLib
      <+> currentLibDeps
      <+> currentExe
      <+> currentExeTools
      <+> currentTest
      <+> currentTestTools
      <+> currentBench
      <+> currentBenchTools
      <+> (G.overlays nextLib)
      <+> (G.overlays nextExe)

getLatestCabal :: (Monad m) => PackageName -> HsM m GenericPackageDescription
getLatestCabal name = do
  db <- view hackage
  case Map.lookup name db of
    (Just m) -> case Map.lookupMax m of
      Just (_, vdata) -> return $ vdata & DB.cabalFile
      Nothing -> throwError VersionError
    Nothing -> throwError $ PkgNotFound name

getPkgName :: GenericPackageDescription -> PackageName
getPkgName = I.pkgName.package.packageDescription

collectLibDeps :: (Monad m) => GenericPackageDescription -> HsM m (PkgList, PkgList)
collectLibDeps cabal = do
  case cabal & condLibrary of
    Just lib -> do
      info <- evalConditionTree (getPkgName cabal) lib
      let libDeps = fmap depPkgName $ targetBuildDepends info
          toolDeps = fmap unExe $ buildToolDepends info
      return (libDeps, toolDeps)
    Nothing -> return ([], [])

collectRunnableDeps ::
  (Monad m, Semigroup k, L.HasBuildInfo k) =>
  (GenericPackageDescription -> [(UnqualComponentName, CondTree ConfVar [Dependency] k)]) ->
  GenericPackageDescription ->
  HsM m (ComponentPkgList, ComponentPkgList)
collectRunnableDeps f cabal = do
  let exes = cabal & f
  info <- zip (fmap fst exes) <$> mapM (evalConditionTree (getPkgName cabal). snd) exes
  let runnableDeps = fmap (mapSnd $ fmap depPkgName . targetBuildDepends) info
      toolDeps = fmap (mapSnd $ fmap unExe . buildToolDepends) info
  return (runnableDeps, toolDeps)

collectExeDeps :: (Monad m) => GenericPackageDescription -> HsM m (ComponentPkgList, ComponentPkgList)
collectExeDeps = collectRunnableDeps condExecutables

collectTestDeps :: (Monad m) => GenericPackageDescription -> HsM m (ComponentPkgList, ComponentPkgList)
collectTestDeps = collectRunnableDeps condTestSuites

collectBenchMarkDeps :: (Monad m) => GenericPackageDescription -> HsM m (ComponentPkgList, ComponentPkgList)
collectBenchMarkDeps = collectRunnableDeps condBenchmarks

unExe :: ExeDependency -> PackageName
unExe (ExeDependency name _ _) = name

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (a, b) = (a, f b)

-----------------------------------------------------------------------------
defaultHackagePath :: FilePath
defaultHackagePath = "/home/berberman/.cabal/packages/mirrors.tuna.tsinghua.edu.cn/00-index.tar"

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
  let name = pkg ^. pkgName
  cabal <- packageDescription <$> (getLatestCabal name )
  let 
      _hkgName = pkg ^. pkgName & unPackageName
      _pkgName = fmap toLower _hkgName
      _pkgVer = intercalate "." . fmap show . versionNumbers . I.pkgVersion . package $ cabal
      _pkgDesc = synopsis cabal
      (License (ELicense (ELicenseId cabalLicense) _)) = license cabal --  TODO unexhausted
      _license = show . mapLicense $ cabalLicense
      _depends = pkg ^. pkgDeps ^.. each . filtered (\x -> notMyself x && notInGHCLib x && (selectDepType isLib x || selectDepType isExe x)) & depsToString
      _makeDepends = pkg ^. pkgDeps ^.. each . filtered (\x -> notMyself x && notInGHCLib x && (selectDepType isLibBuildTools x || selectDepType isTest x || selectDepType isTestBuildTools x)) & depsToString
      depsToString deps = deps <&> (wrap . fixName . unPackageName . _depName) & intercalate " "
      wrap s = '\'' : s ++ "\'"
      fromJust (Just x) = return x
      fromJust _ = throwError $ UrlError name
      head' (x:_) = return x
      head' [] = throwError $ UrlError name
      notInGHCLib x = not ((x ^. depName) `elem` ghcLibList)
      notMyself x = x ^. depName /= name
      selectDepType f x = any f (x ^. depType)
      fixName s = case splitOn "-" s of
        ("haskell" : _) -> fmap toLower s
        _ -> "haskell-" ++ fmap toLower s
  _url <- case homepage cabal of
    "" -> fromJust . repoLocation <=< head' $ sourceRepos cabal
    x -> return x
  return PkgBuild {..}