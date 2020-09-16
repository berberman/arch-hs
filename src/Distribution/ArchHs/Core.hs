{-# LANGUAGE RecordWildCards #-}

-- | Copyright: (c) 2020 berberman
-- SPDX-License-Identifier: MIT
-- Maintainer: berberman <1793913507@qq.com>
-- The core functions of @arch-hs@.
module Distribution.ArchHs.Core
  ( getDependencies,
    cabalToPkgBuild,
    evalConditionTree,
  )
where

import qualified Algebra.Graph.Labelled.AdjacencyMap as G
import Data.List (intercalate, stripPrefix)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Distribution.ArchHs.Hackage
import Distribution.ArchHs.Local
import Distribution.ArchHs.PkgBuild
import Distribution.ArchHs.Types
import Distribution.ArchHs.Utils
import Distribution.Compiler (CompilerFlavor (..))
import Distribution.PackageDescription
import Distribution.Pretty (prettyShow)
import Distribution.SPDX
import Distribution.System (Arch (X86_64), OS (Windows))
import qualified Distribution.Types.BuildInfo.Lens as L
import Distribution.Types.CondTree (simplifyCondTree)
import Distribution.Types.Dependency (Dependency)
import Distribution.Types.PackageName (mkPackageName, PackageName, unPackageName)
import Distribution.Types.UnqualComponentName (UnqualComponentName)
import Distribution.Types.Version (mkVersion)
import Distribution.Types.VersionRange
import Distribution.Utils.ShortText (fromShortText)

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

-- | Simplify the condition tree from 'GenericPackageDescription' with given flag assignments and archlinux system assumption.
evalConditionTree ::
  (Semigroup k, L.HasBuildInfo k, Member FlagAssignmentEnv r) =>
  PackageName ->
  CondTree ConfVar [Dependency] k ->
  Sem r BuildInfo
evalConditionTree name cond = do
  flg <- ask
  let thisFlag = case Map.lookup name flg of
        Just f -> f
        Nothing -> mkFlagAssignment []
  return $ (^. L.buildInfo) . snd $ simplifyCondTree (archEnv thisFlag) cond

-----------------------------------------------------------------------------

-- | Get dependencies of a package recursively.
-- All version constraints will be discarded,
-- and only packages depended by executables, libraries, and test suits will be collected.
getDependencies ::
  Members [HackageEnv, FlagAssignmentEnv, WithMyErr, DependencyRecord] r =>
  -- | Resolved
  Set PackageName ->
  -- | Skipped
  [UnqualComponentName] ->
  -- | Whether recursive
  Bool ->
  -- | Target
  PackageName ->
  Sem r (G.AdjacencyMap (Set DependencyType) PackageName)
getDependencies resolved skip recursive name = do
  cabal <- getLatestCabal name
  -- Ignore subLibraries
  (libDeps, libToolsDeps) <- collectLibDeps cabal
  (exeDeps, exeToolsDeps) <- collectExeDeps cabal skip
  (testDeps, testToolsDeps) <- collectTestDeps cabal skip
  -- Ignore benchmarks
  -- (benchDeps, benchToolsDeps) <- collectBenchMarkDeps cabal skip
  let uname :: (UnqualComponentName -> DependencyType) -> ComponentPkgList -> [(DependencyType, PkgList)]
      uname cons list = zip (fmap (cons . fst) list) (fmap snd list)

      flatten :: [(DependencyType, PkgList)] -> [(DependencyType, PackageName)]
      flatten list = mconcat $ fmap (\(t, pkgs) -> zip (repeat t) pkgs) list

      withThisName :: [(DependencyType, PackageName)] -> [(DependencyType, PackageName, PackageName)]
      withThisName = fmap (\(t, pkg) -> (t, name, pkg))

      ignored = filter (\x -> not $ x `elem` ignoreList || x == name || x `elem` resolved)
      filterNot p = filter (not . p)

      currentLib = G.edges $ zip3 (repeat $ Set.singleton CLib) (repeat name) $ filterNot (`elem` ignoreList) libDeps
      currentLibDeps = G.edges $ zip3 (repeat $ Set.singleton CLibBuildTools) (repeat name) $ filterNot (`elem` ignoreList) libToolsDeps

      runnableEdges k l = G.edges $ fmap (\(x, y, z) -> (Set.singleton x, y, z)) . withThisName . filterNot (\(_, x) -> x `elem` ignoreList) . flatten . uname k $ l

      currentExe = runnableEdges CExe exeDeps
      currentExeTools = runnableEdges CExeBuildTools exeToolsDeps
      currentTest = runnableEdges CTest testDeps
      currentTestTools = runnableEdges CTestBuildTools testToolsDeps

      -- currentBench = runnableEdges Types.Benchmark benchDeps
      -- currentBenchTools = runnableEdges BenchmarkBuildTools benchToolsDeps

      (<+>) = G.overlay
  -- Only solve lib & exe deps recursively.
  nextLib <- mapM (getDependencies (Set.insert name resolved) skip recursive) $ ignored libDeps
  nextExe <- mapM (getDependencies (Set.insert name resolved) skip recursive) $ ignored . fmap snd . flatten . uname CExe $ exeDeps
  return $
    currentLib
      <+> currentLibDeps
      <+> currentExe
      <+> currentExeTools
      <+> currentTest
      <+> currentTestTools
      -- <+> currentBench
      -- <+> currentBenchTools
      <+> if recursive
        then (G.overlays nextLib) <+> (G.overlays nextExe)
        else G.empty

collectLibDeps :: Members [HackageEnv, FlagAssignmentEnv, DependencyRecord] r => GenericPackageDescription -> Sem r (PkgList, PkgList)
collectLibDeps cabal = do
  case cabal & condLibrary of
    Just lib -> do
      let name = getPkgName' cabal
      info <- evalConditionTree name lib
      let libDeps = fmap unDepV $ targetBuildDepends info
          toolDeps = fmap unExeV $ buildToolDepends info
      updateDependencyRecord name libDeps
      updateDependencyRecord name toolDeps
      return (fmap fst libDeps, fmap fst toolDeps)
    Nothing -> return ([], [])

collectRunnableDeps ::
  (Semigroup k, L.HasBuildInfo k, Members [HackageEnv, FlagAssignmentEnv, DependencyRecord] r) =>
  (GenericPackageDescription -> [(UnqualComponentName, CondTree ConfVar [Dependency] k)]) ->
  GenericPackageDescription ->
  [UnqualComponentName] ->
  Sem r (ComponentPkgList, ComponentPkgList)
collectRunnableDeps f cabal skip = do
  let exes = cabal & f
      name = getPkgName' cabal
  info <- filter (not . (`elem` skip) . fst) . zip (exes <&> fst) <$> mapM (evalConditionTree name . snd) exes
  let runnableDeps = info <&> ((_2 %~) $ fmap unDepV . targetBuildDepends)
      toolDeps = info <&> ((_2 %~) $ fmap unExeV . buildToolDepends)
      k = fmap (\(c, l) -> (c, fmap fst l))
  mapM_ (updateDependencyRecord name) $ fmap snd runnableDeps
  mapM_ (updateDependencyRecord name) $ fmap snd toolDeps
  return (k runnableDeps, k toolDeps)

collectExeDeps :: Members [HackageEnv, FlagAssignmentEnv, DependencyRecord] r => GenericPackageDescription -> [UnqualComponentName] -> Sem r (ComponentPkgList, ComponentPkgList)
collectExeDeps = collectRunnableDeps condExecutables

collectTestDeps :: Members [HackageEnv, FlagAssignmentEnv, DependencyRecord] r => GenericPackageDescription -> [UnqualComponentName] -> Sem r (ComponentPkgList, ComponentPkgList)
collectTestDeps = collectRunnableDeps condTestSuites

updateDependencyRecord :: Member DependencyRecord r => PackageName -> [(PackageName, VersionRange)] -> Sem r ()
updateDependencyRecord parent deps = modify' $ Map.insertWith (<>)  parent deps

-- collectBenchMarkDeps :: Members [HackageEnv, FlagAssignmentEnv] r => GenericPackageDescription -> [UnqualComponentName] -> Sem r (ComponentPkgList, ComponentPkgList)
-- collectBenchMarkDeps = collectRunnableDeps condBenchmarks

-----------------------------------------------------------------------------

-- | Generate 'PkgBuild' for a 'SolvedPackage'.
cabalToPkgBuild :: Members [HackageEnv, FlagAssignmentEnv, WithMyErr] r => SolvedPackage -> Sem r PkgBuild
cabalToPkgBuild pkg = do
  let name = pkg ^. pkgName
  cabal <- packageDescription <$> (getLatestCabal name)
  let _hkgName = pkg ^. pkgName & unPackageName
      rawName = toLower' _hkgName
      _pkgName = maybe rawName id $ stripPrefix "haskell-" rawName
      _pkgVer = prettyShow $ getPkgVersion cabal
      _pkgDesc = fromShortText $ synopsis cabal
      getL (NONE) = ""
      getL (License e) = getE e
      getE (ELicense (ELicenseId x) _) = show . mapLicense $ x
      getE (ELicense (ELicenseIdPlus x) _) = show . mapLicense $ x
      getE (ELicense (ELicenseRef x) _) = "custom:" <> licenseRef x
      getE (EAnd x y) = getE x <> " " <> getE y
      getE (EOr x y) = getE x <> " " <> getE y

      _license = getL . license $ cabal
      _enableCheck = any id $ pkg ^. pkgDeps & mapped %~ (\dep -> selectDepKind Test dep && dep ^. depName == pkg ^. pkgName)
      depends = pkg ^. pkgDeps ^.. each . filtered (\x -> notMyself x && notInGHCLib x && (selectDepKind Lib x || selectDepKind Exe x))
      makeDepends =
        pkg ^. pkgDeps
          ^.. each
            . filtered
              ( \x ->
                  x `notElem` depends
                    && notMyself x
                    && notInGHCLib x
                    && ( selectDepKind LibBuildTools x
                           || selectDepKind Test x
                           || selectDepKind TestBuildTools x
                       )
              )
      depsToString deps = deps <&> (wrap . fixName . unPackageName . _depName) & intercalate " "
      _depends = depsToString depends
      _makeDepends = depsToString makeDepends
      _url = getUrl cabal
      wrap s = '\'' : s <> "\'"
      notInGHCLib x = (x ^. depName) `notElem` ghcLibList
      notMyself x = x ^. depName /= name
      selectDepKind k x = k `elem` (x ^. depType & mapped %~ dependencyTypeToKind)
  return PkgBuild {..}