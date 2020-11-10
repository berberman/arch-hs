{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

-- | Copyright: (c) 2020 berberman
-- SPDX-License-Identifier: MIT
-- Maintainer: berberman <1793913507@qq.com>
-- Stability: experimental
-- Portability: portable
-- The core functions of @arch-hs@.
module Distribution.ArchHs.Core
  ( getDependencies,
    cabalToPkgBuild,
    evalConditionTree,
  )
where

import qualified Algebra.Graph.Labelled.AdjacencyMap as G
import Data.Bifunctor (second)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Distribution.ArchHs.Exception
import Distribution.ArchHs.Hackage
  ( getLatestCabal,
    getLatestSHA256,
  )
import Distribution.ArchHs.Internal.Prelude
import Distribution.ArchHs.Local (ignoreList)
import Distribution.ArchHs.Name
import Distribution.ArchHs.PkgBuild
  ( PkgBuild (..),
    mapLicense,
  )
import Distribution.ArchHs.Types
import Distribution.ArchHs.Utils
import Distribution.Compiler (CompilerFlavor (..))
import Distribution.PackageDescription
import Distribution.SPDX
import Distribution.System
  ( Arch (X86_64),
    OS (Linux),
  )
import qualified Distribution.Types.BuildInfo.Lens as L
import Distribution.Types.CondTree (simplifyCondTree)
import Distribution.Types.Dependency (Dependency)
import Distribution.Utils.ShortText (fromShortText)

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

-- | Simplify the condition tree from 'GenericPackageDescription' with given flag assignments and archlinux system assumption.
evalConditionTree ::
  (HasCallStack, Semigroup k, L.HasBuildInfo k, Members [FlagAssignmentsEnv, Trace] r) =>
  GenericPackageDescription ->
  CondTree ConfVar [Dependency] k ->
  Sem r BuildInfo
evalConditionTree cabal cond = do
  flagAssignments <- ask
  let name = getPkgName' cabal
      packageFlags = genPackageFlags cabal
      defaultFlagAssignments =
        foldr (\f acc -> insertFlagAssignment (flagName f) (flagDefault f) acc) (mkFlagAssignment []) packageFlags
      flagAssignment = case Map.lookup name flagAssignments of
        Just f -> unFlagAssignment f
        _ -> []
      flagNames = fmap fst flagAssignment
      thisFlag =
        mkFlagAssignment
          . (<> flagAssignment)
          . filter (\(fName, _) -> fName `notElem` flagNames)
          $ unFlagAssignment defaultFlagAssignments
  trace' $ "Evaluating condition tree of " <> show name
  trace' $ "Flags: " <> show thisFlag
  traceCallStack
  return $ (^. L.buildInfo) . snd $ simplifyCondTree (archEnv thisFlag) cond

-----------------------------------------------------------------------------

-- | Get dependencies of a package recursively.
-- All version constraints will be discarded,
-- and only packages depended by executables, libraries, and test suits will be collected.
getDependencies ::
  (HasCallStack, Members [HackageEnv, FlagAssignmentsEnv, WithMyErr, DependencyRecord, State (Set PackageName), Trace] r) =>
  -- | Skipped
  [UnqualComponentName] ->
  -- | Parent
  Maybe PackageName ->
  -- | Target
  PackageName ->
  Sem r (G.AdjacencyMap (Set DependencyType) PackageName, Set PackageName)
getDependencies skip parent name = do
  resolved <- get @(Set PackageName)
  modify' $ Set.insert name
  trace' $ "Getting all dependencies of (" <> show name <> "), parent: (" <> show parent <> ")"
  trace' $ "Already resolved: " <> show resolved
  traceCallStack
  cabal <- getLatestCabal name
  (libDeps, libToolsDeps) <- collectLibDeps cabal
  (subLibDeps, subLibToolsDeps) <- collectSubLibDeps cabal skip
  (exeDeps, exeToolsDeps) <- collectExeDeps cabal skip
  (testDeps, testToolsDeps) <- collectTestDeps cabal skip
  -- Ignore benchmarks
  -- (benchDeps, benchToolsDeps) <- collectBenchMarkDeps cabal skip
  let uname :: (UnqualComponentName -> DependencyType) -> ComponentPkgList -> [(DependencyType, PkgList)]
      uname cons list = zip (fmap (cons . fst) list) (fmap snd list)

      flatten :: [(DependencyType, PkgList)] -> [(DependencyType, PackageName)]
      flatten = mconcat . fmap (\(t, pkgs) -> zip (repeat t) pkgs)

      withThisName :: [(DependencyType, PackageName)] -> [(DependencyType, PackageName, PackageName)]
      withThisName = fmap (\(t, pkg) -> (t, name, pkg))

      ignoreSingle x = x `notElem` ignoreList
      ignore = filter ignoreSingle
      ignoreFlatten k = filter (\(_, x) -> ignoreSingle x) . flatten . uname k

      filteredLibDeps = ignore libDeps
      filteredLibToolsDeps = ignore libToolsDeps
      filteredExeDeps = ignoreFlatten CExe exeDeps
      filteredExeToolsDeps = ignoreFlatten CExeBuildTools exeToolsDeps
      filteredTestDeps = ignoreFlatten CTest testDeps
      filteredTestToolsDeps = ignoreFlatten CTest testToolsDeps
      filteredSubLibDeps = ignoreFlatten CSubLibs subLibDeps
      filteredSubLibToolsDeps = ignoreFlatten CSubLibsBuildTools subLibToolsDeps

      filteredSubLibDepsNames = fmap (unqualComponentNameToPackageName . fst) subLibDeps
      ignoreSubLibs = filter (`notElem` filteredSubLibDepsNames)
      ignoreResolved = filter (`notElem` resolved)

      currentLib = G.edges $ zip3 (repeat $ Set.singleton CLib) (repeat name) filteredLibDeps
      currentLibDeps = G.edges $ zip3 (repeat $ Set.singleton CLibBuildTools) (repeat name) filteredLibToolsDeps

      componentialEdges =
        G.edges
          . fmap (\(x, y, z) -> (Set.singleton x, y, z))
          . withThisName

      currentSubLibs = componentialEdges filteredSubLibDeps
      currentSubLibsTools = componentialEdges filteredSubLibToolsDeps
      currentExe = componentialEdges filteredExeDeps
      currentExeTools = componentialEdges filteredExeToolsDeps
      currentTest = componentialEdges filteredTestDeps
      currentTestTools = componentialEdges filteredTestToolsDeps

      -- currentBench = componentialEdges Types.Benchmark benchDeps
      -- currentBenchTools = componentialEdges BenchmarkBuildTools benchToolsDeps
      processNext = mapM (getDependencies skip (Just name)) . ignoreResolved . ignoreSubLibs
      (<+>) = G.overlay
  nextLib <- processNext filteredLibDeps
  nextExe <- processNext $ fmap snd filteredExeDeps
  -- TODO: maybe unstable
  nextTest <- processNext $ fmap snd filteredTestDeps
  nextSubLibs <- mapM (getDependencies skip (Just name)) $ fmap snd filteredSubLibDeps
  let temp = [nextLib, nextExe, nextTest, nextSubLibs]
      nexts = G.overlays $ temp ^. each ^.. each . _1
      subsubs = temp ^. each ^.. each . _2 ^. each
  return
    ( currentLib
        <+> currentLibDeps
        <+> currentExe
        <+> currentExeTools
        <+> currentTest
        <+> currentTestTools
        <+> currentSubLibs
        <+> currentSubLibsTools
        -- <+> currentBench
        -- <+> currentBenchTools
        <+> nexts,
      Set.fromList filteredSubLibDepsNames <> subsubs
    )

collectLibDeps :: Members [FlagAssignmentsEnv, DependencyRecord, Trace] r => GenericPackageDescription -> Sem r (PkgList, PkgList)
collectLibDeps cabal = do
  case cabal & condLibrary of
    Just lib -> do
      let name = getPkgName' cabal
      trace' $ "Getting libs dependencies of " <> show name
      info <- evalConditionTree cabal lib
      let libDeps = unDepV <$> buildDependsIfBuild info
          toolDeps = unBuildTools $ buildToolsAndbuildToolDependsIfBuild info
      mapM_ (uncurry updateDependencyRecord) libDeps
      mapM_ (uncurry updateDependencyRecord) toolDeps
      let result = (fmap fst libDeps, fmap fst toolDeps)
      trace' $ "Found: " <> show result
      traceCallStack
      return result
    Nothing -> return ([], [])

collectComponentialDeps ::
  (HasCallStack, Semigroup k, L.HasBuildInfo k, Members [FlagAssignmentsEnv, DependencyRecord, Trace] r) =>
  String ->
  (GenericPackageDescription -> [(UnqualComponentName, CondTree ConfVar [Dependency] k)]) ->
  GenericPackageDescription ->
  [UnqualComponentName] ->
  Sem r (ComponentPkgList, ComponentPkgList)
collectComponentialDeps tag f cabal skip = do
  let conds = cabal & f
      name = getPkgName' cabal
  trace' $ "Getting " <> tag <> " dependencies of " <> show name
  info <- filter (not . (`elem` skip) . fst) . zip (conds <&> fst) <$> mapM (evalConditionTree cabal . snd) conds
  let deps = info <&> _2 %~ (fmap unDepV . buildDependsIfBuild)
      toolDeps = info <&> _2 %~ (unBuildTools . buildToolsAndbuildToolDependsIfBuild)
      k = fmap (second $ fmap fst)
  mapM_ (uncurry updateDependencyRecord) $ deps ^.. each . _2 ^. each
  mapM_ (uncurry updateDependencyRecord) $ toolDeps ^.. each . _2 ^. each
  let result = (k deps, k toolDeps)
  trace' $ "Found: " <> show result
  traceCallStack
  return result

collectExeDeps :: (HasCallStack, Members [FlagAssignmentsEnv, DependencyRecord, Trace] r) => GenericPackageDescription -> [UnqualComponentName] -> Sem r (ComponentPkgList, ComponentPkgList)
collectExeDeps = collectComponentialDeps "exe" condExecutables

collectTestDeps :: (HasCallStack, Members [FlagAssignmentsEnv, DependencyRecord, Trace] r) => GenericPackageDescription -> [UnqualComponentName] -> Sem r (ComponentPkgList, ComponentPkgList)
collectTestDeps = collectComponentialDeps "test" condTestSuites

collectSubLibDeps :: (HasCallStack, Members [FlagAssignmentsEnv, DependencyRecord, Trace] r) => GenericPackageDescription -> [UnqualComponentName] -> Sem r (ComponentPkgList, ComponentPkgList)
collectSubLibDeps = collectComponentialDeps "sublib" condSubLibraries

updateDependencyRecord :: Member DependencyRecord r => PackageName -> VersionRange -> Sem r ()
updateDependencyRecord name range = modify' $ Map.insertWith (<>) name [range]

-- collectBenchMarkDeps :: Members [HackageEnv, FlagAssignmentEnv] r => GenericPackageDescription -> [UnqualComponentName] -> Sem r (ComponentPkgList, ComponentPkgList)
-- collectBenchMarkDeps = collectComponentialDeps condBenchmarks

-----------------------------------------------------------------------------

-- | Generate 'PkgBuild' for a 'SolvedPackage'.
cabalToPkgBuild :: Members [HackageEnv, FlagAssignmentsEnv, WithMyErr] r => SolvedPackage -> Bool -> Sem r PkgBuild
cabalToPkgBuild pkg uusi = do
  let name = pkg ^. pkgName
  cabal <- packageDescription <$> getLatestCabal name
  _sha256sums <- (\case Just s -> "'" <> s <> "'"; Nothing -> "'SKIP'") <$> tryMaybe (getLatestSHA256 name)
  let _hkgName = pkg ^. pkgName & unPackageName
      _pkgName = unCommunityName . toCommunityName $ pkg ^. pkgName
      _pkgVer = prettyShow $ getPkgVersion cabal
      _pkgDesc = fromShortText $ synopsis cabal
      getL NONE = ""
      getL (License e) = getE e
      getE (ELicense (ELicenseId x) _) = show . mapLicense $ x
      getE (ELicense (ELicenseIdPlus x) _) = show . mapLicense $ x
      getE (ELicense (ELicenseRef x) _) = "custom:" <> licenseRef x
      getE (EAnd x y) = getE x <> " " <> getE y
      getE (EOr x y) = getE x <> " " <> getE y

      _license = getL . license $ cabal
      _enableCheck = or $ (pkg ^. pkgDeps) <&> depIsKind Test
      depends =
        pkg ^. pkgDeps
          ^.. each
            . filtered
              ( \x ->
                  depNotMyself name x
                    && depNotInGHCLib x
                    && ( depIsKind Lib x
                           || depIsKind Exe x
                           || depIsKind SubLibs x
                       )
              )
      makeDepends =
        pkg ^. pkgDeps
          ^.. each
            . filtered
              ( \x ->
                  x `notElem` depends
                    && depNotMyself name x
                    && depNotInGHCLib x
                    && ( depIsKind LibBuildTools x
                           || depIsKind ExeBuildTools x
                           || depIsKind Test x
                           || depIsKind TestBuildTools x
                           || depIsKind SubLibsBuildTools x
                       )
              )
      depsToString deps = deps <&> (wrap . unCommunityName . toCommunityName . _depName) & mconcat
      _depends = depsToString depends
      _makeDepends = (if uusi then " 'uusi'" else "") <> depsToString makeDepends
      _url = getUrl cabal
      wrap s = " '" <> s <> "'"
      _licenseFile = licenseFiles cabal ^? ix 0
      _enableUusi = uusi
  return PkgBuild {..}
