{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

-- | Copyright: (c) 2020-2021 berberman
-- SPDX-License-Identifier: MIT
-- Maintainer: berberman <berberman@yandex.com>
-- Stability: experimental
-- Portability: portable
-- The core functions of @arch-hs@.
module Distribution.ArchHs.Core
  ( getDependencies,
    cabalToPkgBuild,
    evalConditionTree,
    subsumeGHCVersion,

    -- * Helper functions
    collectLibDeps,
    collectExeDeps,
    collectTestDeps,
    collectSubLibDeps,
    collectSetupDeps,
  )
where

import qualified Algebra.Graph.Labelled.AdjacencyMap as G
import Conduit
import Control.Monad (filterM, unless)
import Data.Bifunctor (second)
import qualified Data.ByteString as BS
import qualified Data.Conduit.Tar as Tar
import Data.Containers.ListUtils (nubOrd)
import Data.IORef (modifyIORef', newIORef, readIORef)
import qualified Data.Map as Map
import Data.List (sort)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Distribution.ArchHs.ExtraDB (versionInExtra)
import Distribution.ArchHs.Exception
import Distribution.ArchHs.Hackage
  ( getLatestCabal,
    getLatestSHA256,
    getPackageFlag,
    lookupHackagePath,
  )
import Distribution.ArchHs.Internal.Prelude
import Distribution.ArchHs.Local (ignoreList)
import Distribution.ArchHs.Name
import Distribution.ArchHs.PkgBuild
  ( PkgBuild (..),
    mapLicense,
    showArchLicense,
  )
import Distribution.ArchHs.Types
import Distribution.ArchHs.Utils
import Distribution.Compiler (CompilerFlavor (..), AbiTag (..), buildCompilerId, unknownCompilerInfo)
import Distribution.PackageDescription hiding (pkgName)
import Distribution.PackageDescription.Configuration
import Distribution.PackageDescription.Parsec (parseGenericPackageDescriptionMaybe)
import Distribution.SPDX
import Distribution.System (Arch (X86_64), OS (Linux), Platform (..))
import qualified Distribution.Types.BuildInfo.Lens as L
import Distribution.Types.CondTree (simplifyCondTree)
import Distribution.Types.ComponentRequestedSpec (ComponentRequestedSpec (..))
import Distribution.Utils.ShortText (fromShortText)

archEnv :: Version -> FlagAssignment -> ConfVar -> Either ConfVar Bool
archEnv _ _ (OS Linux) = Right True
archEnv _ _ (OS _) = Right False
archEnv _ _ (Arch X86_64) = Right True
archEnv _ _ (Arch _) = Right False
archEnv ghcVersion _ (Impl GHC range) = Right $ withinRange ghcVersion range
archEnv _ _ (Impl _ _) = Right False
archEnv _ assignment f@(PkgFlag f') = go f $ lookupFlagAssignment f' assignment
  where
    go _ (Just r) = Right r
    go x Nothing = Left x

-- | Simplify the condition tree from 'GenericPackageDescription' with given flag assignments and archlinux system assumption.
evalConditionTree ::
  (HasCallStack, Semigroup k, L.HasBuildInfo k, Members [KnownGHCVersion, FlagAssignmentsEnv, Trace] r) =>
  GenericPackageDescription ->
  CondTree ConfVar [Dependency] k ->
  Sem r BuildInfo
evalConditionTree cabal cond = do
  flagAssignments <- ask
  let name = getPkgName' cabal
      packageFlags = genPackageFlags cabal
      flagAssignment = getFlagAssignment name flagAssignments
      thisFlag = defaultFlags packageFlags flagAssignment
  trace' $ "Evaluating condition tree of " <> show name
  trace' $ "Flags: " <> show thisFlag
  traceCallStack
  ghcVersion <- ask
  return $ (^. L.buildInfo) . snd $ simplifyCondTree (archEnv ghcVersion thisFlag) cond

-----------------------------------------------------------------------------

-- | Get dependencies of a package recursively.
-- All version constraints will be discarded,
-- and only packages depended by executables, libraries, and test suits will be collected.
getDependencies ::
  (HasCallStack, Members [KnownGHCVersion, HackageEnv, FlagAssignmentsEnv, WithMyErr, DependencyRecord, State (Set PackageName), Trace] r) =>
  -- | Skipped
  [UnqualComponentName] ->
  -- | Parent
  Maybe PackageName ->
  -- | Target
  PackageName ->
  Sem r (G.AdjacencyMap (Set DependencyType) PackageName, Set PackageName, Map.Map PackageName [SystemDependency])
getDependencies skip parent name = do
  resolved <- get @(Set PackageName)
  modify' $ Set.insert name
  trace' $ "Getting all dependencies of (" <> show name <> "), parent: (" <> show parent <> ")"
  trace' $ "Already resolved: " <> show resolved
  traceCallStack
  cabal <- getLatestCabal name
  let kIgnoreVersionLib = fmap fst
      kIgnoreVersionComp = fmap (second $ fmap fst)
      kIgnoreVersionSetup = kIgnoreVersionLib
  (libDeps, libToolsDeps, libSysDeps) <- collectLibDeps kIgnoreVersionLib cabal
  (subLibDeps, subLibToolsDeps, subLibSysDeps) <- collectSubLibDeps kIgnoreVersionComp cabal skip
  (exeDeps, exeToolsDeps, exeSysDeps) <- collectExeDeps kIgnoreVersionComp cabal skip
  (testDeps, testToolsDeps, testSysDeps) <- collectTestDeps kIgnoreVersionComp cabal skip
  setupDeps <- collectSetupDeps kIgnoreVersionSetup cabal
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
      filteredSetupDeps = ignore setupDeps

      filteredSubLibDepsNames = fmap (unqualComponentNameToPackageName . fst) subLibDeps
      ignoreSubLibs = filter (`notElem` filteredSubLibDepsNames)
      ignoreResolved = filter (`notElem` resolved)

      currentLib = G.edges $ zip3 (repeat $ Set.singleton CLib) (repeat name) filteredLibDeps
      currentLibToolDeps = G.edges $ zip3 (repeat $ Set.singleton CLibBuildTools) (repeat name) filteredLibToolsDeps
      currentSetupDeps = G.edges $ zip3 (repeat $ Set.singleton CSetup) (repeat name) filteredSetupDeps

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

      currentSysDeps = nubOrd $ libSysDeps <> subLibSysDeps <> exeSysDeps <> testSysDeps
      processNext = mapM (getDependencies skip (Just name)) . ignoreResolved . ignoreSubLibs
      (<+>) = G.overlay
  nextLib <- processNext filteredLibDeps
  nextSetup <- processNext filteredSetupDeps
  nextExe <- processNext $ fmap snd filteredExeDeps
  -- TODO: maybe unstable
  nextTest <- processNext $ fmap snd filteredTestDeps
  nextSubLibs <- processNext $ fmap snd filteredSubLibDeps
  let temp = [nextLib, nextSetup, nextExe, nextTest, nextSubLibs]
      nexts = G.overlays $ temp ^. each ^.. each . _1
      subsubs = temp ^. each ^.. each . _2 ^. each
      nextSys = temp ^. each ^.. each . _3 ^. each
  return
    ( currentLib
        <+> currentLibToolDeps
        <+> currentSetupDeps
        <+> currentExe
        <+> currentExeTools
        <+> currentTest
        <+> currentTestTools
        <+> currentSubLibs
        <+> currentSubLibsTools
        -- <+> currentBench
        -- <+> currentBenchTools
        <+> nexts,
      Set.fromList filteredSubLibDepsNames <> subsubs,
      (if null currentSysDeps then Map.empty else Map.singleton name currentSysDeps) <> nextSys
    )

collectLibDeps ::
  (Members [KnownGHCVersion, FlagAssignmentsEnv, DependencyRecord, Trace] r, Show a, Monoid a) =>
  ([(PackageName, VersionRange)] -> a) ->
  GenericPackageDescription ->
  Sem r (a, a, [SystemDependency])
collectLibDeps k cabal = do
  case cabal & condLibrary of
    Just lib -> do
      let name = getPkgName' cabal
      trace' $ "Getting libs dependencies of " <> show name
      info <- evalConditionTree cabal lib
      let libDeps = unDepV <$> buildDependsIfBuild info
          toolDeps = unBuildTools $ buildToolsAndbuildToolDependsIfBuild info
          systemDeps = unSystemDependency $ pkgconfigDependsAndExtraLibsIfBuild info
      mapM_ (uncurry updateDependencyRecord) libDeps
      mapM_ (uncurry updateDependencyRecord) toolDeps
      let result = (k libDeps, k toolDeps, systemDeps)
      trace' $ "Found: " <> show result
      traceCallStack
      return result
    Nothing -> return mempty -- 'Monoid a' comes from here

collectComponentialDeps ::
  (HasCallStack, Semigroup k, L.HasBuildInfo k, Members [KnownGHCVersion, FlagAssignmentsEnv, DependencyRecord, Trace] r, Show a) =>
  String ->
  (GenericPackageDescription -> [(UnqualComponentName, CondTree ConfVar [Dependency] k)]) ->
  ([(UnqualComponentName, [(PackageName, VersionRange)])] -> a) ->
  GenericPackageDescription ->
  [UnqualComponentName] ->
  Sem r (a, a, [SystemDependency])
collectComponentialDeps tag f k cabal skip = do
  let conds = cabal & f
      name = getPkgName' cabal
  trace' $ "Getting " <> tag <> " dependencies of " <> show name
  info <- filter (not . (`elem` skip) . fst) . zip (conds <&> fst) <$> mapM (evalConditionTree cabal . snd) conds
  let deps = info <&> _2 %~ (fmap unDepV . buildDependsIfBuild)
      toolDeps = info <&> _2 %~ (unBuildTools . buildToolsAndbuildToolDependsIfBuild)
      sysDeps = info <&> _2 %~ (unSystemDependency . pkgconfigDependsAndExtraLibsIfBuild)
  mapM_ (uncurry updateDependencyRecord) $ deps ^.. each . _2 ^. each
  mapM_ (uncurry updateDependencyRecord) $ toolDeps ^.. each . _2 ^. each
  let result = (k deps, k toolDeps, mconcat $ fmap snd sysDeps)
  trace' $ "Found: " <> show result
  traceCallStack
  return result

collectExeDeps ::
  (HasCallStack, Members [KnownGHCVersion, FlagAssignmentsEnv, DependencyRecord, Trace] r, Show a) =>
  ([(UnqualComponentName, [(PackageName, VersionRange)])] -> a) ->
  GenericPackageDescription ->
  [UnqualComponentName] ->
  Sem r (a, a, [SystemDependency])
collectExeDeps = collectComponentialDeps "exe" condExecutables

collectTestDeps ::
  (HasCallStack, Members [KnownGHCVersion, FlagAssignmentsEnv, DependencyRecord, Trace] r, Show a) =>
  ([(UnqualComponentName, [(PackageName, VersionRange)])] -> a) ->
  GenericPackageDescription ->
  [UnqualComponentName] ->
  Sem r (a, a, [SystemDependency])
collectTestDeps = collectComponentialDeps "test" condTestSuites

collectSubLibDeps ::
  (HasCallStack, Members [KnownGHCVersion, FlagAssignmentsEnv, DependencyRecord, Trace] r, Show a) =>
  ([(UnqualComponentName, [(PackageName, VersionRange)])] -> a) ->
  GenericPackageDescription ->
  [UnqualComponentName] ->
  Sem r (a, a, [SystemDependency])
collectSubLibDeps = collectComponentialDeps "sublib" condSubLibraries

collectSetupDeps ::
  (Member Trace r, Show a, Monoid a) =>
  ([(PackageName, VersionRange)] -> a) ->
  GenericPackageDescription ->
  Sem r a
collectSetupDeps k cabal = do
  let name = getPkgName' cabal
  trace' $ "Getting setup dependencies of " <> show name
  case setupBuildInfo $ packageDescription cabal of
    Just (SetupBuildInfo deps _) -> do
      let result = k $ unDepV <$> deps
      trace' $ "Found: " <> show result
      return result
    _ -> return mempty

updateDependencyRecord :: Member DependencyRecord r => PackageName -> VersionRange -> Sem r ()
updateDependencyRecord name range = modify' $ Map.insertWith (<>) name [range]

-- collectBenchMarkDeps :: Members [HackageEnv, FlagAssignmentEnv] r => GenericPackageDescription -> [UnqualComponentName] -> Sem r (ComponentPkgList, ComponentPkgList)
-- collectBenchMarkDeps = collectComponentialDeps condBenchmarks

-----------------------------------------------------------------------------

inRange :: Members [ExtraEnv, WithMyErr] r => (PackageName, VersionRange) -> Sem r (Either (PackageName, VersionRange) (PackageName, VersionRange, Version, Bool))
inRange (name, hRange) =
  try @MyException (versionInExtra name)
    >>= \case
      Right rawVersion ->
        case simpleParsec rawVersion of
          Just version -> return . Right $ (name, hRange, version, withinRange version hRange)
          Nothing -> throw $ VersionNoParse rawVersion
      Left _ -> return . Left $ (name, hRange)

isInRangeBool :: Members [ExtraEnv, WithMyErr] r => PackageName -> VersionRange -> Sem r Bool
isInRangeBool name hRange = do
  l <- inRange (name, hRange)
  case l of
    Right (_, _, _, i) -> return i
    Left _ -> return False

isNotInRangeBool :: Members [ExtraEnv, WithMyErr] r => PackageName -> VersionRange -> Sem r Bool
isNotInRangeBool name hRange = do
  l <- isInRangeBool name hRange
  return (not l)

getCabalsFromIndex :: Members [Embed IO, WithMyErr] r => FilePath -> PackageName -> [Version] -> Sem r (Map.Map Version GenericPackageDescription)
getCabalsFromIndex hackagePath name versions = do
  let pkg = unPackageName name
      cabalPaths = Map.fromList [(pkg </> prettyShow version </> (pkg <> ".cabal"), version) | version <- nub versions]
  cabalFiles <- embed $ findCabalFilesInIndex hackagePath cabalPaths
  Map.fromList <$> traverse (parseCabal cabalFiles) (nub versions)
  where
    parseCabal cabalFiles version =
      case parseGenericPackageDescriptionMaybe =<< Map.lookup version cabalFiles of
        Just cabal -> return (version, cabal)
        Nothing -> throw $ VersionNotFound name version

findCabalFilesInIndex :: FilePath -> Map.Map FilePath Version -> IO (Map.Map Version BS.ByteString)
findCabalFilesInIndex hackagePath cabalPaths = do
  foundRef <- newIORef Map.empty
  Map.fromList
    <$> runConduitRes
      ( sourceFileBS hackagePath
          .| Tar.untarChunks
          .| Tar.withEntries (action foundRef)
          .| takeC (Map.size cabalPaths)
          .| sinkList
      )
  where
    action foundRef header
      | Tar.FTNormal <- Tar.headerFileType header,
        Just version <- Map.lookup (Tar.headerFilePath header) cabalPaths = do
        found <- liftIO $ readIORef foundRef
        unless (Map.member version found) $ do
          cabalFile <- mconcat <$> sinkList
          liftIO $ modifyIORef' foundRef (Map.insert version ())
          yield (version, cabalFile)
      | otherwise = return ()

-- | Generate 'PkgBuild' for a 'SolvedPackage'.
cabalToPkgBuild :: Members [Embed IO, ExtraEnv, HackageEnv, FlagAssignmentsEnv, WithMyErr] r => SolvedPackage -> Bool -> [ArchLinuxName] -> Sem r PkgBuild
cabalToPkgBuild pkg uusi sysDeps = do
  let name = pkg ^. pkgName
  latestCabal <- getLatestCabal name
  let cabal = packageDescription latestCabal
  pkgFlags <- getPackageFlag name
  assignment <- getFlagAssignment name <$> ask
  let showFlagForCmd (unFlagName -> fName, enabled) = "-f" <> (if enabled then "" else "-") <> fName
      _flags = unwords . fmap showFlagForCmd . unFlagAssignment $ defaultFlags pkgFlags assignment
  _sha256sums <- (\case Just s -> "'" <> s <> "'"; Nothing -> "'SKIP'") <$> getLatestSHA256 name
  let _hkgName = pkg ^. pkgName & unPackageName
      _pkgName = unArchLinuxName . toArchLinuxName $ pkg ^. pkgName
      pkgVersion = getPkgVersion cabal
      _pkgVer = prettyShow $ pkgVersion
      _pkgDesc = fromShortText $ synopsis cabal
      getL NONE = ""
      getL (License e) = getE e
      getE (ELicense (ELicenseId x) _) = showArchLicense . mapLicense $ x
      getE (ELicense (ELicenseIdPlus x) _) = showArchLicense . mapLicense $ x
      getE (ELicense (ELicenseRef x) _) = "custom:" <> licenseRef x
      getE (EAnd x y) = getE x <> " " <> getE y
      getE (EOr x y) = getE x <> " " <> getE y

      _license = getL . license $ cabal
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
                           || depIsKind Setup x
                       )
              )
      depsToString k deps = (sort $ deps <&> (wrap . unArchLinuxName . toArchLinuxName . k)) & mconcat
      _depends = depsToString _depName depends <> depsToString id sysDeps
      _url = getUrl cabal
      wrap s = " '" <> s <> "'"
      _licenseFile = licenseFile cabal
      pd = (finalizePD assignment (ComponentRequestedSpec True False) (\_ -> True) (Platform X86_64 Linux) (unknownCompilerInfo buildCompilerId NoAbiTag) [] latestCabal)
      finalisedPackageDescription = (\case Right (x, _) -> x; Left _ -> error $ "Missing dependencies") pd
      versionRanges = Map.fromList $ enabledBuildDepends finalisedPackageDescription (ComponentRequestedSpec True False) <&> (\dep -> (depPkgName dep, depVerRange dep))
      -- If dep has been removed in revision, then it clearly isn't needed, so ignore the version bounds
      revisedIsInRange dep = (\case Just version -> isInRangeBool dep version; Nothing -> return True) (Map.lookup dep versionRanges)

  hackagePath <- embed $ lookupHackagePath
  cabals <- getCabalsFromIndex hackagePath name [pkgVersion]
  let lookupCabal cs version = (\case Just c -> return c; Nothing -> throw $ VersionNotFound name version) $ Map.lookup version cs
  unrevisedCabalFile <- lookupCabal cabals pkgVersion
  let unrevisedPd = (finalizePD assignment (ComponentRequestedSpec True False) (\_ -> True) (Platform X86_64 Linux) (unknownCompilerInfo buildCompilerId NoAbiTag) [] unrevisedCabalFile)
      finalisedUnrevisedPackageDescription = (\case Right (x, _) -> x; Left _ -> error $ "Missing dependencies") unrevisedPd
      unRevisedEnabledBuildDepends = enabledBuildDepends finalisedUnrevisedPackageDescription (ComponentRequestedSpec True False)
      unrevisedVersionRanges = Map.fromList $ unRevisedEnabledBuildDepends <&> (\dep -> (depPkgName dep, depVerRange dep))
      unrevisedIsNotInRange x = isNotInRangeBool x (fromMaybe (error $ "Internal error: Dependency '" <> prettyShow x <> "' is declared as depends or makedepends " <>
                                                                       "while not being a dependency of an enabled component in the unrevised .cabal.")
                                                              $ Map.lookup x unrevisedVersionRanges)
  outOfBounds <- filterM unrevisedIsNotInRange $ sort (unRevisedEnabledBuildDepends <&> depPkgName)
  revisedInBounds <- filterM revisedIsInRange $ outOfBounds
  let _removeWithUusi = map prettyShow revisedInBounds
      _enableUusi = uusi || (not (null _removeWithUusi))
      _makeDepends = (if _enableUusi then " 'uusi'" else "") <> depsToString _depName makeDepends
  -- _ <- error $ ((Map.assocs versionRanges) <&> (\x -> " " <> prettyShow (fst x) <> " " <> prettyShow (snd x)) & mconcat) <> "\nUnrevised version ranges: " <> ((Map.assocs unrevisedVersionRanges) <&> (\x -> " " <> prettyShow (fst x) <> " " <> prettyShow (snd x)) & mconcat) <> "\nTo remove with uusi: " <> (mconcat (map (\x -> x <> " ") (_removeWithUusi)))
  return PkgBuild {..}

-----------------------------------------------------------------------------

-- | Get the ghc version in 'ExtraDB'
subsumeGHCVersion :: Members [ExtraEnv, WithMyErr] r => InterpreterFor KnownGHCVersion r
subsumeGHCVersion m = do
  rawVersion <- versionInExtra $ ArchLinuxName "haskell-ghc"
  let ghcVersion = fromMaybe (error $ "Impossible: unable to parse ghc version from [extra]: " <> rawVersion) $ simpleParsec rawVersion
  runReader ghcVersion m
