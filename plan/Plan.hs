{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}

module Plan (PlanResult (..), PlanProblem (..), planUpdates, planIsReady, prettyPlanResult, comparePlanRevisions) where

import Control.Monad (foldM, forM, forM_, unless)
import Data.Foldable (toList)
import Data.List (foldl', partition, sortOn)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.Ord (Down (..))
import qualified Data.Set as Set
import Distribution.ArchHs.DepCheck (VersionedList, directDependencies)
import Distribution.ArchHs.Exception
import Distribution.ArchHs.ExtraDB (versionInExtra)
import Distribution.ArchHs.Hackage
import Distribution.ArchHs.Internal.Prelude
import Distribution.ArchHs.Local (ghcLibList)
import Distribution.ArchHs.Name (isGHCLibs, isHaskellPackage, toArchLinuxName, toHackageName)
import Distribution.ArchHs.PP
import Distribution.ArchHs.RDepCheck
import Distribution.ArchHs.Types
import Distribution.Compiler (CompilerFlavor (GHC))
import Distribution.Types.CondTree (CondTree, condTreeComponents, condBranchCondition, condBranchIfTrue, condBranchIfFalse)
import Distribution.Types.ConfVar (ConfVar (Impl))
import Distribution.Version (asVersionIntervals, simplifyVersionRange)
import Plan.Toolchain
import qualified Plan.Solver as Solver
import Plan.Trace (tracePlan)

type PlanEffects r =
  Members
    '[ExtraEnv, HackageEnv, RawHackageEnv, KnownGHCVersion, FlagAssignmentsEnv, Trace, DependencyRecord, WithMyErr, Embed IO]
    r

data PlanProblem
  = DependencyProblem PackageName PackageName VersionRange (Maybe Version)
  | ReverseDependencyProblem ArchLinuxName PackageName DepSrc VersionRange Version
  | UncheckedCandidate PackageName Version MyException
  | UncheckedReverseDependency ArchLinuxName [PackageName] MyException
  | UncheckedCompilerTool PackageName PackageName VersionRange
  | UnavailableDependency PackageName VersionRange (Maybe Version)

data PlanResult = PlanResult
  { planInstalled :: Map.Map PackageName Version,
    planVersions :: Map.Map PackageName Version,
    planRequested :: Set.Set PackageName,
    planProblems :: [PlanProblem],
    planWarnings :: [PlanProblem],
    plansTried :: Int,
    planToolchain :: Maybe Toolchain,
    planRevisionNotes :: [Doc AnsiStyle],
    planSearchNotes :: [Doc AnsiStyle]
  }

-- Exact requests are checked as given; solving can advance from each minimum.
planUpdates :: PlanEffects r => GHCReleases -> Bool -> [(PackageName, Maybe Version)] -> Sem r (Either String PlanResult)
planUpdates releases solve targets
  | null targets = pure $ Left "At least one target is required."
  | length names /= Set.size (Set.fromList names) = pure $ Left "Each target must be specified only once."
  | any (\name -> name /= "ghc" && isGHCLibs name) names = pure $ Left "GHC bundled libraries cannot be updated independently; request ghc to update the toolchain."
  | otherwise = do
      tracePlan "Selecting starting versions..."
      installed <- Map.fromList <$> forM names (\name -> (name,) <$> currentVersion name)
      choices <- forM targets $ \(name, requested) -> do
        newer <- if not solve && requested /= Nothing then pure [] else
          if name == "ghc"
            then pure [release | release <- Map.keys releases, stableGHCRelease release, release > installed Map.! name]
            else getNewerVersions name (installed Map.! name)
        pure $ case requested of
          Just version
            | version < installed Map.! name -> Left $ "Downgrades are not supported: " <> unPackageName name
            | name == "ghc", Map.notMember version releases -> Left $ "No upstream bundled-library metadata for GHC " <> prettyShow version
            | otherwise -> Right (name, version : [v | solve, v <- newer, v > version])
          Nothing -> case newer of
            [] -> Left $ if name == "ghc" then "No newer stable GHC release is available in upstream metadata." else "No newer preferred version is available for " <> unPackageName name
            first : rest -> Right (name, first : [v | solve, v <- rest])
      case sequence choices of
        Left err -> pure $ Left err
        Right options -> do
          initial <- selectToolchain releases (Map.fromList [(name, first) | (name, first : _) <- options]) emptyDependencies
          let bundled = Set.unions [Map.keysSet packages | release <- fromMaybe [] $ lookup "ghc" options, Just packages <- [Map.lookup release releases]]
          if any (\name -> name /= "ghc" && (Set.member name bundled || fixedPackage initial name)) names
            then pure $ Left "Bundled libraries and compiler tools cannot be updated independently of the requested GHC releases."
            else Right <$> search releases solve installed (Map.fromList options)
  where
    names = fst <$> targets

currentVersion :: Members '[ExtraEnv, WithMyErr] r => PackageName -> Sem r Version
currentVersion name = do
  raw <- versionInExtra $ if name == "ghc" then ArchLinuxName "ghc" else toArchLinuxName name
  maybe (throw $ VersionNoParse raw) pure $ simpleParsec raw

data DependencyCache = DependencyCache
  { cachedDependencies :: Map.Map (Version, PackageName, Version) (Either MyException (VersionedList, VersionedList)),
    cachedDependencyConditions :: Map.Map (PackageName, Version) [VersionRange],
    cachedDependencyVariants :: Map.Map (PackageName, Version, [Bool]) (Either MyException (VersionedList, VersionedList)),
    cachedExistingDependencies :: Map.Map PackageName ExistingDependencies,
    cachedCandidateChecks :: Map.Map (Version, PackageName, Version, Bool, Map.Map PackageName Version) ([PlanProblem], [PlanProblem]),
    cachedToolchain :: Maybe Toolchain
  }
type ExistingDependencies = Map.Map (DepSrc, PackageName) (VersionRange, Maybe Version)
type ReverseChecks = [(PackageName, [ReverseDep], [SkippedReverseDep])]

emptyDependencies :: DependencyCache
emptyDependencies = DependencyCache Map.empty Map.empty Map.empty Map.empty Map.empty Nothing

selectToolchain :: PlanEffects r => GHCReleases -> Map.Map PackageName Version -> DependencyCache -> Sem r DependencyCache
selectToolchain releases selected cache = case Map.lookup "ghc" selected of
  Nothing -> pure cache
  Just release | Just toolchain <- cachedToolchain cache, toolchainVersion toolchain == release -> pure cache
  Just release -> do
    compiler <- ask @Version
    extra <- ask @ExtraDB
    let packages = releases Map.! release
        libraryNames = Map.fromList [(toArchLinuxName name, name) | name <- ghcLibList <> concatMap Map.keys (Map.elems releases)]
        provided =
          [ _pdName dependency
            | name <- ["ghc", "ghc-libs"],
              Just desc <- [Map.lookup (ArchLinuxName name) extra],
              dependency <- _provides desc,
              isHaskellPackage $ _pdName dependency
          ]
        libraries = Set.fromList [name | providedName <- provided, Just name <- [Map.lookup providedName libraryNames]]
        tools = Set.fromList [toHackageName name | name <- provided, Map.notMember name libraryNames]
        names = Set.unions [libraries, Set.fromList ghcLibList, Map.keysSet packages, Map.keysSet $ Map.findWithDefault Map.empty compiler releases]
    installed <- fmap (Map.fromList . catMaybes) $ forM (Set.toList names) $ \name -> do
      found <- try @MyException $ currentVersion name
      case found of
        Right version -> pure $ Just (name, version)
        Left (PkgNotFound _) -> pure $ (name,) <$> (Map.lookup compiler releases >>= Map.lookup name)
        Left err -> throw err
    pure cache {cachedToolchain = Just $ Toolchain release packages installed tools}

fixedPackage :: DependencyCache -> PackageName -> Bool
fixedPackage cache name = maybe (isGHCLibs name) (`toolchainContains` name) $ cachedToolchain cache

unknownCompilerTool :: DependencyCache -> PackageName -> Bool
unknownCompilerTool cache name = maybe False (Set.member name . toolchainTools) $ cachedToolchain cache

availableVersion :: PlanEffects r => DependencyCache -> Map.Map PackageName Version -> PackageName -> Sem r (Maybe Version)
availableVersion cache selected name
  | Just toolchain <- cachedToolchain cache, toolchainContains toolchain name = pure $ Map.lookup name $ toolchainPackages toolchain
  | Just version <- Map.lookup name selected = pure $ Just version
  | otherwise = do
      found <- try @MyException $ currentVersion name
      case found of
        Right version -> pure $ Just version
        Left (PkgNotFound _) -> pure Nothing
        Left err -> throw err

data SearchCache = SearchCache
  { searchInstalled :: Map.Map PackageName Version,
    searchChoices :: Map.Map PackageName [Version],
    searchDependencies :: DependencyCache,
    searchReverseChecks :: Map.Map (Set.Set PackageName) ReverseChecks,
    searchReversePackages :: Map.Map ArchLinuxName (Set.Set ArchLinuxName),
    searchRetainable :: Map.Map (PackageName, Int, PackageName, Maybe Version) Bool,
    searchRequiredRanges :: Map.Map PackageName VersionRange,
    searchRangeOrigins :: RangeOrigins,
    searchConflicts :: Map.Map PackageName PlanProblem,
    searchRepairBounds :: Map.Map (Maybe Int, PackageName, Maybe Int, PackageName, Maybe Int) RepairBound,
    searchUnavoidableFailures :: Map.Map (Maybe Int, PackageName, Int) Int
  }

data RangeOrigin
  = DependencyOrigin [Version] [DepSrc] VersionRange
  | ReverseUpdateOrigin [Version]

type RangeOrigins = Map.Map PackageName (Map.Map PackageName RangeOrigin)

data RepairBound = RepairBound Int [(Set.Set PackageName, Int)] (Map.Map Version Int)

externalChecks :: PlanEffects r => [PackageName] -> Map.Map ArchLinuxName (Set.Set ArchLinuxName) -> DependencyCache -> Sem r (ReverseChecks, DependencyCache)
externalChecks targets reversePackages cache = do
  let names = Set.toList $ Set.filter ((`notElem` targets) . toHackageName) $
        Set.unions [Map.findWithDefault Set.empty (toArchLinuxName target) reversePackages | target <- targets]
  (checked, cache') <- foldM
    (\(results, known) name -> do
      current <- try @MyException $ currentVersion $ toHackageName name
      case current of
        Left err -> pure ((name, Left err) : results, known)
        Right version -> do
          (deps, known') <- loadDependencies (toHackageName name) version known
          pure ((name, deps) : results, known'))
    ([], cache)
    names
  pure
    ( [ ( target,
          [ ReverseDep name ranges
            | (name, Right (depends, makeDepends)) <- checked,
              let ranges = [(src, range) | (src, deps) <- [(Run, depends), (Make, makeDepends)], (dependency, range) <- deps, dependency == target],
              not $ null ranges
          ],
          [ SkippedReverseDep name err
            | (name, Left err) <- checked,
              Set.member name $ Map.findWithDefault Set.empty (toArchLinuxName target) reversePackages
          ]
        )
          | target <- targets
      ],
      cache'
    )

search ::
  PlanEffects r =>
  GHCReleases ->
  Bool ->
  Map.Map PackageName Version ->
  Map.Map PackageName [Version] ->
  Sem r PlanResult
search releases solve installed choices = do
  tracePlan "Collecting required dependency ranges..."
  extra <- ask @ExtraDB
  let reversePackages = Map.fromListWith Set.union
        [ (_pdName dependency, Set.singleton $ _name desc)
          | desc <- Map.elems extra,
            isHaskellPackage $ _name desc,
            dependency <- _depends desc <> _makeDepends desc <> _checkDepends desc
        ]
  (requiredRanges, origins, dependencies) <- if updatingGHC then pure (Map.empty, Map.empty, emptyDependencies) else requestedRanges choices emptyDependencies
  let initial = SearchCache installed choices dependencies Map.empty reversePackages Map.empty requiredRanges origins Map.empty Map.empty Map.empty
  tracePlan "Propagating dependency constraints..."
  (conflict, directCache) <- if solve && not updatingGHC then propagateRanges False (Map.keysSet choices) initial else pure (Nothing, initial)
  cache <- case conflict of
    Nothing | solve && not updatingGHC -> do
      tracePlan "Propagating reverse-dependency constraints..."
      snd <$> propagateRanges True (Map.keysSet choices) directCache
    Just problem -> pure directCache {searchConflicts = Map.singleton (fst $ Map.findMin choices) problem}
    _ -> pure directCache
  let partial = not $ Map.null $ searchConflicts cache
      known = if partial then cache {searchRequiredRanges = Map.empty} else cache
  tracePlan $ if partial then "Constraints cannot all be satisfied; optimizing the partial plan..." else "Searching candidate update sets..."
  forM_ (Map.elems $ searchConflicts cache) $ tracePlan . show . prettySearchConflict (Map.keysSet choices) cache
  go partial
    startingQueue
    startingVisited
    known 0 Nothing
  where
    updatingGHC = Map.member "ghc" choices
    start = Map.map (const 0) choices
    startingStates = (0, start) :
      [(index, Map.insert name index start) | (name, candidates) <- Map.toList choices, index <- [1 .. length candidates - 1]]
    startingQueue = Map.fromList [((0 :: Int, cost, Down cost, indices), Nothing) | (cost, indices) <- startingStates]
    startingVisited = Set.fromList $ snd <$> startingStates
    versions catalog indices = Map.mapWithKey (\name index -> catalog Map.! name !! index) indices

    go partial queue visited cache tried best =
      case Map.minViewWithKey queue of
        Nothing -> case best of
          Just (_, result)
            | solve && not partial -> do
                tracePlan "No complete solution; optimizing the partial plan..."
                go True
                  startingQueue
                  startingVisited cache {searchRequiredRanges = Map.empty} tried best
            | otherwise -> finish tried cache result
          Nothing -> error "planner search starts with one candidate set"
        Just (((failureEstimate, estimate, Down cost, indices), queued), remaining)
          | partial && prunable failureEstimate estimate best -> go partial remaining visited cache tried best
          | Just (result, neighbors) <- queued ->
              continue partial failureEstimate estimate cost remaining visited cache tried best result neighbors
          | otherwise -> do
              tracePlan $ "Candidate " <> show (tried + 1) <> ": " <> show cost <> " release steps, " <> show (Map.size remaining) <> " queued; " <>
                intercalate ", " [unPackageName name <> " " <> prettyShow version | (name, version) <- Map.toList $ versions (searchChoices cache) indices]
              toolchainDependencies <- selectToolchain releases (versions (searchChoices cache) indices) (searchDependencies cache)
              let selected = Map.filterWithKey (\name _ -> name == "ghc" || not (fixedPackage toolchainDependencies name)) $ versions (searchChoices cache) indices
                  targets = Map.keysSet selected
              (reverseChecks, dependencies) <- if updatingGHC then pure ([], toolchainDependencies) else case Map.lookup targets (searchReverseChecks cache) of
                Just cached -> pure (cached, searchDependencies cache)
                Nothing -> externalChecks (Map.keys selected) (searchReversePackages cache) (searchDependencies cache)
              (problems, warnings, dependencies') <- checkSet (searchInstalled cache) selected reverseChecks dependencies
              tracePlan $ "Candidate " <> show (tried + 1) <> ": " <> show (length problems) <> " blockers, " <> show (length warnings) <> " warnings"
              let checkedCache = cache
                    { searchDependencies = dependencies',
                      searchReverseChecks = Map.insert targets reverseChecks (searchReverseChecks cache)
                    }
              (initialBounds, boundedCache) <- if not (solve && updatingGHC) && (partial || solve && tried == 0)
                then repairBounds releases indices selected problems checkedCache
                else pure ([], checkedCache)
              (ownerFailures, ownerCache) <- if not (solve && updatingGHC) && not (null problems) && (partial || solve && tried == 0 || jointFailures initialBounds > 0)
                then unavoidableFailures releases indices boundedCache
                else pure (0, boundedCache)
              (bounds, cache') <- if ownerFailures > 0 && null initialBounds
                then repairBounds releases indices selected problems ownerCache
                else pure (initialBounds, ownerCache)
              let unavoidable = sum [minimumFailures | (_, RepairBound minimumFailures _ _) <- bounds]
                  failureBound = max ownerFailures $ jointFailures bounds
                  boundedQueue = if indices == start && failureBound > 0
                    then Map.fromList
                      [((max failureBound failures, priority, depth, candidate), queuedCandidate)
                        | ((failures, priority, depth, candidate), queuedCandidate) <- Map.toList remaining]
                    else remaining
                  partial' = partial || failureBound > 0
                  diagnosed = if not partial && failureBound > 0
                    then cache' {searchConflicts = Map.fromList
                      [(owner, problem) | (problem : _, RepairBound minimumFailures _ _) <- bounds,
                        owner : _ <- [problemTargets problem], minimumFailures > 0]}
                    else cache'
                  result = PlanResult
                    { planInstalled = Map.filterWithKey (\name _ -> Map.member name selected) (searchInstalled cache),
                      planVersions = selected,
                      planRequested = Map.keysSet choices,
                      planProblems = problems,
                      planWarnings = warnings,
                      plansTried = tried + 1,
                      planToolchain = cachedToolchain dependencies',
                      planRevisionNotes = [],
                      planSearchNotes = []
                    }
                  best' = case best of
                    Just (previousCost, previous)
                      | (length (planProblems previous), previousCost) <= (length problems, cost) -> best
                    _ -> Just (cost, result)
              when (maybe True (\(previousCost, previous) -> (length problems, cost) < (length $ planProblems previous, previousCost)) best) $ do
                tracePlan $ "Best plan: " <> show (length problems) <> " blockers, " <> show cost <> " release steps"
                forM_ problems $ tracePlan . show . prettyProblem
              when (not partial && partial') $ tracePlan $ "At least " <> show failureBound <> " blockers are unavoidable; optimizing the partial plan..."
              case problems of
                [] -> finish (tried + 1) cache' result
                _ | solve && updatingGHC -> do
                  tracePlan "Optimizing compiler alternatives without incremental update-set enumeration..."
                  (checked, optimized) <- optimizePartial releases choices cache' (tried + 1) best'
                  let conflicts = Map.fromList
                        [(owner, problem) | problem <- planProblems optimized, owner : _ <- [problemTargets problem]]
                  finish checked cache' {searchConflicts = conflicts} optimized
                _ | partial' -> do
                  let active = [alternatives | (related, RepairBound minimumFailures alternatives _) <- bounds, length related > minimumFailures]
                      names = Set.toList $ Set.unions $ fst <$> concat active
                  (neighbors, known) <- foldM (advance indices) ([], diagnosed {searchRequiredRanges = Map.empty}) names
                  let movable = Set.fromList $ fst <$> neighbors
                      minimumFailures = max failureEstimate $ max 1 failureBound
                      allowance = minimumFailures - unavoidable
                      steps = remainingCost movable allowance active
                      priority = if minimumFailures == failureEstimate then max estimate (cost + steps) else cost + steps
                  tracePlan $ "Partial-plan lower bound: " <> show minimumFailures <> " blockers, " <> show priority <> " release steps"
                  if prunable minimumFailures priority best'
                    then go True boundedQueue visited known (tried + 1) best'
                    else do
                      (checked, optimized) <- optimizePartial releases choices known (tried + 1) best'
                      finish checked known optimized
                _ -> do
                  (neighbors, cache'') <- foldM (advance indices) ([], cache') (nub $ ["ghc" | updatingGHC] <> concatMap problemTargets problems)
                  let movable = Set.fromList $ fst <$> neighbors
                  (repairs, cache''') <- if updatingGHC
                    then pure
                      ( filter (not . Set.null) [Set.intersection movable $ Set.fromList $ "ghc" : problemTargets problem | problem <- problems],
                        cache''
                      )
                    else repairChoices movable indices problems cache''
                  let lowerBound = max (requiredSteps indices cache''') (remainingSteps movable repairs)
                      priority = max estimate (cost + lowerBound)
                      -- Every complete solution must repair this clause. Keep all
                      -- of its alternatives, without enumerating interleavings of
                      -- unrelated repairs. Prefer small clauses and preserve the
                      -- requested versions when either choice costs the same.
                      branchKey clause = (Set.size clause, not $ Set.null $ Set.intersection (Map.keysSet choices) clause, Set.toList clause)
                      next = case sortOn branchKey repairs of
                        [] -> []
                        clause : _ -> [nextState | (name, nextState) <- neighbors, Set.member name clause]
                  tracePlan $ "Complete-plan lower bound: " <> show priority <> " release steps; " <> show (length next) <> " branches"
                  -- Refine a queued lower bound before expanding the node. Retain
                  -- its evaluation so reordering does not repeat metadata checks.
                  if priority > estimate
                    then go
                      False (Map.insert (0, priority, Down cost, indices) (Just (result, next)) boundedQueue)
                      visited cache''' (tried + 1) best'
                    else continue False 0 priority cost boundedQueue visited cache''' (tried + 1) best' result next

    prunable failures cost best = case best of
      Just (previousCost, previous) -> (failures, cost) >= (length $ planProblems previous, previousCost)
      Nothing -> False

    finish tried cache result = do
      tracePlan $ "Search finished after " <> show tried <> " candidates; " <> show (length $ planProblems result) <> " blockers remain"
      pure result { plansTried = tried,
        planSearchNotes =
          [ vsep $ "Full-solution conflicts (not additional blockers in the partial plan):"
              : (indent 2 . prettySearchConflict (planRequested result) cache <$> Map.elems (searchConflicts cache))
            | not $ Map.null $ searchConflicts cache
          ]
      }

    continue partial failureEstimate estimate cost remaining visited cache tried best result neighbors
      | null $ planProblems result = pure result {plansTried = tried}
      | otherwise =
          let unseen = filter (`Set.notMember` visited) neighbors
              -- One release step can reduce the remaining cost by at most one.
              priority = max estimate (cost + 1)
           in go partial
                (foldr (\indices -> Map.insert (failureEstimate, priority, Down (cost + 1), indices) Nothing) remaining unseen)
                (foldr Set.insert visited unseen)
                cache tried best

    advance indices (neighbors, cache) name
      | name /= "ghc" && fixedPackage (searchDependencies cache) name = pure (neighbors, cache)
      | otherwise =
          case Map.lookup name indices of
            Just index -> pure
              ( [(name, Map.adjust (+ 1) name indices) | index + 1 < length (searchChoices cache Map.! name)] <> neighbors,
                cache
              )
            Nothing
              | not solve || isGHCLibs name -> pure (neighbors, cache)
              | otherwise -> do
                  cache' <- discoverVersions name cache
                  pure ([(name, Map.insert name 0 indices) | not $ null $ searchChoices cache' Map.! name] <> neighbors, cache')

data DomainOption = DomainOption
  { optionVersion :: Maybe Version,
    optionUpdated :: Bool,
    optionSteps :: Int
  }

data ConstraintView = ConstraintView Bool Int VersionedList

data ConstraintProblem = ConstraintProblem
  { constraintDomains :: Map.Map PackageName (IntMap.IntMap DomainOption),
    constraintViews :: Map.Map (PackageName, Int) ConstraintView,
    constraintCache :: SearchCache
  }

inspectConstraint :: PlanEffects r => PackageName -> DomainOption -> SearchCache -> Sem r (ConstraintView, SearchCache)
inspectConstraint owner option cache = case optionVersion option of
  Nothing -> pure (ConstraintView False 0 [], cache)
  Just version -> do
    (parsed, loaded) <- loadDependencies owner version $ searchDependencies cache
    (existing, known) <- existingDependencies owner loaded
    compiler <- ask @Version
    let updated = optionUpdated option
        repository = not updated
        checkingCompiler = isJust $ cachedToolchain known
        previous = Map.lookup (compiler, owner, version) (cachedDependencies known) >>= either (const Nothing) Just
        activeRanges = case parsed of
          Left _ -> []
          Right parts -> tagDependencies parts
    ranges <- fmap catMaybes $ forM activeRanges $ \(src, dependency, range) ->
      if updated || checkingCompiler
        then pure $ if dependencyWasBroken previous known existing src dependency range then Nothing else Just (dependency, range)
        else if Set.member (toArchLinuxName owner) $ Map.findWithDefault Set.empty (toArchLinuxName dependency) (searchReversePackages cache)
          then do
            installed <- availableVersion known Map.empty dependency
            pure $ if maybe False (not . (`withinRange` range)) installed then Nothing else Just (dependency, range)
          else pure Nothing
    pure (ConstraintView updated (case parsed of Left _ | not repository -> 1; _ -> 0) ranges,
      cache {searchDependencies = known})

introduceConstraints :: PlanEffects r => Map.Map PackageName [Version] -> Set.Set PackageName -> ConstraintProblem -> Sem r ConstraintProblem
introduceConstraints requested = go
  where
    go pending problem = case Set.minView pending of
      Nothing -> pure problem
      Just (name, rest)
        | Map.member name $ constraintDomains problem -> go rest problem
        | fixedPackage (searchDependencies $ constraintCache problem) name -> go rest problem
        | otherwise -> do
            known <- discoverVersions name $ constraintCache problem
            let installed = Map.lookup name $ searchInstalled known
                candidates = searchChoices known Map.! name
                values = case Map.lookup name requested of
                  Just releases -> IntMap.fromList [(index, DomainOption (Just version) True index) | (index, version) <- zip [0 ..] releases]
                  Nothing -> IntMap.fromList $ (-1, DomainOption installed False 0) :
                    [(index, DomainOption (Just version) True (index + 1)) | (index, version) <- zip [0 ..] candidates]
                added = problem {constraintDomains = Map.insert name values $ constraintDomains problem, constraintCache = known}
            withInstalled <- case IntMap.lookup (-1) values of
              Nothing -> pure added
              Just option -> do
                (view, checked) <- inspectConstraint name option known
                pure added {constraintViews = Map.insert (name, -1) view $ constraintViews added, constraintCache = checked}
            (owners, checked) <- if isJust $ cachedToolchain $ searchDependencies $ constraintCache withInstalled
              then pure ([], constraintCache withInstalled)
              else reverseOwners name values $ constraintCache withInstalled
            let direct = case Map.lookup (name, -1) $ constraintViews withInstalled of
                  Just (ConstraintView _ _ ranges) | isJust $ cachedToolchain $ searchDependencies checked -> fst <$> ranges
                  _ -> []
            go (Set.unions [rest, Set.fromList owners, Set.fromList direct]) withInstalled {constraintCache = checked}

    reverseOwners target values initial = foldM inspect ([], initial) $ Set.toList $
      Map.findWithDefault Set.empty (toArchLinuxName target) (searchReversePackages initial)
      where
        inspect (owners, cache) archOwner = do
          let owner = toHackageName archOwner
          found <- try @MyException $ currentVersion owner
          case found of
            Left _ -> pure (owners, cache)
            Right version -> do
              (parsed, loaded) <- loadDependencies owner version $ searchDependencies cache
              installed <- availableVersion loaded Map.empty target
              let ranges = case parsed of
                    Left _ -> []
                    Right parts -> [range | (_, dependency, range) <- tagDependencies parts, dependency == target,
                      not $ maybe False (not . (`withinRange` range)) installed]
                  broken = any (\option -> optionUpdated option && any (\range -> maybe True (not . (`withinRange` range)) $ optionVersion option) ranges) $ IntMap.elems values
              pure ([owner | broken] <> owners, cache {searchDependencies = loaded})

expandConstraints :: PlanEffects r => Map.Map PackageName [Version] -> [(PackageName, Int)] -> ConstraintProblem -> Sem r ConstraintProblem
expandConstraints requested selections initial = do
  let names = Set.fromList $ fst <$> selections
      candidates = [(name, value) | name <- Set.toList names,
        (value, option) <- IntMap.toList $ constraintDomains initial Map.! name,
        optionUpdated option, Map.notMember (name, value) $ constraintViews initial]
  (expanded, dependencies) <- foldM inspect (initial, Set.empty) candidates
  introduceConstraints requested dependencies expanded
  where
    inspect (problem, dependencies) key@(name, value) = do
      tracePlan $ "Expanding constraint metadata for " <> unPackageName name <> " " <>
        maybe "(missing)" prettyShow (optionVersion $ constraintDomains problem Map.! name IntMap.! value)
      (view@(ConstraintView _ _ ranges), known) <- inspectConstraint name (constraintDomains problem Map.! name IntMap.! value) $ constraintCache problem
      pure (problem {constraintViews = Map.insert key view $ constraintViews problem, constraintCache = known},
        Set.union dependencies $ Set.fromList $ fst <$> ranges)

constraintModel :: PlanEffects r => Int -> ConstraintProblem -> Sem r (Solver.Model PackageName)
constraintModel compilerCost problem = foldM addView initial $ Map.toList $ constraintViews problem
  where
    domains = constraintDomains problem
    known = searchDependencies $ constraintCache problem
    checkingCompiler = isJust $ cachedToolchain known
    initial = Solver.Model (0, compilerCost) (Map.map (IntMap.map $ \option -> (0, optionSteps option)) domains) Map.empty
    addCost (failures, steps) (otherFailures, otherSteps) = (failures + otherFailures, steps + otherSteps)
    addUnary owner value failures model = model
      {Solver.modelDomains = Map.adjust (IntMap.adjust (addCost (failures, 0)) value) owner $ Solver.modelDomains model}
    addView model ((owner, value), ConstraintView updated unchecked ranges) =
      foldM (addRange owner value updated) (addUnary owner value unchecked model) ranges
    addRange owner value updated model (dependency, range)
      | unknownCompilerTool known dependency = pure model
      | dependency == owner = pure $ addUnary owner value (failure updated $ domains Map.! owner IntMap.! value) model
      | fixedPackage known dependency = do
          actual <- availableVersion known Map.empty dependency
          pure $ addUnary owner value (failure updated $ DomainOption actual False 0) model
      | Just available <- Map.lookup dependency domains = do
          let key = if owner < dependency then (owner, dependency) else (dependency, owner)
              emptyTable = IntMap.map (const $ IntMap.map (const (0, 0)) $ domains Map.! snd key) $ domains Map.! fst key
              table = Map.findWithDefault emptyTable key $ Solver.modelEdges model
              costs = IntMap.map (\option -> (failure updated option, 0)) available
              combined = if owner < dependency
                then IntMap.adjust (IntMap.unionWith addCost costs) value table
                else IntMap.mapWithKey (\other -> IntMap.adjust (addCost $ costs IntMap.! other) value) table
          pure model {Solver.modelEdges = Map.insert key combined $ Solver.modelEdges model}
      | otherwise = pure model
      where
        failure active option = if (active || checkingCompiler || optionUpdated option)
          && maybe True (not . (`withinRange` range)) (optionVersion option) then 1 else 0

optimizePartial :: PlanEffects r => GHCReleases -> Map.Map PackageName [Version] -> SearchCache -> Int -> Maybe (Int, PlanResult) -> Sem r (Int, PlanResult)
optimizePartial releases requested initial tried best = do
  tracePlan "Optimizing finite package domains instead of enumerating update subsets..."
  (_, checked, final) <- foldM compiler (initial, tried, best) compilers
  case final of
    Just (_, result) -> pure (checked, result)
    Nothing -> error "constraint solver starts with an evaluated plan"
  where
    compilers = case Map.lookup "ghc" requested of
      Nothing -> [(Nothing, 0)]
      Just versions -> [(Just version, index) | (index, version) <- zip [0 ..] versions]
    compiler (cache, checked, previous) (release, cost)
      | Just (steps, result) <- previous, null (planProblems result), cost >= steps = pure (cache, checked, previous)
      | otherwise = do
          tracePlan $ "Considering " <> maybe "package updates" (\version -> "GHC " <> prettyShow version) release <>
            "; best score " <> show ((\(steps, result) -> (length $ planProblems result, steps)) <$> previous)
          dependencies <- case release of
            Nothing -> pure $ (searchDependencies cache) {cachedToolchain = Nothing}
            Just version -> selectToolchain releases (Map.singleton "ghc" version) $ searchDependencies cache
          extra <- ask @ExtraDB
          let names = Map.keysSet (Map.delete "ghc" requested) `Set.union` case cachedToolchain dependencies of
                Nothing -> Set.empty
                Just toolchain -> Set.fromList
                  [toHackageName $ _name desc | desc <- Map.elems extra, isHaskellPackage $ _name desc,
                    Set.notMember (_name desc) $ toolchainArchPackages toolchain, isJust $ simpleParsec @Version $ _version desc]
              problem = ConstraintProblem Map.empty Map.empty cache {searchDependencies = dependencies, searchRequiredRanges = Map.empty}
          introduced <- introduceConstraints requested names problem
          let cachedSelections = [(name, value) | isJust release, (name, values) <- Map.toList $ constraintDomains introduced,
                (value, option) <- IntMap.toList values, optionUpdated option,
                Just version <- [optionVersion option],
                Map.member (name, version) $ cachedDependencyConditions $ searchDependencies $ constraintCache introduced]
          expanded <- expandConstraints requested
            (cachedSelections <> [(name, value) | name <- Map.keys $ Map.delete "ghc" requested,
              value <- IntMap.keys $ constraintDomains introduced Map.! name]) introduced
          let limit = (\(steps, result) -> (length $ planProblems result, steps)) <$> previous
          (improved, searched, known) <- refine limit cost checked expanded
          case improved of
            Nothing -> do
              tracePlan $ "Pruned " <> maybe "package updates" (\version -> "GHC " <> prettyShow version) release <>
                ": no improvement below " <> show limit
              pure (known, searched, previous)
            Just (score, selected) -> do
              let selectedCompiler = maybe selected (\version -> Map.insert "ghc" version selected) release
              (reverseChecks, loaded) <- case release of
                Just _ -> pure ([], searchDependencies known)
                Nothing -> externalChecks (Map.keys selectedCompiler) (searchReversePackages known) $ searchDependencies known
              (problems, warnings, finalDependencies) <- checkSet (searchInstalled known) selectedCompiler reverseChecks loaded
              unless (fst score == length problems) $ error $ "constraint solver blocker count differs from checked plan: " <> show score <> " versus " <> show (length problems)
              let result = PlanResult
                    { planInstalled = Map.restrictKeys (searchInstalled known) $ Map.keysSet selectedCompiler,
                      planVersions = selectedCompiler,
                      planRequested = Map.keysSet requested,
                      planProblems = problems,
                      planWarnings = warnings,
                      plansTried = searched,
                      planToolchain = cachedToolchain finalDependencies,
                      planRevisionNotes = [],
                      planSearchNotes = []
                    }
              tracePlan $ "Verified constraint optimum: " <> show (length problems) <> " blockers, " <> show (snd score) <> " release steps"
              pure (known {searchDependencies = finalDependencies}, searched, Just (snd score, result))

    refine limit cost checked problem = do
      model <- constraintModel cost problem
      tracePlan $ "Constraint model: " <> show (Map.size $ Solver.modelDomains model) <> " packages, " <>
        show (Map.size $ Solver.modelEdges model) <> " dependency edges"
      (improved, nodes) <- Solver.optimizeBelow tracePlan limit model
      case improved of
        Nothing -> pure (Nothing, checked, constraintCache problem)
        Just (score, assignment) -> do
          tracePlan $ "Constraint lower bound " <> show score <> " after " <> show nodes <> " branches"
          let unknown = [(name, value) | (name, value) <- Map.toList assignment,
                optionUpdated $ constraintDomains problem Map.! name IntMap.! value,
                Map.notMember (name, value) $ constraintViews problem]
          if null unknown
            then pure (Just (score, Map.mapMaybeWithKey
              (\name value -> let option = constraintDomains problem Map.! name IntMap.! value
                in if optionUpdated option then optionVersion option else Nothing) assignment), checked + 1, constraintCache problem)
            else do
              expanded <- expandConstraints requested unknown problem
              refine limit cost (checked + 1) expanded

jointFailures :: [([PlanProblem], RepairBound)] -> Int
jointFailures bounds = case Set.toList compilers of
  [] -> 0
  available -> minimum [sum [Map.findWithDefault 0 compiler failures | (_, RepairBound _ _ failures) <- bounds] | compiler <- available]
  where
    compilers = Set.unions [Map.keysSet failures | (_, RepairBound _ _ failures) <- bounds]

remainingCost :: Set.Set PackageName -> Int -> [[(Set.Set PackageName, Int)]] -> Int
remainingCost movable allowance repairs = sum $ drop allowance $ sortOn Down costs
  where
    branchKey (names, cost) = (Set.size names, Down cost, Set.toList names)
    clauses = if allowance == 0 then concat repairs else
      [first | alternatives <- repairs, first : _ <- [sortOn branchKey alternatives]]
    actionable = [(names', cost) | (names, cost) <- clauses, let names' = Set.intersection movable names, not $ Set.null names']
    (_, costs) = foldl' count (Set.empty, []) $ sortOn branchKey actionable
    count (used, totals) (names, cost)
      | Set.null $ Set.intersection used names = (Set.union used names, cost : totals)
      | otherwise = (used, totals)

repairBounds :: PlanEffects r => GHCReleases -> Map.Map PackageName Int -> Map.Map PackageName Version -> [PlanProblem] -> SearchCache -> Sem r ([([PlanProblem], RepairBound)], SearchCache)
repairBounds releases indices selected problems initial = foldM collect (unchecked, initial) $ Map.toList grouped
  where
    groupKey problem = case problem of
      DependencyProblem owner dependency _ _ -> Just (owner, dependency)
      ReverseDependencyProblem owner dependency _ _ _ -> Just (toHackageName owner, dependency)
      _ -> Nothing
    grouped = Map.fromListWith (<>) [(key, [problem]) | problem <- problems, Just key <- [groupKey problem]]
    unchecked = [([problem], RepairBound 0 [(Set.fromList $ problemTargets problem, 1)] Map.empty) | problem <- problems, Nothing <- [groupKey problem]]

    collect (bounds, cache) ((owner, dependency), related) = do
      let key = (Map.lookup "ghc" indices, owner, Map.lookup owner indices, dependency, Map.lookup dependency indices)
      case Map.lookup key $ searchRepairBounds cache of
        Just bound -> pure ((related, bound) : bounds, cache)
        Nothing -> do
          tracePlan $ "Analyzing repairs for " <> unPackageName owner <> " -> " <> unPackageName dependency
          (bound, checked) <- check owner dependency (length related) cache
          pure ((related, bound) : bounds, checked {searchRepairBounds = Map.insert key bound $ searchRepairBounds checked})

    check owner dependency failures cache = do
      (owners, known) <- domain owner cache
      (dependencies, known') <- domain dependency known
      installedCompiler <- ask @Version
      let compilers = case Map.lookup "ghc" indices of
            Nothing -> [(installedCompiler, 0)]
            Just index -> [(compiler, offset) | (offset, compiler) <- zip [0 :: Int ..] $ drop index $ searchChoices cache Map.! "ghc"]
      ((minimumFailures, mandatory, alternatives, minimumCost, compilerFailures), checked) <- foldM
        (compilerBounds owner dependency failures owners dependencies)
        ((failures, Nothing, Set.empty, maxBound, Map.empty), known') compilers
      let restored = checked {searchDependencies = (searchDependencies checked)
            { cachedToolchain = cachedToolchain $ searchDependencies cache }}
          repairs = case mandatory of
            Nothing -> []
            Just required
              | Map.null required -> [(alternatives, minimumCost)]
              | otherwise -> [(Set.singleton name, cost) | (name, cost) <- Map.toList required]
      pure (RepairBound minimumFailures repairs compilerFailures, restored)

    domain name cache
      | isGHCLibs name || unknownCompilerTool (searchDependencies cache) name = pure ([], cache)
      | otherwise = do
          known <- discoverVersions name cache
          let versions = Map.findWithDefault [] name $ searchChoices known
              available = case Map.lookup name indices of
                Just index -> [(Just version, offset) | (offset, version) <- zip [0 :: Int ..] $ drop index versions]
                Nothing -> (Map.lookup name $ searchInstalled known, 0) : [(Just version, cost) | (cost, version) <- zip [1 :: Int ..] versions]
          pure (available, known)

    compilerBounds owner dependency failures owners dependencies (summary, cache) (compiler, compilerCost) = do
      toolchain <- if Map.member "ghc" selected
        then selectToolchain releases (Map.singleton "ghc" compiler) (searchDependencies cache)
        else pure $ searchDependencies cache
      let known = cache {searchDependencies = toolchain}
      available <- if fixedPackage toolchain dependency
        then (\actual -> [(actual, 0)]) <$> availableVersion toolchain Map.empty dependency
        else pure dependencies
      foldM (candidateBounds owner dependency failures compiler compilerCost available) (summary, known) owners

    candidateBounds owner dependency failures targetCompiler compilerCost available (summary, cache) (candidate, ownerCost) = do
      let toolchain = searchDependencies cache
      (ranges, known) <- if fixedPackage toolchain owner || unknownCompilerTool toolchain dependency
        then pure ([], toolchain)
        else case candidate of
          Nothing -> pure ([], toolchain)
          Just version -> do
            (parsed, loaded) <- loadDependencies owner version toolchain
            (existing, loaded') <- existingDependencies owner loaded
            compiler <- ask @Version
            let previous = Map.lookup (compiler, owner, version) (cachedDependencies loaded') >>= either (const Nothing) Just
                required = case parsed of
                  Left _ -> []
                  Right parts -> [range | (src, name, range) <- tagDependencies parts, name == dependency,
                    not $ dependencyWasBroken previous loaded' existing src name range]
            pure (required, loaded')
      let summarize (minimumFailures, mandatory, alternatives, minimumCost, compilerFailures) (actual, dependencyCost) =
            let count = length [range | range <- ranges, maybe True (not . (`withinRange` range)) actual]
                changed = Map.fromListWith max $ [(owner, ownerCost) | ownerCost > 0] <>
                  [(dependency, dependencyCost) | dependencyCost > 0] <> [("ghc", compilerCost) | compilerCost > 0]
                perCompiler = Map.insertWith min targetCompiler count compilerFailures
             in if count < failures
                  then (min minimumFailures count, Just $ maybe changed (Map.intersectionWith min changed) mandatory,
                    Set.union (Map.keysSet changed) alternatives, min minimumCost $ sum $ Map.elems changed, perCompiler)
                  else (minimumFailures, mandatory, alternatives, minimumCost, perCompiler)
      pure (foldl' summarize summary available, cache {searchDependencies = known})

unavoidableFailures :: PlanEffects r => GHCReleases -> Map.Map PackageName Int -> SearchCache -> Sem r (Int, SearchCache)
unavoidableFailures releases indices initial = foldM collect (0, initial) $ Map.toList $ Map.delete "ghc" indices
  where
    collect (total, cache) (owner, index) = do
      let key = (Map.lookup "ghc" indices, owner, index)
      case Map.lookup key $ searchUnavoidableFailures cache of
        Just count -> pure (total + count, cache)
        Nothing -> do
          tracePlan $ "Checking unavoidable future failures of " <> unPackageName owner
          installedCompiler <- ask @Version
          let compilers = case Map.lookup "ghc" indices of
                Nothing -> [installedCompiler]
                Just compilerIndex -> drop compilerIndex $ searchChoices cache Map.! "ghc"
              candidates = drop index $ searchChoices cache Map.! owner
          (counts, known) <- foldM (compilerFailures owner candidates) ([], cache) compilers
          let count = minimum counts
              restored = known {searchDependencies = (searchDependencies known)
                    {cachedToolchain = cachedToolchain $ searchDependencies cache}}
          pure (total + count, restored {searchUnavoidableFailures = Map.insert key count $ searchUnavoidableFailures restored})

    compilerFailures owner candidates (counts, cache) compiler = do
      toolchain <- if Map.member "ghc" indices
        then selectToolchain releases (Map.singleton "ghc" compiler) (searchDependencies cache)
        else pure $ searchDependencies cache
      if fixedPackage toolchain owner
        then pure (0 : counts, cache)
        else foldM (candidateFailures owner) (counts, cache {searchDependencies = toolchain}) candidates

    candidateFailures owner (counts, cache) candidate = do
      (parsed, loaded) <- loadDependencies owner candidate $ searchDependencies cache
      (existing, known) <- existingDependencies owner loaded
      compiler <- ask @Version
      let previous = Map.lookup (compiler, owner, candidate) (cachedDependencies known) >>= either (const Nothing) Just
          grouped = case parsed of
            Left _ -> Map.empty
            Right parts -> Map.fromListWith (<>)
              [(dependency, [range]) | (src, dependency, range) <- tagDependencies parts,
                not $ dependencyWasBroken previous known existing src dependency range]
      (count, checked) <- foldM dependencyFailures (0, cache {searchDependencies = known}) $ Map.toList grouped
      pure (count : counts, checked)

    dependencyFailures (total, cache) (dependency, ranges)
      | unknownCompilerTool (searchDependencies cache) dependency = pure (total, cache)
      | fixedPackage (searchDependencies cache) dependency = do
          actual <- availableVersion (searchDependencies cache) Map.empty dependency
          pure (total + failures actual, cache)
      | otherwise = do
          known <- discoverVersions dependency cache
          let available = Map.lookup dependency (searchInstalled known) :
                (Just <$> Map.findWithDefault [] dependency (searchChoices known))
          pure (total + minimum (failures <$> available), known)
      where
        failures actual = length [range | range <- ranges, maybe True (not . (`withinRange` range)) actual]

-- Disjoint repair choices each require at least one separate release step.
-- Shared dependencies are deliberately counted only once, so the estimate
-- cannot rule out a smaller update set. Unrepairable constraints contribute
-- zero, allowing independent improvements to a blocked plan.
remainingSteps :: Set.Set PackageName -> [Set.Set PackageName] -> Int
remainingSteps movable repairs = snd $ foldl' count (Set.empty, 0) choices
  where
    choices = sortOn Set.size $ filter (not . Set.null)
      [Set.intersection movable repair | repair <- repairs]
    count (used, total) candidates
      | Set.null $ Set.intersection used candidates = (Set.union used candidates, total + 1)
      | otherwise = (used, total)

requiredSteps :: Map.Map PackageName Int -> SearchCache -> Int
requiredSteps indices cache = sum
  [ case Map.lookup name indices of
      Just current -> firstCost [(index - current, version) | (index, version) <- indexed, index >= current]
      Nothing
        | maybe False (`withinRange` range) (Map.lookup name $ searchInstalled cache) -> 0
        | otherwise -> firstCost [(index + 1, version) | (index, version) <- indexed]
    | (name, range) <- Map.toList (searchRequiredRanges cache),
      let indexed = zip [0 ..] $ Map.findWithDefault [] name (searchChoices cache)
          firstCost candidates = case [cost | (cost, version) <- candidates, withinRange version range] of
            cost : _ -> cost
            [] -> 0
  ]

-- If no future owner version accepts the current dependency, updating the
-- dependency is mandatory. Conversely, if no future dependency fits the
-- current range, the owner must change. Both may be necessary for lockstep
-- releases; recognizing that avoids counting them as interchangeable repairs.
repairChoices :: PlanEffects r => Set.Set PackageName -> Map.Map PackageName Int -> [PlanProblem] -> SearchCache -> Sem r ([Set.Set PackageName], SearchCache)
repairChoices movable indices problems cache = foldM repair ([], cache) problems
  where
    repair (clauses, known) problem = case problem of
      DependencyProblem owner dependency range actual -> dependencyRepair clauses known owner dependency range actual
      ReverseDependencyProblem owner dependency _ range actual -> dependencyRepair clauses known (toHackageName owner) dependency range (Just actual)
      _ -> pure (add [Set.fromList $ problemTargets problem] clauses, known)

    -- If a necessary action is impossible, this failure cannot be repaired in
    -- this branch. Keep its diagnostic while working on independent failures.
    add alternatives clauses =
      let actionable = Set.intersection movable <$> alternatives
       in if any Set.null actionable then clauses else actionable <> clauses

    future name known = drop (nextIndex name) $ Map.findWithDefault [] name (searchChoices known)
    nextIndex name = maybe 0 (+ 1) $ Map.lookup name indices

    dependencyRepair clauses known owner dependency range actual = do
      let key = (owner, nextIndex owner, dependency, actual)
      (canKeepDependency, known') <- case Map.lookup key (searchRetainable known) of
        Just cached -> pure (cached, known)
        Nothing -> do
          (compatible, dependencies) <- acceptsCurrent (searchRequiredRanges known) owner dependency actual (future owner known) (searchDependencies known)
          pure (compatible, known
            { searchDependencies = dependencies,
              searchRetainable = Map.insert key compatible (searchRetainable known)
            })
      let allowed = Map.findWithDefault anyVersion dependency (searchRequiredRanges known')
          canKeepOwner = any (\v -> withinRange v range && withinRange v allowed) $ future dependency known'
          forced = [Set.singleton dependency | not canKeepDependency] <> [Set.singleton owner | not canKeepOwner]
      pure (add (if null forced then [Set.fromList [owner, dependency]] else forced) clauses, known')

    acceptsCurrent _ _ _ _ [] dependencies = pure (False, dependencies)
    acceptsCurrent required owner dependency actual (candidate : rest) dependencies = do
      (parsed, dependencies') <- loadDependencies owner candidate dependencies
      (existing, dependencies'') <- existingDependencies owner dependencies'
      case parsed of
        -- Unknown metadata must not strengthen a lower bound.
        Left _ -> pure (True, dependencies'')
        Right parts
          | not (withinRange candidate $ Map.findWithDefault anyVersion owner required)
              || any (\(name, range) -> null $ asVersionIntervals $ intersectVersionRanges range $ Map.findWithDefault anyVersion name required)
                (requiredDependencies existing parts) -> acceptsCurrent required owner dependency actual rest dependencies''
          | all (\(_, range) -> maybe False (`withinRange` range) actual)
              (filter ((== dependency) . fst) $ requiredDependencies existing parts) -> pure (True, dependencies'')
          | otherwise -> acceptsCurrent required owner dependency actual rest dependencies''

-- Only dependencies present in every possible release of a requested target
-- constrain the whole search. Their ranges are unions across releases and
-- intersections across targets. These bounds prevent impossible later releases
-- from weakening the estimate for an otherwise mandatory update.
requestedRanges :: PlanEffects r => Map.Map PackageName [Version] -> DependencyCache -> Sem r (Map.Map PackageName VersionRange, RangeOrigins, DependencyCache)
requestedRanges choices cache = foldM collect (Map.empty, Map.empty, cache) (Map.toList choices)
  where
    collect (required, origins, known) (name, releases) = do
      tracePlan $ "Required ranges for " <> unPackageName name <> ": " <> show (length releases) <> " releases"
      (existing, known') <- existingDependencies name known
      (common, sources, known'') <- foldM
        (\(previous, previousSources, parsed) release -> do
          (result, parsed') <- loadDependencies name release parsed
          let dependencies = case result of
                Left _ -> []
                Right parts -> [(src, dependency, range) | (src, dependency, range) <- tagDependencies parts,
                  not $ existingDependencyFailure existing src dependency range]
              bounds = Map.fromListWith intersectVersionRanges [(dependency, range) | (_, dependency, range) <- dependencies]
              dependencySources = Map.fromListWith Set.union [(dependency, Set.singleton src) | (src, dependency, _) <- dependencies]
          pure (Just $ maybe bounds (Map.intersectionWith unionVersionRanges bounds) previous,
            Map.unionWith Set.union previousSources dependencySources, parsed'))
        (Nothing, Map.empty, known')
        releases
      let own = foldr (unionVersionRanges . thisVersion) noVersion releases
          dependencies = Map.map simplifyVersionRange $ fromMaybe Map.empty common
          bounds = Map.insertWith intersectVersionRanges name own dependencies
          reasons = Map.mapWithKey (\dependency range -> Map.singleton name $
            DependencyOrigin releases (Set.toList $ Map.findWithDefault Set.empty dependency sources) range) dependencies
      pure (Map.map simplifyVersionRange $ Map.unionWith intersectVersionRanges required bounds,
        Map.unionWith Map.union reasons origins, known'')

-- Propagate only dependencies required by every remaining release. A package
-- that can stay installed does not need its existing dependencies revalidated.
-- Empty domains prove impossibility without enumerating unrelated update sets.
propagateRanges :: PlanEffects r => Bool -> Set.Set PackageName -> SearchCache -> Sem r (Maybe PlanProblem, SearchCache)
propagateRanges includeReverse requested initial = go (Map.keysSet $ searchRequiredRanges initial) Map.empty initial
  where
    go pending examined cache = case Set.minView pending of
      Nothing -> pure (Nothing, cache)
      Just (name, rest) -> do
        tracePlan $ "Propagating " <> unPackageName name <> "; " <> show (Set.size rest) <> " packages pending"
        let required = searchRequiredRanges cache Map.! name
        current <- case Map.lookup name (searchInstalled cache) of
          Just version -> pure $ Just version
          Nothing -> do
            found <- try @MyException $ currentVersion name
            case found of
              Right version -> pure $ Just version
              Left (PkgNotFound _) -> pure Nothing
              Left err -> throw err
        let withCurrent = cache {searchInstalled = maybe id (Map.insert name) current (searchInstalled cache)}
        if Set.notMember name requested && maybe False (`withinRange` required) current
          then go rest examined withCurrent
          else do
            known <- if isGHCLibs name then pure withCurrent else discoverVersions name withCurrent
            let available
                  | Set.member name requested = searchChoices known Map.! name
                  | isGHCLibs name = maybe [] pure current
                  | otherwise = maybe [] pure current <> Map.findWithDefault [] name (searchChoices known)
                inRange = filter (`withinRange` required) available
            (eligible, known') <- if includeReverse then compatibleReleases name inRange known else pure (inRange, known)
            if null eligible
              then if includeReverse
                then go rest examined $ if null inRange
                  then known' {searchConflicts = Map.insert name (UnavailableDependency name required current) (searchConflicts known')}
                  else known'
                else pure (Just $ UnavailableDependency name required current, known')
              else if Map.lookup name examined == Just eligible
                then go rest examined known'
                else do
                  (implied, origins, dependencies) <- requestedRanges (Map.singleton name eligible) (searchDependencies known')
                  let combined = Map.map simplifyVersionRange $ Map.unionWith intersectVersionRanges (searchRequiredRanges known') implied
                      forward = known' {searchDependencies = dependencies, searchRequiredRanges = combined,
                        searchRangeOrigins = Map.unionWith Map.union origins (searchRangeOrigins known')}
                  expanded <- if includeReverse then forceReverseUpdates name eligible current forward else pure forward
                  let finalRanges = searchRequiredRanges expanded
                      changed = Map.keysSet $ Map.filterWithKey
                        (\dependency range -> maybe True ((/= asVersionIntervals range) . asVersionIntervals) $ Map.lookup dependency $ searchRequiredRanges cache)
                        finalRanges
                  go (Set.union rest changed) (Map.insert name eligible examined) expanded

compatibleReleases :: PlanEffects r => PackageName -> [Version] -> SearchCache -> Sem r ([Version], SearchCache)
compatibleReleases name releases cache = do
  (existing, known) <- existingDependencies name (searchDependencies cache)
  (compatible, dependencies) <- foldM
    (\(accepted, parsed) version -> do
      (result, parsed') <- loadDependencies name version parsed
      let allowed = withinRange version $ Map.findWithDefault anyVersion name $ searchRequiredRanges cache
          fits = case result of
            Left _ -> True
            Right parts -> all
              (\(dependency, range) -> not $ null $ asVersionIntervals $ intersectVersionRanges range $ Map.findWithDefault anyVersion dependency $ searchRequiredRanges cache)
              (requiredDependencies existing parts)
      pure ([version | allowed && fits] <> accepted, parsed'))
    ([], known) releases
  pure (reverse compatible, cache {searchDependencies = dependencies})

-- If every possible target release breaks a previously satisfied repository
-- range, that reverse dependent must also update. Unrepairable dependents stay
-- in the normal search so other parts of a blocked plan can still improve.
forceReverseUpdates :: PlanEffects r => PackageName -> [Version] -> Maybe Version -> SearchCache -> Sem r SearchCache
forceReverseUpdates target eligible current initial =
  foldM force initial $ Set.toList $ Map.findWithDefault Set.empty (toArchLinuxName target) (searchReversePackages initial)
  where
    force cache archName
      | isGHCLibs (toHackageName archName) = pure cache
      | otherwise = do
          let owner = toHackageName archName
          installed <- try @MyException $ currentVersion owner
          case installed of
            Left _ -> pure cache
            Right version -> do
              (result, dependencies) <- loadDependencies owner version (searchDependencies cache)
              let known = cache {searchDependencies = dependencies}
              case result of
                Left _ -> pure known
                Right (depends, makeDepends) -> do
                  let ranges = [range | (name, range) <- depends <> makeDepends, name == target, maybe True (`withinRange` range) current]
                  if any (\candidate -> all (withinRange candidate) ranges) eligible
                    then pure known
                    else do
                      discovered <- discoverVersions owner known
                      (releases, checked) <- compatibleReleases owner (Map.findWithDefault [] owner $ searchChoices discovered) discovered
                      if null releases
                        then pure checked
                        else pure checked
                          { searchRequiredRanges = Map.insertWith (\a b -> simplifyVersionRange $ intersectVersionRanges a b) owner
                              (foldr (unionVersionRanges . thisVersion) noVersion releases) (searchRequiredRanges checked),
                            searchRangeOrigins = Map.insertWith Map.union owner (Map.singleton target $ ReverseUpdateOrigin eligible)
                              (searchRangeOrigins checked)
                          }

-- Adding a package starts at its next preferred release and costs one step,
-- just like advancing an existing target by one release.
discoverVersions :: PlanEffects r => PackageName -> SearchCache -> Sem r SearchCache
discoverVersions name cache
  | Map.member name (searchChoices cache) = pure cache
  | otherwise = do
      current <- try @MyException $ currentVersion name
      installed <- case current of
        Right version -> pure $ Just version
        Left (PkgNotFound _) -> pure Nothing
        Left err -> throw err
      newer <- try @MyException $ getNewerVersions name (maybe nullVersion id installed)
      available <- case newer of
        Right releases -> pure releases
        Left (PkgNotFound _) -> pure []
        Left err -> throw err
      tracePlan $ "Discovered " <> show (length available) <> " newer releases of " <> unPackageName name
      pure cache
        { searchInstalled = maybe id (Map.insert name) installed (searchInstalled cache),
          searchChoices = Map.insert name available (searchChoices cache)
        }

problemTargets :: PlanProblem -> [PackageName]
problemTargets = \case
  DependencyProblem owner dependency _ _ -> [owner, dependency]
  ReverseDependencyProblem owner dependency _ _ _ -> [toHackageName owner, dependency]
  UncheckedCandidate name _ _ -> [name]
  UncheckedReverseDependency owner _ _ -> [toHackageName owner]
  UncheckedCompilerTool _ _ _ -> []
  UnavailableDependency _ _ _ -> []

checkSet ::
  PlanEffects r =>
  Map.Map PackageName Version ->
  Map.Map PackageName Version ->
  ReverseChecks ->
  DependencyCache ->
  Sem r ([PlanProblem], [PlanProblem], DependencyCache)
checkSet installed selected reverseChecks cache = do
  (directProblems, directWarnings, candidateCache) <- checkCandidates False (filter ((/= "ghc") . fst) $ Map.toList selected) cache
  (repositoryProblems, repositoryWarnings, cache') <- case cachedToolchain candidateCache of
    Nothing -> pure ([], [], candidateCache)
    Just toolchain -> do
      extra <- ask @ExtraDB
      let fixed = toolchainArchPackages toolchain
          owners =
            [ (name, _version desc)
              | desc <- Map.elems extra,
                isHaskellPackage $ _name desc,
                let name = toHackageName $ _name desc,
                Map.notMember name selected,
                Set.notMember (_name desc) fixed
            ]
          unreadable = [UncheckedReverseDependency (toArchLinuxName name) ["ghc"] (VersionNoParse raw) | (name, raw) <- owners, Nothing <- [simpleParsec @Version raw]]
      (problems, warnings, known) <- checkCandidates True [(name, version) | (name, raw) <- owners, Just version <- [simpleParsec raw]] candidateCache
      pure (problems, unreadable <> warnings, known)
  let reverseProblems = concat
        [ [ ReverseDependencyProblem (reverseDepName dep) target src range (selected Map.! target)
            | dep <- deps,
              (src, range) <- reverseDepRanges dep,
              not $ withinRange (selected Map.! target) range
          ]
          | (target, deps, _) <- reverseChecks
        ]
      unchecked = Map.fromListWith
        (\(err, a) (_, b) -> (err, Set.union a b))
        [ ((skippedReverseDepName dep, show $ skippedReverseDepError dep), (skippedReverseDepError dep, Set.singleton target))
          | (target, _, skipped) <- reverseChecks,
            dep <- skipped
        ]
      unverified = [UncheckedReverseDependency name (Set.toList targets) err | ((name, _), (err, targets)) <- Map.toList unchecked]
      (existing, introduced) = partition alreadyBroken reverseProblems
      alreadyBroken (ReverseDependencyProblem _ target _ range _) =
        maybe False (not . (`withinRange` range)) $ Map.lookup target installed
      alreadyBroken _ = False
  pure (directProblems <> repositoryProblems <> introduced, directWarnings <> repositoryWarnings <> existing <> unverified, cache')
  where
    checkCandidates _ [] known = pure ([], [], known)
    checkCandidates repository ((name, version) : rest) known = do
      (dependencies, known') <- loadDependencies name version known
      (existing, known'') <- existingDependencies name known'
      compiler <- ask @Version
      let previous = Map.lookup (compiler, name, version) (cachedDependencies known'') >>= either (const Nothing) Just
          proposedCompiler = maybe compiler toolchainVersion $ cachedToolchain known''
          targets = either (const Set.empty) (Set.fromList . fmap (\(_, dependency, _) -> dependency) . tagDependencies) dependencies
          key = (proposedCompiler, name, version, repository, Map.restrictKeys selected targets)
      (failures, warnings, checked) <- case Map.lookup key $ cachedCandidateChecks known'' of
        Just (failures, warnings) -> pure (failures, warnings, known'')
        Nothing -> do
          problems <- case dependencies of
            Left err -> pure [if repository then (True, UncheckedReverseDependency (toArchLinuxName name) ["ghc"] err) else (False, UncheckedCandidate name version err)]
            Right parts -> concat <$> forM (tagDependencies parts) (\(src, dependency, range) -> do
              actual <- availableVersion known'' selected dependency
              let problem = case (repository, actual) of
                    (True, Just candidate) -> ReverseDependencyProblem (toArchLinuxName name) dependency src range candidate
                    _ -> DependencyProblem name dependency range actual
              pure $ if unknownCompilerTool known'' dependency
                then [(True, UncheckedCompilerTool name dependency range)]
                else [(dependencyWasBroken previous known'' existing src dependency range, problem) | maybe True (not . (`withinRange` range)) actual])
          let (old, introduced) = partition fst problems
              failures = snd <$> introduced
              warnings = snd <$> old
          pure (failures, warnings, known'' {cachedCandidateChecks = Map.insert key (failures, warnings) $ cachedCandidateChecks known''})
      (others, otherWarnings, finalCache) <- checkCandidates repository rest checked
      pure (failures <> others, warnings <> otherWarnings, finalCache)

loadDependencies ::
  PlanEffects r =>
  PackageName ->
  Version ->
  DependencyCache ->
  Sem r (Either MyException (VersionedList, VersionedList), DependencyCache)
loadDependencies name version known = do
  installedCompiler <- ask @Version
  let compiler = maybe installedCompiler toolchainVersion $ cachedToolchain known
  (dependencies, cache) <- loadDependenciesWith compiler name version known
  baselineCache <- if compiler == installedCompiler then pure cache else snd <$> loadDependenciesWith installedCompiler name version cache
  pure (dependencies, baselineCache)

loadDependenciesWith :: PlanEffects r => Version -> PackageName -> Version -> DependencyCache -> Sem r (Either MyException (VersionedList, VersionedList), DependencyCache)
loadDependenciesWith compiler name version known = case Map.lookup (compiler, name, version) (cachedDependencies known) of
  Just cached -> pure (cached, known)
  Nothing -> case Map.lookup (name, version) (cachedDependencyConditions known) >>= \conditions ->
    Map.lookup (name, version, fmap (withinRange compiler) conditions) (cachedDependencyVariants known) of
      Just cached -> pure (cached, known {cachedDependencies = Map.insert (compiler, name, version) cached (cachedDependencies known)})
      Nothing -> do
        tracePlan $ "Reading " <> unPackageName name <> " " <> prettyShow version <> " metadata for GHC " <> prettyShow compiler
        parsed <- try @MyException $ getCabalIncludingDeprecated name version
        let conditions = either (const []) compilerConditions parsed
        dependencies <- case parsed of
          Left err -> pure $ Left err
          Right cabal -> try @MyException $ local @Version (const compiler) $ localDependencyRecord $ directDependencies cabal
        pure (dependencies, known
          { cachedDependencies = Map.insert (compiler, name, version) dependencies (cachedDependencies known),
            cachedDependencyConditions = Map.insert (name, version) conditions (cachedDependencyConditions known),
            cachedDependencyVariants = Map.insert (name, version, fmap (withinRange compiler) conditions) dependencies (cachedDependencyVariants known)
          })

compilerConditions :: GenericPackageDescription -> [VersionRange]
compilerConditions cabal = maybe [] compilerRanges (condLibrary cabal)
  <> concatMap (compilerRanges . snd) (condSubLibraries cabal)
  <> concatMap (compilerRanges . snd) (condExecutables cabal)
  <> concatMap (compilerRanges . snd) (condTestSuites cabal)

compilerRanges :: CondTree ConfVar constraints component -> [VersionRange]
compilerRanges tree = concat
  [ [range | Impl GHC range <- toList $ condBranchCondition branch]
      <> compilerRanges (condBranchIfTrue branch)
      <> maybe [] compilerRanges (condBranchIfFalse branch)
    | branch <- condTreeComponents tree
  ]

tagDependencies :: (VersionedList, VersionedList) -> [(DepSrc, PackageName, VersionRange)]
tagDependencies (depends, makeDepends) =
  [(src, name, range) | (src, deps) <- [(Run, depends), (Make, makeDepends)], (name, range) <- deps]

requiredDependencies :: ExistingDependencies -> (VersionedList, VersionedList) -> VersionedList
requiredDependencies existing parts =
  [(name, range) | (src, name, range) <- tagDependencies parts, not $ existingDependencyFailure existing src name range]

existingDependencyFailure :: ExistingDependencies -> DepSrc -> PackageName -> VersionRange -> Bool
existingDependencyFailure existing src dependency range = case Map.lookup (src, dependency) existing of
  Just (baseline, Just installed) ->
    not (withinRange installed baseline)
      || not (null $ asVersionIntervals range)
        && null (asVersionIntervals $ intersectVersionRanges range $ orLaterVersion installed)
  Just (_, Nothing) -> True
  Nothing -> False

dependencyWasBroken :: Maybe (VersionedList, VersionedList) -> DependencyCache -> ExistingDependencies -> DepSrc -> PackageName -> VersionRange -> Bool
dependencyWasBroken previous cache existing src dependency range = case cachedToolchain cache of
  Nothing -> existingDependencyFailure existing src dependency range
  Just _
    | any (\(source, name, oldRange) -> source == src && name == dependency && asVersionIntervals oldRange == asVersionIntervals range)
        (maybe [] tagDependencies previous) -> existingDependencyFailure existing src dependency range
    | otherwise -> case Map.lookup (src, dependency) existing of
        Just (baseline, Just installed) -> not $ withinRange installed baseline
        Just (_, Nothing) -> True
        Nothing -> False

-- Compare with the installed owner's metadata and installed dependency
-- versions, never with candidates being explored in the current branch.
existingDependencies :: PlanEffects r => PackageName -> DependencyCache -> Sem r (ExistingDependencies, DependencyCache)
existingDependencies name cache = case Map.lookup name (cachedExistingDependencies cache) of
  Just existing -> pure (existing, cache)
  Nothing -> do
    current <- try @MyException $ currentVersion name
    compiler <- ask @Version
    (baseline, known) <- case current of
      Right version -> loadDependenciesWith compiler name version cache
      Left err -> pure (Left err, cache)
    dependencies <- case baseline of
      Left _ -> pure []
      Right parts -> concat <$> forM (tagDependencies parts) (\(src, dependency, range) -> do
        installed <- try @MyException $ currentVersion dependency
        pure $ case installed of
          Right version -> [((src, dependency), (range, Just version))]
          Left (PkgNotFound _) -> [((src, dependency), (range, cachedToolchain known >>= Map.lookup dependency . toolchainInstalled))]
          _ -> [])
    let existing = Map.fromListWith (\(range, installed) (other, _) -> (intersectVersionRanges range other, installed)) dependencies
    pure (existing, known {cachedExistingDependencies = Map.insert name existing (cachedExistingDependencies known)})

localDependencyRecord :: Member DependencyRecord r => Sem r a -> Sem r a
localDependencyRecord action = do
  saved <- get @(Map.Map PackageName [VersionRange])
  put @(Map.Map PackageName [VersionRange]) Map.empty
  result <- action
  put saved
  pure result

revisionOwners :: ExtraDB -> PlanResult -> [(PackageName, Version)]
revisionOwners extra plan = filter (\(name, _) -> name /= "ghc" || not (isJust $ planToolchain plan)) $ Map.toList $ Map.union (planVersions plan) $ Map.fromList
  [ (toHackageName $ _name desc, version)
    | desc <- owners,
      Just version <- [simpleParsec $ _version desc]
  ]
  where
    owners = case planToolchain plan of
      Nothing -> [desc | target <- Map.keys (planVersions plan), (desc, _) <- reverseDependencyPackages extra target]
      Just toolchain -> [desc | desc <- Map.elems extra, isHaskellPackage $ _name desc, Set.notMember (_name desc) $ toolchainArchPackages toolchain]

type RevisionRanges = Map.Map (DepSrc, PackageName) (VersionRange, String, Doc AnsiStyle)
type RevisionView = Either MyException RevisionRanges

comparePlanRevisions :: PlanEffects r => RawHackageDB -> PlanResult -> Sem r PlanResult
comparePlanRevisions original plan = do
  extra <- ask @ExtraDB
  let emptyCache = emptyDependencies {cachedToolchain = planToolchain plan}
  (notes, _, _) <- foldM compareOwner ([], emptyCache, emptyCache) (revisionOwners extra plan)
  pure plan {planRevisionNotes = reverse notes}
  where
    compareOwner (notes, latestCache, originalCache) (owner, version) = do
      (latest, latestCache') <- inspectRevision plan owner version latestCache
      (revision0, originalCache') <- local @RawHackageDB (const original) $ inspectRevision plan owner version originalCache
      pure (maybe notes (: notes) (revisionDifference owner version latest revision0), latestCache', originalCache')

inspectRevision :: PlanEffects r => PlanResult -> PackageName -> Version -> DependencyCache -> Sem r (RevisionView, DependencyCache)
inspectRevision plan owner version cache = do
  (parsed, known) <- loadDependencies owner version cache
  compiler <- ask @Version
  case parsed of
    Left err -> pure (Left err, known)
    Right parts -> do
      let candidate = Map.member owner (planVersions plan)
          ranges = Map.fromListWith intersectVersionRanges
            [ ((src, dependency), range)
              | (src, dependency, range) <- tagDependencies parts,
                candidate || isJust (planToolchain plan) || Map.member dependency (planVersions plan)
            ]
      (existing, known') <- if candidate || isJust (planToolchain plan) then existingDependencies owner known else pure (Map.empty, known)
      let previous = Map.lookup (compiler, owner, version) (cachedDependencies known') >>= either (const Nothing) Just
      checked <- forM (Map.toList ranges) $ \(key@(src, dependency), range) -> do
        actual <- try @MyException $ availableVersion known' (planVersions plan) dependency
        let old = if candidate || isJust (planToolchain plan)
              then dependencyWasBroken previous known' existing src dependency range
              else maybe False (not . (`withinRange` range)) (Map.lookup dependency $ planInstalled plan)
            (status, doc)
              | unknownCompilerTool known' dependency = ("unchecked tool", prettyWarning $ UncheckedCompilerTool owner dependency range)
              | otherwise = case actual of
                  Left err -> ("unchecked", annYellow (viaPretty range) <> line <> indent 2 (annYellow $ "unchecked:" <+> viaShow err))
                  Right selected
                    | maybe False (`withinRange` range) selected -> ("ok", annGreen $ viaPretty range <+> parens "ok")
                    | otherwise ->
                        let problem = case (candidate, selected) of
                              (False, Just chosen) -> ReverseDependencyProblem (toArchLinuxName owner) dependency src range chosen
                              _ -> DependencyProblem owner dependency range selected
                            label = (if candidate then "dep" else "rdep") <> if old then "-old" else ""
                            style = if old then annYellow else annRed
                            details = if old then prettyWarning problem else prettyProblem problem
                         in (label, style (viaPretty range) <> line <> indent 2 details)
        pure (key, (range, status, doc))
      pure (Right $ Map.fromList checked, known')

revisionDifference :: PackageName -> Version -> RevisionView -> RevisionView -> Maybe (Doc AnsiStyle)
revisionDifference owner version latest original = case (latest, original) of
  (Left _, Left _) -> Nothing
  (Right a, Right b) ->
    let changed =
          [ (key, Map.lookup key a, Map.lookup key b)
            | key <- Set.toList $ Set.union (Map.keysSet a) (Map.keysSet b),
              outcome (Map.lookup key a) /= outcome (Map.lookup key b)
          ]
     in if null changed then Nothing else Just $ vsep $
          header :
            [ indent 2 $ vsep
                [ pretty src <> colon <+> viaPretty dependency,
                  indent 2 $ revisionLine annCyan "latest revision" current,
                  indent 2 $ revisionLine annBlue "revision 0" first
                ]
              | ((src, dependency), current, first) <- changed
            ]
  _ -> Just $ vsep
    [ header,
      indent 2 $ completeRevision annCyan "latest revision" latest,
      indent 2 $ completeRevision annBlue "revision 0" original
    ]
  where
    header = annMagneta $ "Revision comparison:" <+> viaPretty owner <+> viaPretty version
    outcome = maybe "ok" (\(_, status, _) -> status)
    revisionLine style label details = style label <> colon <+> maybe "not required" (\(_, _, doc) -> doc) details
    completeRevision style label result = style label <> colon <> line <> indent 2 (case result of
      Left err -> annYellow $ "unchecked:" <+> viaShow err
      Right ranges
        | Map.null ranges -> "No relevant dependencies"
        | otherwise -> vsep [pretty src <> colon <+> viaPretty dependency <+> doc | ((src, dependency), (_, _, doc)) <- Map.toList ranges])

-- Missing repository metadata is distinct from a known incompatibility.
-- Candidate metadata remains mandatory because it defines the proposed set.
planIsReady :: PlanResult -> Bool
planIsReady = null . planProblems

prettyPlanResult :: PlanResult -> Doc AnsiStyle
prettyPlanResult result@PlanResult {..} =
  vsep $
    [ status <> colon,
      indent 2 $ vsep
        [ viaPretty name <+> maybe "not in repo" viaPretty (Map.lookup name planInstalled) <+> "->" <+> annBold (viaPretty version)
            <> (if Set.member name planRequested then mempty else space <> annCyan "(added by solver)")
          | (name, version) <- Map.toList planVersions
        ],
      "Candidate sets checked:" <+> pretty plansTried
    ]
      <> [ line <> "Bundled with GHC" <+> viaPretty (toolchainVersion toolchain) <> colon <> line
             <> indent 2 (vsep
               [ viaPretty name <+> maybe "not in repo" viaPretty installed
                   <+> "->" <+> maybe "not bundled" viaPretty proposed
                 | (name, installed, proposed) <- changes
               ])
           | Just toolchain <- [planToolchain],
             let changes =
                   [ (name, installed, proposed)
                     | name <- Set.toList $ Set.delete "ghc" $ Set.union (Map.keysSet $ toolchainInstalled toolchain) (Map.keysSet $ toolchainPackages toolchain),
                       let installed = Map.lookup name $ toolchainInstalled toolchain,
                       let proposed = Map.lookup name $ toolchainPackages toolchain,
                       installed /= proposed
                   ],
             not $ null changes
         ]
      <> (prettyProblem <$> planProblems)
      <> (prettyWarning <$> planWarnings)
      <> [line <> vsep planSearchNotes | not $ null planSearchNotes]
      <> [line <> vsep planRevisionNotes | not $ null planRevisionNotes]
      <> [ line <> "Commit message:" <> line <> pretty (intercalate ", " updates)
             <> line <> line <> ("genrebuild -H"
               <+> hsep [if name == "ghc" then "ghc" else pretty $ unArchLinuxName $ toArchLinuxName name | name <- Map.keys planVersions])
           | not $ null updates
         ]
  where
    updates =
      [ unPackageName name <> " " <> prettyShow version
        | (name, version) <- Map.toList planVersions,
          Map.lookup name planInstalled /= Just version
      ]
    status
      | not $ planIsReady result = annRed "Blocked update set"
      | any unchecked planWarnings = annYellow "Update plan ready (with unchecked packages)"
      | not $ null planWarnings = annYellow "Update plan ready (with warnings)"
      | otherwise = annGreen "Update plan ready"
    unchecked (UncheckedReverseDependency _ _ _) = True
    unchecked (UncheckedCompilerTool _ _ _) = True
    unchecked _ = False

prettySearchConflict :: Set.Set PackageName -> SearchCache -> PlanProblem -> Doc AnsiStyle
prettySearchConflict requested cache problem = case problem of
  UnavailableDependency dependency _ _ -> vsep $ prettyProblem problem :
    [ indent 2 $ vsep $ prettyOrigin dependency owner origin :
        [ "Required through:" <+> prettyPath (path <> [dependency])
          | let path = requiredThrough owner, not $ null path
        ]
      | (owner, origin) <- Map.toList $ contributingOrigins dependency
    ]
  _ -> prettyProblem problem
  where
    origins = searchRangeOrigins cache
    contributingOrigins dependency = foldl' removeRedundant available $ Map.keys available
      where
        available = Map.findWithDefault Map.empty dependency origins
        bounds reasons = asVersionIntervals $ foldr intersectVersionRanges anyVersion
          [range | DependencyOrigin _ _ range <- Map.elems reasons]
        removeRedundant reasons owner = case Map.lookup owner reasons of
          Just (DependencyOrigin _ _ _)
            | Map.size reasons > 1,
              let remaining = Map.delete owner reasons,
              bounds remaining == bounds reasons -> Map.delete owner reasons
          _ -> reasons
    prettyOwner owner [release] = viaPretty owner <+> viaPretty release
    prettyOwner owner releases = viaPretty owner <+> parens (pretty (length releases) <+> "eligible releases")
    prettyOrigin dependency owner (DependencyOrigin releases sources range) =
      prettyOwner owner releases <+> hcat (punctuate "/" $ pretty <$> sources)
        <+> "requires" <+> viaPretty dependency <+> viaPretty range
    prettyOrigin dependency owner (ReverseUpdateOrigin releases) =
      prettyOwner owner releases <+> "forces" <+> viaPretty dependency <+> "to update (reverse dependency)"
    requiredThrough owner = go Set.empty $ Map.singleton owner [owner]
      where
        go visited pending
          | Just (_, path) <- Map.lookupMin $ Map.restrictKeys pending requested = path
          | Map.null pending = []
          | otherwise =
              let seen = Set.union visited $ Map.keysSet pending
                  next = Map.fromList
                    [ (parent, parent : path)
                      | (dependency, path) <- Map.toList pending,
                        parent <- Map.keys $ Map.findWithDefault Map.empty dependency origins,
                        Set.notMember parent seen
                    ]
               in go seen next
    prettyPath [] = mempty
    prettyPath (first : rest) = foldl' append (viaPretty first) $ zip (first : rest) rest
      where
        append path (parent, dependency) = path <+> arrow <+> viaPretty dependency
          where
            arrow = case Map.lookup dependency origins >>= Map.lookup parent of
              Just (ReverseUpdateOrigin _) -> "-[reverse dependency]->"
              _ -> "->"

prettyProblem :: PlanProblem -> Doc AnsiStyle
prettyProblem = \case
  DependencyProblem owner dependency range actual ->
    prettyDependency (annRed "dep:") owner dependency range actual
  ReverseDependencyProblem owner dependency src range version ->
    prettyReverseDependency (annRed "rdep:") owner dependency src range version
  UncheckedCandidate name version err ->
    annYellow "unchecked:" <+> viaPretty name <+> viaPretty version <> colon <+> viaShow err
  UncheckedReverseDependency name targets err ->
    annYellow "unchecked rdep:" <+> pretty (unArchLinuxName name) <+> "for" <+> hsep (punctuate comma $ viaPretty <$> targets) <> colon <+> viaShow err
  UncheckedCompilerTool owner tool range ->
    annYellow "unchecked compiler tool:" <+> viaPretty owner <+> "requires" <+> viaPretty tool <+> viaPretty range
      <> comma <+> "upstream bundled-library metadata does not specify its version"
  UnavailableDependency name range (Just current) | isGHCLibs name ->
    annRed "dep:" <+> viaPretty name <+> "is fixed at" <+> viaPretty current <+> "by the installed GHC"
      <> comma <+> "but the required range is" <+> viaPretty range
  UnavailableDependency name range current ->
    annRed "dep:" <+> "no installed or newer preferred version of" <+> viaPretty name <+> "satisfies" <+> viaPretty range
      <> maybe mempty (\version -> comma <+> "repository version is" <+> viaPretty version) current

prettyWarning :: PlanProblem -> Doc AnsiStyle
prettyWarning (DependencyProblem owner dependency range actual) =
  annYellow $ prettyDependency "dep-old:" owner dependency range actual
prettyWarning (ReverseDependencyProblem owner dependency src range version) =
  annYellow $ prettyReverseDependency "rdep-old:" owner dependency src range version
prettyWarning problem = prettyProblem problem

prettyDependency :: Doc AnsiStyle -> PackageName -> PackageName -> VersionRange -> Maybe Version -> Doc AnsiStyle
prettyDependency label owner dependency range actual =
  label <+> viaPretty owner <+> "requires" <+> viaPretty dependency <+> viaPretty range
    <> comma <+> maybe "missing from the update set and repository" (\version -> "selected/repository version is" <+> viaPretty version) actual

prettyReverseDependency :: Doc AnsiStyle -> ArchLinuxName -> PackageName -> DepSrc -> VersionRange -> Version -> Doc AnsiStyle
prettyReverseDependency label owner dependency src range version =
  label <+> pretty (unArchLinuxName owner) <+> pretty src <+> "requires" <+> viaPretty dependency <+> viaPretty range
    <> comma <+> "selected version is" <+> viaPretty version
