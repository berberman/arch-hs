{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}

module Plan (PlanResult (..), PlanProblem (..), planUpdates, planIsReady, prettyPlanResult, comparePlanRevisions) where

import Control.Monad (foldM, forM)
import Data.List (foldl', partition, sortOn)
import qualified Data.Map.Strict as Map
import Data.Ord (Down (..))
import qualified Data.Set as Set
import Distribution.ArchHs.DepCheck (VersionedList, directDependencies)
import Distribution.ArchHs.Exception
import Distribution.ArchHs.ExtraDB (versionInExtra)
import Distribution.ArchHs.Hackage
import Distribution.ArchHs.Internal.Prelude
import Distribution.ArchHs.Name (isGHCLibs, isHaskellPackage, toArchLinuxName, toHackageName)
import Distribution.ArchHs.PP
import Distribution.ArchHs.RDepCheck
import Distribution.ArchHs.Types
import Distribution.Version (asVersionIntervals, simplifyVersionRange)

type PlanEffects r =
  Members
    '[ExtraEnv, HackageEnv, RawHackageEnv, KnownGHCVersion, FlagAssignmentsEnv, Trace, DependencyRecord, WithMyErr, Embed IO]
    r

data PlanProblem
  = DependencyProblem PackageName PackageName VersionRange (Maybe Version)
  | ReverseDependencyProblem ArchLinuxName PackageName DepSrc VersionRange Version
  | UncheckedCandidate PackageName Version MyException
  | UncheckedReverseDependency ArchLinuxName [PackageName] MyException
  | UnavailableDependency PackageName VersionRange (Maybe Version)

data PlanResult = PlanResult
  { planInstalled :: Map.Map PackageName Version,
    planVersions :: Map.Map PackageName Version,
    planRequested :: Set.Set PackageName,
    planProblems :: [PlanProblem],
    planWarnings :: [PlanProblem],
    plansTried :: Int,
    planRevisionNotes :: [Doc AnsiStyle],
    planSearchNotes :: [Doc AnsiStyle]
  }

-- Exact requests are checked as given; solving can advance from each minimum.
planUpdates :: PlanEffects r => Bool -> [(PackageName, Maybe Version)] -> Sem r (Either String PlanResult)
planUpdates solve targets
  | null targets = pure $ Left "At least one target is required."
  | length names /= Set.size (Set.fromList names) = pure $ Left "Each target must be specified only once."
  | any isGHCLibs names = pure $ Left "GHC and its bundled libraries cannot be updated independently; this planner uses the installed toolchain."
  | otherwise = do
      installed <- Map.fromList <$> forM names (\name -> (name,) <$> currentVersion name)
      choices <- forM targets $ \(name, requested) -> do
        newer <- if solve || requested == Nothing then getNewerVersions name (installed Map.! name) else pure []
        pure $ case requested of
          Just version
            | version < installed Map.! name -> Left $ "Downgrades are not supported: " <> unPackageName name
            | otherwise -> Right (name, version : [v | solve, v <- newer, v > version])
          Nothing -> case newer of
            [] -> Left $ "No newer preferred version is available for " <> unPackageName name
            first : rest -> Right (name, first : [v | solve, v <- rest])
      case sequence choices of
        Left err -> pure $ Left err
        Right options -> Right <$> search solve installed (Map.fromList options)
  where
    names = fst <$> targets

currentVersion :: Members '[ExtraEnv, WithMyErr] r => PackageName -> Sem r Version
currentVersion name = do
  raw <- versionInExtra name
  maybe (throw $ VersionNoParse raw) pure $ simpleParsec raw

data DependencyCache = DependencyCache
  { cachedDependencies :: Map.Map (PackageName, Version) (Either MyException (VersionedList, VersionedList)),
    cachedExistingDependencies :: Map.Map PackageName ExistingDependencies
  }
type ExistingDependencies = Map.Map (DepSrc, PackageName) (VersionRange, Maybe Version)
type ReverseChecks = [(PackageName, [ReverseDep], [SkippedReverseDep])]

data SearchCache = SearchCache
  { searchInstalled :: Map.Map PackageName Version,
    searchChoices :: Map.Map PackageName [Version],
    searchDependencies :: DependencyCache,
    searchReverseChecks :: Map.Map (Set.Set PackageName) ReverseChecks,
    searchReversePackages :: Map.Map ArchLinuxName (Set.Set ArchLinuxName),
    searchRetainable :: Map.Map (PackageName, Int, PackageName, Maybe Version) Bool,
    searchRequiredRanges :: Map.Map PackageName VersionRange,
    searchConflicts :: Map.Map PackageName PlanProblem
  }

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
  Bool ->
  Map.Map PackageName Version ->
  Map.Map PackageName [Version] ->
  Sem r PlanResult
search solve installed choices = do
  extra <- ask @ExtraDB
  let reversePackages = Map.fromListWith Set.union
        [ (_pdName dependency, Set.singleton $ _name desc)
          | desc <- Map.elems extra,
            isHaskellPackage $ _name desc,
            dependency <- _depends desc <> _makeDepends desc <> _checkDepends desc
        ]
  (requiredRanges, dependencies) <- requestedRanges choices (DependencyCache Map.empty Map.empty)
  let initial = SearchCache installed choices dependencies Map.empty reversePackages Map.empty requiredRanges Map.empty
  (conflict, directCache) <- if solve then propagateRanges False (Map.keysSet choices) initial else pure (Nothing, initial)
  cache <- case conflict of
    Nothing | solve -> snd <$> propagateRanges True (Map.keysSet choices) directCache
    _ -> pure directCache
  case conflict of
    Nothing -> go
      (Map.singleton (0 :: Int, Down (0 :: Int), start) Nothing)
      (Set.singleton start)
      cache 0 Nothing
    Just problem -> do
      let selected = versions choices start
      (reverseChecks, known) <- externalChecks (Map.keys selected) reversePackages (searchDependencies cache)
      (problems, warnings, _) <- checkSet installed selected reverseChecks known
      pure $ PlanResult installed selected (Map.keysSet choices) (problem : problems) warnings 1 [] []
  where
    start = Map.map (const 0) choices
    versions catalog indices = Map.mapWithKey (\name index -> catalog Map.! name !! index) indices

    go queue visited cache tried best =
      case Map.minViewWithKey queue of
        Nothing -> case best of
          Just (_, result) -> finish tried cache result
          Nothing -> error "planner search starts with one candidate set"
        Just (((estimate, Down cost, _), Just (result, neighbors)), remaining) ->
          continue estimate cost remaining visited cache tried best result neighbors
        Just (((estimate, Down cost, indices), Nothing), remaining) -> do
          let selected = versions (searchChoices cache) indices
              targets = Map.keysSet selected
          (reverseChecks, dependencies) <- case Map.lookup targets (searchReverseChecks cache) of
            Just cached -> pure (cached, searchDependencies cache)
            Nothing -> externalChecks (Map.keys selected) (searchReversePackages cache) (searchDependencies cache)
          (problems, warnings, dependencies') <- checkSet (searchInstalled cache) selected reverseChecks dependencies
          let cache' = cache
                { searchDependencies = dependencies',
                  searchReverseChecks = Map.insert targets reverseChecks (searchReverseChecks cache)
                }
              result = PlanResult
                { planInstalled = Map.filterWithKey (\name _ -> Map.member name selected) (searchInstalled cache),
                  planVersions = selected,
                  planRequested = Map.keysSet choices,
                  planProblems = problems,
                  planWarnings = warnings,
                  plansTried = tried + 1,
                  planRevisionNotes = [],
                  planSearchNotes = []
                }
              best' = case best of
                Just (previousCost, previous)
                  | (length (planProblems previous), previousCost) <= (length problems, cost) -> best
                _ -> Just (cost, result)
          case problems of
            [] -> pure result
            -- Once propagation proves the whole update impossible, report the
            -- checked starting set and the conflict. Optimizing partial sets
            -- cannot produce a working plan and can grow exponentially.
            _ | not $ Map.null $ searchConflicts cache' -> finish (tried + 1) cache' result
            _ -> do
              (neighbors, cache'') <- foldM (advance indices) ([], cache') (nub $ concatMap problemTargets problems)
              let movable = Set.fromList $ fst <$> neighbors
              (repairs, cache''') <- repairChoices movable indices problems cache''
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
              -- Refine a queued lower bound before expanding the node. Retain
              -- its evaluation so reordering does not repeat metadata checks.
              if priority > estimate
                then go
                  (Map.insert (priority, Down cost, indices) (Just (result, next)) remaining)
                  visited cache''' (tried + 1) best'
                else continue priority cost remaining visited cache''' (tried + 1) best' result next

    finish tried cache result = pure result
      { plansTried = tried,
        planSearchNotes =
          [ vsep $ "The required updates cannot all be satisfied:" : (indent 2 . prettyProblem <$> Map.elems (searchConflicts cache))
            | not $ Map.null $ searchConflicts cache
          ]
      }

    continue estimate cost remaining visited cache tried best result neighbors
      | null $ planProblems result = pure result {plansTried = tried}
      | otherwise =
          let unseen = filter (`Set.notMember` visited) neighbors
              -- One release step can reduce the remaining cost by at most one.
              priority = max estimate (cost + 1)
           in go
                (foldr (\indices -> Map.insert (priority, Down (cost + 1), indices) Nothing) remaining unseen)
                (foldr Set.insert visited unseen)
                cache tried best

    advance indices (neighbors, cache) name =
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
requestedRanges :: PlanEffects r => Map.Map PackageName [Version] -> DependencyCache -> Sem r (Map.Map PackageName VersionRange, DependencyCache)
requestedRanges choices cache = foldM collect (Map.empty, cache) (Map.toList choices)
  where
    collect (required, known) (name, releases) = do
      (existing, known') <- existingDependencies name known
      (common, known'') <- foldM
        (\(previous, parsed) release -> do
          (result, parsed') <- loadDependencies name release parsed
          let bounds = case result of
                Left _ -> Map.empty
                Right parts -> Map.fromListWith intersectVersionRanges $ requiredDependencies existing parts
          pure (Just $ maybe bounds (Map.intersectionWith unionVersionRanges bounds) previous, parsed'))
        (Nothing, known')
        releases
      let own = foldr (unionVersionRanges . thisVersion) noVersion releases
          bounds = Map.insertWith intersectVersionRanges name own (maybe Map.empty id common)
      pure (Map.map simplifyVersionRange $ Map.unionWith intersectVersionRanges required bounds, known'')

-- Propagate only dependencies required by every remaining release. A package
-- that can stay installed does not need its existing dependencies revalidated.
-- Empty domains prove impossibility without enumerating unrelated update sets.
propagateRanges :: PlanEffects r => Bool -> Set.Set PackageName -> SearchCache -> Sem r (Maybe PlanProblem, SearchCache)
propagateRanges includeReverse requested initial = go (Map.keysSet $ searchRequiredRanges initial) Map.empty initial
  where
    go pending examined cache = case Set.minView pending of
      Nothing -> pure (Nothing, cache)
      Just (name, rest) -> do
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
                  (implied, dependencies) <- requestedRanges (Map.singleton name eligible) (searchDependencies known')
                  let combined = Map.map simplifyVersionRange $ Map.unionWith intersectVersionRanges (searchRequiredRanges known') implied
                      forward = known' {searchDependencies = dependencies, searchRequiredRanges = combined}
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
                              (foldr (unionVersionRanges . thisVersion) noVersion releases) (searchRequiredRanges checked)
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
  UnavailableDependency _ _ _ -> []

checkSet ::
  PlanEffects r =>
  Map.Map PackageName Version ->
  Map.Map PackageName Version ->
  ReverseChecks ->
  DependencyCache ->
  Sem r ([PlanProblem], [PlanProblem], DependencyCache)
checkSet installed selected reverseChecks cache = do
  (directProblems, directWarnings, cache') <- checkCandidates (Map.toList selected) cache
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
  pure (directProblems <> introduced, directWarnings <> existing <> unverified, cache')
  where
    checkCandidates [] known = pure ([], [], known)
    checkCandidates ((name, version) : rest) known = do
      (dependencies, known') <- loadDependencies name version known
      (existing, known'') <- existingDependencies name known'
      problems <- case dependencies of
        Left err -> pure [(False, UncheckedCandidate name version err)]
        Right parts -> concat <$> forM (tagDependencies parts) (\(src, dependency, range) -> do
          actual <- case Map.lookup dependency selected of
            Just candidate -> pure $ Just candidate
            Nothing -> do
              found <- try @MyException $ currentVersion dependency
              case found of
                Right current -> pure $ Just current
                Left (PkgNotFound _) -> pure Nothing
                Left err -> throw err
          pure [(existingDependencyFailure existing src dependency range, DependencyProblem name dependency range actual) | maybe True (not . (`withinRange` range)) actual])
      let (warnings, failures) = partition fst problems
      (others, otherWarnings, finalCache) <- checkCandidates rest known''
      pure ((snd <$> failures) <> others, (snd <$> warnings) <> otherWarnings, finalCache)

loadDependencies ::
  PlanEffects r =>
  PackageName ->
  Version ->
  DependencyCache ->
  Sem r (Either MyException (VersionedList, VersionedList), DependencyCache)
loadDependencies name version known = case Map.lookup (name, version) (cachedDependencies known) of
  Just cached -> pure (cached, known)
  Nothing -> do
    dependencies <- try @MyException $ do
      cabal <- getCabalIncludingDeprecated name version
      -- Candidate exploration must not accumulate dependency records.
      localDependencyRecord $ directDependencies cabal
    pure (dependencies, known {cachedDependencies = Map.insert (name, version) dependencies (cachedDependencies known)})

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

-- Compare with the installed owner's metadata and installed dependency
-- versions, never with candidates being explored in the current branch.
existingDependencies :: PlanEffects r => PackageName -> DependencyCache -> Sem r (ExistingDependencies, DependencyCache)
existingDependencies name cache = case Map.lookup name (cachedExistingDependencies cache) of
  Just existing -> pure (existing, cache)
  Nothing -> do
    current <- try @MyException $ currentVersion name
    (baseline, known) <- case current of
      Right version -> loadDependencies name version cache
      Left err -> pure (Left err, cache)
    dependencies <- case baseline of
      Left _ -> pure []
      Right parts -> concat <$> forM (tagDependencies parts) (\(src, dependency, range) -> do
        installed <- try @MyException $ currentVersion dependency
        pure $ case installed of
          Right version -> [((src, dependency), (range, Just version))]
          Left (PkgNotFound _) -> [((src, dependency), (range, Nothing))]
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
revisionOwners extra plan = Map.toList $ Map.union (planVersions plan) $ Map.fromList
  [ (toHackageName $ _name desc, version)
    | target <- Map.keys (planVersions plan),
      (desc, _) <- reverseDependencyPackages extra target,
      Just version <- [simpleParsec $ _version desc]
  ]

type RevisionRanges = Map.Map (DepSrc, PackageName) (VersionRange, String, Doc AnsiStyle)
type RevisionView = Either MyException RevisionRanges

comparePlanRevisions :: PlanEffects r => RawHackageDB -> PlanResult -> Sem r PlanResult
comparePlanRevisions original plan = do
  extra <- ask @ExtraDB
  let emptyCache = DependencyCache Map.empty Map.empty
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
  case parsed of
    Left err -> pure (Left err, known)
    Right parts -> do
      let candidate = Map.member owner (planVersions plan)
          ranges = Map.fromListWith intersectVersionRanges
            [ ((src, dependency), range)
              | (src, dependency, range) <- tagDependencies parts,
                candidate || Map.member dependency (planVersions plan)
            ]
      (existing, known') <- if candidate then existingDependencies owner known else pure (Map.empty, known)
      checked <- forM (Map.toList ranges) $ \(key@(src, dependency), range) -> do
        actual <- case Map.lookup dependency (planVersions plan) of
          Just selected -> pure $ Right $ Just selected
          Nothing -> do
            installed <- try @MyException $ currentVersion dependency
            pure $ case installed of
              Right selected -> Right $ Just selected
              Left (PkgNotFound _) -> Right Nothing
              Left err -> Left err
        let old = if candidate
              then existingDependencyFailure existing src dependency range
              else maybe False (not . (`withinRange` range)) (Map.lookup dependency $ planInstalled plan)
            (status, doc) = case actual of
              Left err -> ("unchecked: " <> show err, annYellow (viaPretty range) <> line <> indent 2 (annYellow $ "unchecked:" <+> viaShow err))
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
  (Left a, Left b) | show a == show b -> Nothing
  (Right a, Right b) ->
    let changed =
          [ (key, Map.lookup key a, Map.lookup key b)
            | key <- Set.toList $ Set.union (Map.keysSet a) (Map.keysSet b),
              not $ sameRange (Map.lookup key a) (Map.lookup key b)
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
    sameRange Nothing Nothing = True
    sameRange (Just (a, statusA, _)) (Just (b, statusB, _)) = asVersionIntervals a == asVersionIntervals b && statusA == statusB
    sameRange _ _ = False
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
      <> (prettyProblem <$> planProblems)
      <> (prettyWarning <$> planWarnings)
      <> [line <> vsep planSearchNotes | not $ null planSearchNotes]
      <> [line <> vsep planRevisionNotes | not $ null planRevisionNotes]
      <> [line <> "Commit message:" <> line <> pretty (intercalate ", " updates) | not $ null updates]
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
    unchecked _ = False

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
