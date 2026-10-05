module Plan.Solver (Score, Model (..), optimize, optimizeBelow) where

import Control.Applicative ((<|>))
import Control.Monad (foldM)
import qualified Data.IntMap.Strict as IntMap
import Data.List (foldl', sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Ord (Down (..))
import qualified Data.Set as Set

type Score = (Int, Int)
type Table = IntMap.IntMap (IntMap.IntMap Score)

data Model key = Model
  { modelConstant :: Score,
    modelDomains :: Map.Map key (IntMap.IntMap Score),
    modelEdges :: Map.Map (key, key) Table
  }

data Elimination key
  = Fixed key Int
  | Leaf key key (IntMap.IntMap Int)

add :: Score -> Score -> Score
add (failures, steps) (otherFailures, otherSteps) = (failures + otherFailures, steps + otherSteps)

subtractScore :: Score -> Score -> Score
subtractScore (failures, steps) (otherFailures, otherSteps) = (failures - otherFailures, steps - otherSteps)

entry :: Table -> Int -> Int -> Score
entry table left right = table IntMap.! left IntMap.! right

evaluate :: Ord key => Model key -> Map.Map key Int -> Score
evaluate model assignment = foldl' add (modelConstant model) $
  [values IntMap.! (assignment Map.! name) | (name, values) <- Map.toList $ modelDomains model] <>
  [entry table (assignment Map.! left) (assignment Map.! right) | ((left, right), table) <- Map.toList $ modelEdges model]

restore :: Ord key => [Elimination key] -> Map.Map key Int -> Map.Map key Int
restore eliminated assignment = foldl' insert assignment eliminated
  where
    insert known (Fixed name value) = Map.insert name value known
    insert known (Leaf name neighbor choices) = Map.insert name (choices IntMap.! (known Map.! neighbor)) known

restrictTables :: Ord key => Model key -> Model key
restrictTables model = model {modelEdges = Map.mapWithKey restrict $ modelEdges model}
  where
    restrict (left, right) table =
      IntMap.map (\row -> IntMap.restrictKeys row $ IntMap.keysSet $ modelDomains model Map.! right) $
        IntMap.restrictKeys table $ IntMap.keysSet $ modelDomains model Map.! left

project :: Ord key => Model key -> Model key
project initial = normalize $ foldl' shift initial $ Map.keys $ modelEdges initial
  where
    shift model key@(left, right) =
      let table = modelEdges model Map.! key
          rows = IntMap.map (minimum . IntMap.elems) table
          reduced = IntMap.mapWithKey (\value -> IntMap.map (`subtractScore` (rows IntMap.! value))) table
          columns = IntMap.fromList
            [(value, minimum [row IntMap.! value | row <- IntMap.elems reduced])
              | value <- IntMap.keys $ modelDomains model Map.! right]
          finalTable = IntMap.map (IntMap.mapWithKey (\value cost -> subtractScore cost $ columns IntMap.! value)) reduced
          domains = Map.adjust (IntMap.unionWith add rows) left $ Map.adjust (IntMap.unionWith add columns) right $ modelDomains model
       in model {modelDomains = domains, modelEdges = Map.insert key finalTable $ modelEdges model}
    normalize model =
      let minima = Map.map (minimum . IntMap.elems) $ modelDomains model
       in model
            { modelConstant = foldl' add (modelConstant model) $ Map.elems minima,
              modelDomains = Map.mapWithKey (\name -> IntMap.map (`subtractScore` (minima Map.! name))) $ modelDomains model,
              modelEdges = Map.filter (any (/= (0, 0)) . concatMap IntMap.elems . IntMap.elems) $ modelEdges model
            }

neighbors :: Ord key => Model key -> Map.Map key [key]
neighbors model = Map.fromListWith (<>) $
  concat [[(left, [right]), (right, [left])] | (left, right) <- Map.keys $ modelEdges model]

equivalentDomains :: Ord key => Model key -> Map.Map key (IntMap.IntMap Score)
equivalentDomains model = Map.mapWithKey reduce $ modelDomains model
  where
    adjacent = neighbors model
    reduce name values = case Map.findWithDefault [] name adjacent of
      [] -> values
      related ->
        let signature value = concat
              [ [if name < neighbor then entry table value other else entry table other value
                  | other <- IntMap.keys $ modelDomains model Map.! neighbor]
                | neighbor <- related,
                  let key = if name < neighbor then (name, neighbor) else (neighbor, name),
                  let table = modelEdges model Map.! key
              ]
            representatives = Map.fromListWith min
              [(signature value, (cost, value)) | (value, cost) <- IntMap.toList values]
         in IntMap.fromList [(value, cost) | (cost, value) <- Map.elems representatives]

fixValue :: Ord key => key -> Int -> Model key -> Model key
fixValue name value model = foldl' fixEdge without $ Map.toList $ modelEdges model
  where
    without = model
      { modelConstant = add (modelConstant model) $ modelDomains model Map.! name IntMap.! value,
        modelDomains = Map.delete name $ modelDomains model,
        modelEdges = Map.filterWithKey (\(left, right) _ -> left /= name && right /= name) $ modelEdges model
      }
    fixEdge known ((left, right), table)
      | left == name = known {modelDomains = Map.adjust (IntMap.unionWith add $ table IntMap.! value) right $ modelDomains known}
      | right == name = known {modelDomains = Map.adjust (IntMap.unionWith add $ IntMap.map (IntMap.! value) table) left $ modelDomains known}
      | otherwise = known

eliminateLeaf :: Ord key => key -> key -> Model key -> (Model key, Elimination key)
eliminateLeaf name neighbor model =
  let values = modelDomains model Map.! name
      key = if name < neighbor then (name, neighbor) else (neighbor, name)
      table = modelEdges model Map.! key
      cost own other = if name < neighbor then entry table own other else entry table other own
      choices = IntMap.mapWithKey
        (\other _ -> minimum [(add unary $ cost own other, own) | (own, unary) <- IntMap.toList values])
        (modelDomains model Map.! neighbor)
      reduced = model
        { modelDomains = Map.adjust (IntMap.unionWith add $ fst <$> choices) neighbor $ Map.delete name $ modelDomains model,
          modelEdges = Map.delete key $ modelEdges model
        }
   in (reduced, Leaf name neighbor $ snd <$> choices)

simplify :: Ord key => Maybe Score -> Model key -> Maybe (Model key, [Elimination key])
simplify cutoff = go []
  where
    go eliminated initial =
      let model = project $ restrictTables initial
          adjacent = neighbors model
          independent = [(name, values) | (name, values) <- Map.toList $ modelDomains model,
            IntMap.size values == 1 || Map.notMember name adjacent]
          leaves = [(name, neighbor) | (name, [neighbor]) <- Map.toList adjacent]
          permitted = Map.map (IntMap.filter (\cost -> maybe True (add (modelConstant model) cost <) cutoff)) $ modelDomains model
          allowed = equivalentDomains model {modelDomains = permitted}
       in if maybe False (modelConstant model >=) cutoff || any IntMap.null (Map.elems allowed)
            then Nothing
            else if Map.map IntMap.size allowed /= Map.map IntMap.size (modelDomains model)
              then go eliminated model {modelDomains = allowed}
              else case independent of
                _ : _ ->
                  let fixed = [(name, snd $ minimum [(cost, candidate) | (candidate, cost) <- IntMap.toList values])
                        | (name, values) <- independent]
                      reduced = foldl' (\known (name, value) -> fixValue name value known) model fixed
                   in go (reverse [Fixed name value | (name, value) <- fixed] <> eliminated) reduced
                [] -> case leaves of
                  (name, neighbor) : _ ->
                    let (reduced, removed) = eliminateLeaf name neighbor model
                     in go (removed : eliminated) reduced
                  [] -> Just (model, eliminated)

components :: Ord key => Model key -> [Set.Set key]
components model = go (Map.keysSet $ modelDomains model) []
  where
    adjacent = neighbors model
    connected pending found = case Set.minView pending of
      Nothing -> found
      Just (name, rest)
        | Set.member name found -> connected rest found
        | otherwise -> connected (Set.union rest $ Set.fromList $ Map.findWithDefault [] name adjacent) $ Set.insert name found
    go pending found = case Set.minView pending of
      Nothing -> found
      Just (name, _) ->
        let group = connected (Set.singleton name) Set.empty
         in go (Set.difference pending group) (group : found)

optimize :: (Monad monad, Ord key, Show key) => (String -> monad ()) -> Model key -> monad (Score, Map.Map key Int, Int)
optimize debug original = do
  (result, tried) <- optimizeBelow debug Nothing original
  case result of
    Just (score, assignment) -> pure (score, assignment, tried)
    Nothing -> error "unbounded constraint optimization always has an assignment"

optimizeBelow :: (Monad monad, Ord key, Show key) => (String -> monad ()) -> Maybe Score -> Model key -> monad (Maybe (Score, Map.Map key Int), Int)
optimizeBelow debug upperBound original = do
  let initial = Map.map (fst . IntMap.findMin) $ modelDomains original
      initialScore = evaluate original initial
      limit = Just $ maybe initialScore (min initialScore) upperBound
      fallback = if maybe True (initialScore <) upperBound then Just (initialScore, initial) else Nothing
  (improved, tried) <- search limit original
  pure (improved <|> fallback, tried)
  where
    search cutoff initial = case simplify cutoff initial of
      Nothing -> pure (Nothing, 1)
      Just (model, eliminated)
        | Map.null $ modelDomains model -> pure (Just (modelConstant model, restore eliminated Map.empty), 1)
        | groups@(_ : _ : _) <- components model -> do
            (result, tried) <- foldM (component model cutoff) (Just (modelConstant model, Map.empty), 1) groups
            pure ((\(score, assignment) -> (score, restore eliminated assignment)) <$> result, tried)
        | otherwise -> do
            let adjacent = neighbors model
                (_, _, name) = minimum
                  [(Down $ length $ Map.findWithDefault [] candidate adjacent, IntMap.size values, candidate)
                    | (candidate, values) <- Map.toList $ modelDomains model]
                candidates = sortOn (\(value, cost) -> (cost, value)) $ IntMap.toList $ modelDomains model Map.! name
            debug $ "Constraint branch: " <> show name <> ", " <> show (length candidates) <> " choices; lower bound " <> show (modelConstant model)
            (best, tried) <- foldM (branch model name cutoff) (Nothing, 1) candidates
            pure ((\(score, assignment) -> (score, restore eliminated assignment)) <$> best, tried)

    branch model name cutoff (best, tried) (value, _) = do
      let limit = maybe cutoff (Just . fst) best
      (result, count) <- search limit $ fixValue name value model
      pure ((\(score, assignment) -> (score, Map.insert name value assignment)) <$> result <|> best, tried + count)

    component _ _ (Nothing, tried) _ = pure (Nothing, tried)
    component model cutoff (Just (score, assignment), tried) group = do
      let isolated = Model (0, 0) (Map.restrictKeys (modelDomains model) group)
            (Map.filterWithKey (\(left, _) _ -> Set.member left group) $ modelEdges model)
          initial = Map.map (fst . IntMap.findMin) $ modelDomains isolated
          initialScore = evaluate isolated initial
      (result, count) <- search (Just initialScore) isolated
      let (cost, selected) = fromMaybe (initialScore, initial) result
          combined = add score cost
      pure (if maybe False (combined >=) cutoff then Nothing else Just (combined, Map.union assignment selected), tried + count)
