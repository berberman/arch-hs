{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import qualified Algebra.Graph.AdjacencyMap.Algorithm as G
import qualified Algebra.Graph.Labelled.AdjacencyMap as G
import Community
import Conduit
import Control.Monad (filterM, when)
import Core
import Data.List (groupBy)
import qualified Data.Map.Strict as Map
import qualified Data.Set as S
import Distribution.Hackage.DB (HackageDB)
import Distribution.Types.PackageName
import Hackage
import Lens.Micro
import Local
import PkgBuild
import System.Environment
import Types

-- runProgram :: Members [CommunityEnv,HackageEnv,WithMyErr] r => CommunityDB -> HackageDB -> Sem r a -> Either MyException a
-- runProgram community hackage = runReader @HackageDB hackage. runReader @CommunityDB community

h :: Members '[Embed IO, CommunityEnv, HackageEnv, FlagAssignmentEnv, WithMyErr] r => String -> Sem r ()
h name = do
  let target = mkPackageName name
  deps <- getDependencies S.empty 0 target
  exist <- isInCommunity target
  when exist $ throw $ TargetExist target
  liftIO $ putStrLn . prettyDeps $ G.reachable target $ G.skeleton deps

  let d = groupDeps $ deps
      v = S.toList $ S.fromList (d ^.. each . pkgName ++ d ^.. each . pkgDeps . each . depName)
  providedList <- fmap (++ ghcLibList) $ filterM (isInCommunity) v
  let q = d <&> (\x -> if (x ^. pkgName) `elem` providedList then ProvidedPackage (x ^. pkgName) ByCommunity else x)
      r = q <&> pkgDeps %~ each %~ (\y -> if y ^. depName `elem` providedList then y & depProvider .~ (Just ByCommunity) else y)
      s = r ^.. each . filtered (\case ProvidedPackage {..} -> False; _ -> True)
  liftIO $ putStrLn . prettySolvedPkgs $ r
  _ <- mapM (\solved -> (liftIO . writeFile ("/home/berberman/Desktop/test/" <> (solved ^. pkgName & unPackageName) <> ".PKGBUILD") . applyTemplate) =<< cabalToPkgBuild solved) s
  return ()

runH ::
  HackageDB ->
  CommunityDB ->
  Sem '[CommunityEnv, HackageEnv, FlagAssignmentEnv, WithMyErr, Embed IO, Final IO] a ->
  IO (Either MyException a)
runH hackage community =
  runFinal
    . embedToFinal
    . errorToIOFinal
    . runReader (Map.empty)
    . runReader hackage
    . runReader community

main :: IO ()
main = do
  name <- head <$> getArgs
  hackage <- defaultHackageDB
  community <- defaultCommunity
  runH hackage community (h name) >>= print

groupDeps :: G.AdjacencyMap (S.Set DependencyType) PackageName -> [SolvedPackage]
groupDeps =
  fmap (\(name, deps) -> SolvedPackage name $ fmap (uncurry . flip $ SolvedDependency Nothing) deps)
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
          SolvedPackage {..} ->
            "⊢ " ++ show _pkgName ++ "\n"
              ++ mconcat
                ( fmap
                    ( \SolvedDependency {..} -> "    ⊢ " ++ show _depName ++ " " ++ show _depType ++ (case _depProvider of (Just x) -> " ✔ " ++ show x; _ -> "") ++ "\n"
                    )
                    _pkgDeps
                )
          ProvidedPackage {..} -> "⊢ " ++ show _pkgName ++ " ✔ " ++ show _pkgProvider ++ "\n"
      )

prettyDeps :: [PackageName] -> String
prettyDeps list =
  mconcat $
    fmap (\(i, n) -> show @Int i ++ ". " ++ show n ++ "\n") $
      zip [1 ..] $
        reverse list
