{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import qualified Algebra.Graph.AdjacencyMap.Algorithm as G
import qualified Algebra.Graph.Labelled.AdjacencyMap as G
import Community
import Conduit
import Control.Monad.Except
import Core
import Data.List (groupBy)
import qualified Data.Map as Map
import qualified Data.Set as S
import Distribution.Types.PackageName
import Hackage
import Lens.Micro
import Local
import PkgBuild
import System.Environment
import Types
import Utils

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

main :: IO ()
main = do
  env <- getHsEnv
  h env >>= print

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
    fmap (\(i, n) -> show @Int i ++ ". " ++ show n ++ "\n") $
      zip [1 ..] $
        reverse list
