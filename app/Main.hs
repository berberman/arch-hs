{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Main (main) where

import qualified Algebra.Graph.AdjacencyMap.Algorithm as G
import qualified Algebra.Graph.Labelled.AdjacencyMap as G
import qualified Colourista as C
import Community
import Conduit
import Control.Monad (filterM, when)
import Core
import Data.List (groupBy)
import qualified Data.Map.Strict as Map
import qualified Data.Set as S
import qualified Data.Text as T
import Distribution.Hackage.DB (HackageDB)
import Distribution.PackageDescription (Flag, flagDefault, flagDescription, flagManual, flagName, mkFlagAssignment, unFlagName)
import Distribution.Types.PackageName
import Hackage
import Lens.Micro
import Local
import PkgBuild
import System.Environment
import Types

h :: Members '[Embed IO, CommunityEnv, HackageEnv, FlagAssignmentEnv, WithMyErr] r => String -> Sem r ()
h name = do
  let target = mkPackageName name
  deps <- getDependencies S.empty 0 target
  exist <- isInCommunity target
  when exist $ throw $ TargetExist target

  let grouped = groupDeps deps
      allNames = fmap head . groupBy (==) $ (grouped ^.. each . pkgName ++ grouped ^.. each . pkgDeps . each . depName)
  providedList <- (++ ghcLibList) <$> filterM isInCommunity allNames
  let fillProvidedPkgs = mapC (\x -> if (x ^. pkgName) `elem` providedList then ProvidedPackage (x ^. pkgName) ByCommunity else x)
      fillProvidedDeps = mapC (pkgDeps %~ each %~ (\y -> if y ^. depName `elem` providedList then y & depProvider .~ (Just ByCommunity) else y))
      cooked = runConduitPure $ yieldMany grouped .| fillProvidedPkgs .| fillProvidedDeps .| sinkList
      toBePacked = cooked ^.. each . filtered (\case ProvidedPackage {..} -> False; _ -> True)

  liftIO $ C.infoMessage "Solved target:"
  liftIO $ putStrLn . prettySolvedPkgs $ cooked
  let flattened = filter (`elem` (toBePacked ^.. each . pkgName)) $ G.reachable target $ G.skeleton deps
  liftIO $ C.infoMessage "Recommended package order (from topological sort):"
  liftIO $ putStrLn . prettyDeps $ flattened
  flags <- filter (\(_, l) -> length l /= 0) <$> mapM (\n -> (n,) <$> getPackageFlag n) flattened
  liftIO $ when (length flags /= 0) $ do
      C.infoMessage "Detected flags from targets (their values will keep default unless you specify):"
      putStrLn . prettyFlags $ flags

  _ <- mapM (\solved -> (liftIO . writeFile ("/home/berberman/Desktop/test/" <> (solved ^. pkgName & unPackageName) <> ".PKGBUILD") . applyTemplate) =<< cabalToPkgBuild solved) toBePacked
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
    . runReader (Map.fromList [(mkPackageName "inline-c", (mkFlagAssignment [("gsl-example", True)]))])
    . runReader hackage
    . runReader community

main :: IO ()
main = do
  name <- head <$> getArgs
  hackage <- defaultHackageDB
  community <- defaultCommunity
  runH hackage community (h name) >>= \case
    Left x -> C.errorMessage $ "Error: " <> (T.pack . show $ x)
    _ -> C.successMessage "Success!"

groupDeps :: G.AdjacencyMap (S.Set DependencyType) PackageName -> [SolvedPackage]
groupDeps =
  fmap (\(name, deps) -> SolvedPackage name $ fmap (uncurry . flip $ SolvedDependency Nothing) deps)
    . fmap ((\(a, b, c) -> (head b, zip a c)) . unzip3)
    . groupBy (\x y -> uncurry (==) ((x, y) & both %~ (^. _2)))
    . fmap (_1 %~ S.toList)
    . S.toList
    . G.edgeSet

prettyFlags :: [(PackageName, [Flag])] -> String
prettyFlags = mconcat . fmap (\(name, flags) -> (C.formatWith [C.magenta] $ pkgNameStr name ++ "\n") ++ mconcat (fmap (C.formatWith [C.indent 4] . prettyFlag) flags))

prettyFlag :: Flag -> String
prettyFlag flag = "⚐ " ++ C.formatWith [C.yellow] name ++ ":\n" ++ mconcat (fmap (C.formatWith [C.indent 6] . (++ "\n")) $ ["description: " ++ desc, "default: " ++ def, "isManual: " ++ manual])
  where
    name = unFlagName . flagName $flag
    desc = flagDescription flag
    def = show $ flagDefault flag
    manual = show $ flagManual flag

prettySolvedPkgs :: [SolvedPackage] -> String
prettySolvedPkgs =
  mconcat
    . fmap
      ( \case
          SolvedPackage {..} ->
            C.formatWith [C.bold, C.yellow] ("⊢ " ++ pkgNameStr _pkgName ++ "\n")
              ++ mconcat
                ( fmap
                    ( \SolvedDependency {..} -> case _depProvider of
                        (Just x) -> (C.formatWith [C.green] $ "    ⊢ " ++ pkgNameStr _depName ++ " " ++ show _depType ++ " ✔ ") ++ (C.formatWith [C.cyan] $ "[" ++ show x ++ "]\n")
                        _ -> C.formatWith [C.bold, C.yellow] $ "    ⊢ " ++ pkgNameStr _depName ++ " " ++ show _depType ++ "\n"
                    )
                    _pkgDeps
                )
          ProvidedPackage {..} -> (C.formatWith [C.green] $ "⊢ " ++ pkgNameStr _pkgName ++ " ✔ ") ++ (C.formatWith [C.cyan] $ "[" ++ show _pkgProvider ++ "]\n")
      )

prettyDeps :: [PackageName] -> String
prettyDeps list =
  mconcat $
    fmap (\(i, n) -> show @Int i ++ ". " ++ pkgNameStr n ++ "\n") $
      zip [1 ..] $
        reverse list

pkgNameStr :: PackageName -> String
pkgNameStr = show . unPackageName