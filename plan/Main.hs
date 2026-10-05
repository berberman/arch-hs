{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import qualified Control.Exception as Exception
import Control.Monad (unless)
import qualified Data.Map.Strict as Map
import Distribution.ArchHs.Core (subsumeGHCVersion)
import Distribution.ArchHs.Exception
import Distribution.ArchHs.Internal.Prelude
import Distribution.ArchHs.Options
import Distribution.ArchHs.PP
import GHC.IO.Encoding (setLocaleEncoding)
import GHC.IO.Encoding.UTF8 (utf8)
import Plan
import Plan.Args
import Plan.Toolchain (loadGHCReleases)
import Plan.Trace (runPlanTrace, tracePlan)
import System.Exit (die, exitFailure)
import System.IO (hFlush, hPutStrLn, stderr)

main :: IO ()
main = Exception.handle @Exception.IOException (\err -> printError (viaShow err) >> exitFailure) $ do
  setLocaleEncoding utf8
  Options {..} <- runArgsParser
  let debug message = when optDebug $ hPutStrLn stderr ("[plan] " <> message) >> hFlush stderr
  debug "Loading package databases..."
  releases <- if any ((== "ghc") . fst) optTargets
    then do
      debug "Loading upstream GHC bundled-library metadata..."
      printInfo "Loading upstream GHC bundled-library metadata..."
      either die pure =<< loadGHCReleases
    else pure Map.empty
  debug "Loading repository package metadata..."
  extra <- loadExtraDBFromOptions optExtraDB
  debug "Loading the Hackage index and Cabal revisions..."
  (hackage, raw, original) <- loadHackageDBsWithRevisionsFromOptions optHackage
  unless (Map.null optFlags) $ printInfo $ "Assigned flags:" <> line <> prettyFlagAssignments optFlags
  printInfo $ if optSolve then "Searching incremental update sets..." else "Checking update set..."
  result <-
    runFinal
      . embedToFinal @IO
      . errorToIOFinal @MyException
      . evalState (Map.empty :: Map.Map PackageName [VersionRange])
      . runPlanTrace optDebug stderr
      . runReader optFlags
      . runReader raw
      . runReader hackage
      . runReader extra
      . subsumeGHCVersion
      $ do
        planned <- planUpdates releases optSolve optTargets
        tracePlan "Comparing Cabal revisions for the selected plan..."
        traverse (comparePlanRevisions original) planned
  case result of
    Left err -> printError (viaShow err) >> exitFailure
    Right (Left err) -> printError (pretty err) >> exitFailure
    Right (Right plan) -> do
      putDoc $ prettyPlanResult plan <> line
      unless (planIsReady plan) $ do
        if not $ null $ planSearchNotes plan
          then printError "The available versions cannot satisfy this update set. Showing the attempted set with the fewest blockers."
          else if optSolve
            then printError "No verified working set found after considering updates to blocking dependencies and reverse dependencies. Showing the attempted set with the fewest blockers."
            else printInfo "Use --solve to find compatible versions and automatically update blocking dependencies and reverse dependencies."
        exitFailure
