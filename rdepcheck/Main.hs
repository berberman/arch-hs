{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Monad (unless)
import qualified Data.Map.Strict as Map
import Distribution.ArchHs.Core
import Distribution.ArchHs.Exception
import Distribution.ArchHs.Hackage
import Distribution.ArchHs.Internal.Prelude
import Distribution.ArchHs.Name (toHackageName)
import Distribution.ArchHs.Options
import Distribution.ArchHs.PP
import Distribution.ArchHs.RDepCheck (reverseDependencyPackages)
import Distribution.ArchHs.Types
import GHC.IO.Encoding (setLocaleEncoding)
import GHC.IO.Encoding.UTF8 (utf8)
import RDepCheck
import RDepCheck.Args

main :: IO ()
main = printHandledIOException $
  do
    setLocaleEncoding utf8
    Options {..} <- runArgsParser
    let isFlagEmpty = Map.null optFlags

    unless isFlagEmpty $ do
      printInfo "You assigned flags:"
      putDoc $ prettyFlagAssignments optFlags <> line

    extra <- loadExtraDBFromOptions optExtraDB
    let packages =
          [ (toHackageName $ _name desc, version)
            | (target, _) <- optTargets,
              (desc, _) <- reverseDependencyPackages extra target,
              Just version <- [simpleParsec $ _version desc]
          ]
            <> [(target, version) | (target, Just version) <- optTargets, length optTargets > 1]
    (hackage, revision0) <- loadRawHackageRevisionsFromOptions optHackage packages

    printInfo "Start running..."
    runCheck hackage extra optFlags (subsumeGHCVersion $ checkTargets revision0 optTargets) & printRdepcheckResult

runCheck ::
  RawHackageDB ->
  ExtraDB ->
  FlagAssignments ->
  Sem '[RawHackageEnv, ExtraEnv, FlagAssignmentsEnv, Trace, DependencyRecord, WithMyErr, Embed IO, Final IO] a ->
  IO (Either MyException a)
runCheck extra flags manager =
  runFinal
    . embedToFinal
    . errorToIOFinal
    . evalState Map.empty
    . ignoreTrace
    . runReader manager
    . runReader flags
    . runReader extra
