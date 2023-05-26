{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Args
import Check
import Control.Monad (unless)
import qualified Data.Map.Strict as Map
import Distribution.ArchHs.Core
import Distribution.ArchHs.Exception
import Distribution.ArchHs.Hackage
import Distribution.ArchHs.Internal.Prelude
import Distribution.ArchHs.Options
import Distribution.ArchHs.PP
import Distribution.ArchHs.Types
import GHC.IO.Encoding (setLocaleEncoding)
import GHC.IO.Encoding.UTF8 (utf8)

main :: IO ()
main = printHandledIOException $
  do
    setLocaleEncoding utf8
    Options {..} <- runArgsParser
    let isFlagEmpty = Map.null optFlags

    unless isFlagEmpty $ do
      printInfo "You assigned flags:"
      putDoc $ prettyFlagAssignments optFlags <> line

    hackage <- loadHackageDBFromOptions optHackage
    extra <- loadExtraDBFromOptions optExtraDB

    printInfo "Start running..."
    runCheck hackage extra optFlags (subsumeGHCVersion $ check optPackageName) & printAppResult

runCheck ::
  HackageDB ->
  ExtraDB ->
  FlagAssignments ->
  Sem '[HackageEnv, ExtraEnv, FlagAssignmentsEnv, Trace, DependencyRecord, WithMyErr, Embed IO, Final IO] a ->
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
