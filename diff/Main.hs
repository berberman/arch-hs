{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Args
import Control.Monad (unless)
import qualified Data.Map as Map
import Diff
import Distribution.ArchHs.Core (subsumeGHCVersion)
import Distribution.ArchHs.Exception
import Distribution.ArchHs.Hackage (lookupHackagePath)
import Distribution.ArchHs.Internal.Prelude
import Distribution.ArchHs.Options
import Distribution.ArchHs.PP
import Distribution.ArchHs.Types
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Network.HTTP.Client.TLS (newTlsManager)

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

    cabalSource <-
      if optOffline
        then do
          hackagePath <- if null optHackagePath then lookupHackagePath else pure optHackagePath
          printInfo $ "Loading hackage from" <+> pretty hackagePath
          pure $ Offline hackagePath
        else Online <$> newTlsManager

    printInfo "Start running..."
    runDiff extra optFlags cabalSource (subsumeGHCVersion $ diffCabal optPackageName optVersionA optVersionB) & printAppResult

runDiff ::
  ExtraDB ->
  FlagAssignments ->
  CabalSource ->
  Sem '[ExtraEnv, FlagAssignmentsEnv, Reader CabalSource, Trace, DependencyRecord, WithMyErr, Embed IO, Final IO] a ->
  IO (Either MyException a)
runDiff extra flags cabalSource =
  runFinal
    . embedToFinal
    . errorToIOFinal
    . evalState Map.empty
    . ignoreTrace
    . runReader cabalSource
    . runReader flags
    . runReader extra
