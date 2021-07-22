{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Args
import Control.Monad (unless)
import qualified Data.Map as Map
import Data.Text.Prettyprint.Doc.Render.Terminal
import Diff
import Distribution.ArchHs.Core (subsumeGHCVersion)
import Distribution.ArchHs.Exception
import Distribution.ArchHs.Internal.Prelude
import Distribution.ArchHs.Options
import Distribution.ArchHs.PP
import Distribution.ArchHs.Types
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Network.HTTP.Client (Manager)
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

    community <- loadCommunityDBFromOptions optCommunityDB

    manager <- newTlsManager

    printInfo "Start running..."
    runDiff community optFlags manager (subsumeGHCVersion $ diffCabal optPackageName optVersionA optVersionB) & printAppResult

runDiff ::
  CommunityDB ->
  FlagAssignments ->
  Manager ->
  Sem '[CommunityEnv, FlagAssignmentsEnv, Reader Manager, Trace, DependencyRecord, WithMyErr, Embed IO, Final IO] a ->
  IO (Either MyException a)
runDiff community flags manager =
  runFinal
    . embedToFinal
    . errorToIOFinal
    . evalState Map.empty
    . ignoreTrace
    . runReader manager
    . runReader flags
    . runReader community
