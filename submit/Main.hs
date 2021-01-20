{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Monad (unless)
import Distribution.ArchHs.CommunityDB
import Distribution.ArchHs.Exception
import Distribution.ArchHs.Hackage
import Distribution.ArchHs.Internal.Prelude
import Distribution.ArchHs.PP
import Distribution.ArchHs.Types
import Network.HTTP.Client (Manager)
import Network.HTTP.Client.TLS (newTlsManager)
import Submit
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)

main :: IO ()
main = printHandledIOException $
  do
    Options {..} <- runArgsParser

    token <- lookupEnv "HACKAGE_API_TOKEN"

    when (null token) $
      printWarn "You didn't set HACKAGE_API_TOKEN, dry run only."

    let hasOutput = not $ null optOutput
    when hasOutput $ do
      printInfo $ "Output will be dumped to" <+> pretty optOutput
      exist <- doesFileExist optOutput
      when exist $
        printWarn $ "File" <+> pretty optOutput <+> "already existed" <+> "overwrite it"
    printInfo "Start running..."
    unless (optUpload || hasOutput) $
      printWarn "Run diff and check only"

#ifdef ALPM
    let src = if optAlpm then "libalpm" else defaultCommunityDBPath
    printInfo $ "Loading community.db from" <+> pretty src
    community <- if optAlpm then loadCommunityDBFFI else loadCommunityDB defaultCommunityDBPath
#else
    printInfo $ "Loading community.db from" <+> pretty optCommunityDBPath
    community <- loadCommunityDB optCommunityDBPath
#endif

    hackagePath <- if null optHackagePath then lookupHackagePath else return optHackagePath

    printInfo $ "Loading hackage from" <+> pretty hackagePath

    hackage <- loadHackageDB hackagePath

    manager <- newTlsManager

    runSubmit community hackage manager (submit token optOutput optUpload) & printAppResult

runSubmit ::
  CommunityDB ->
  HackageDB ->
  Manager ->
  Sem '[CommunityEnv, HackageEnv, Reader Manager, WithMyErr, Embed IO, Final IO] a ->
  IO (Either MyException a)
runSubmit community hackage manager =
  runFinal
    . embedToFinal
    . errorToIOFinal
    . runReader manager
    . runReader hackage
    . runReader community
