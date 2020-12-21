{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Monad (unless)
import qualified Data.Text as T
import Distribution.ArchHs.CommunityDB
import Distribution.ArchHs.Exception
import Distribution.ArchHs.Hackage
import Distribution.ArchHs.Internal.Prelude
import Distribution.ArchHs.PP
import Distribution.ArchHs.Types
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
      printInfo $ "Output will be dumped to " <> T.pack optOutput <> "."
      exist <- doesFileExist optOutput
      when exist $
        printWarn $ "File " <> T.pack optOutput <> " already existed, overwrite it."
    printInfo "Start running..."
    unless (optUpload || hasOutput) $
      printWarn "Run diff and check only."

#ifdef ALPM
    let src = T.pack $ if optAlpm then "libalpm" else defaultCommunityDBPath
    printInfo $ "Loading community.db from " <> src
    community <- if optAlpm then loadCommunityDBFFI else loadCommunityDB defaultCommunityDBPath
#else
    printInfo $ "Loading community.db from " <> T.pack optCommunityDBPath
    community <- loadCommunityDB optCommunityDBPath
#endif

    printInfo "Loading community.db..."

    hackagePath <- if null optHackagePath then lookupHackagePath else return optHackagePath

    printInfo $ "Loading hackage from " <> T.pack hackagePath

    hackage <- loadHackageDB hackagePath

    runSubmit community hackage (submit token optOutput optUpload) & printAppResult

runSubmit :: CommunityDB -> HackageDB -> Sem '[CommunityEnv, HackageEnv, WithMyErr, Embed IO, Final IO] a -> IO (Either MyException a)
runSubmit community hackage = runFinal . embedToFinal . errorToIOFinal . runReader hackage . runReader community
