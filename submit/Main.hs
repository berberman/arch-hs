{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import qualified Colourista as C
import Control.Monad (unless)
import qualified Data.Text as T
import Distribution.ArchHs.Community
import Distribution.ArchHs.Exception
import Distribution.ArchHs.Hackage
import Distribution.ArchHs.Internal.Prelude
import Distribution.ArchHs.Types
import Submit
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)

main :: IO ()
main = printHandledIOException $
  do
    Options {..} <- runArgsParser

    let useDefaultHackage = "YOUR_HACKAGE_MIRROR" `isInfixOf` optHackagePath
    when useDefaultHackage $ C.skipMessage "You didn't pass -h, use hackage index file from default path."

#ifndef ALPM
    let useDefaultCommunity = "/var/lib/pacman/sync/community.db" == optCommunityPath
    when useDefaultCommunity $ C.skipMessage "You didn't pass -c, use community db file from default path."
#endif

    token <- lookupEnv "HACKAGE_API_TOKEN"

    when (null token) $ C.warningMessage "You didn't set HACKAGE_API_TOKEN, dry run only."

    let hasOutput = not $ null optOutput
    when hasOutput $ do
      C.infoMessage $ "Output will be dumped to " <> T.pack optOutput <> "."
      exist <- doesFileExist optOutput
      when exist $
        C.warningMessage $ "File " <> T.pack optOutput <> " already existed, overwrite it."
    C.infoMessage "Start running..."
    unless (optUpload || hasOutput) $
      C.warningMessage "Run diff and check only."

#ifdef ALPM
    when optAlpm $ C.infoMessage "Using alpm."
    community <- if optAlpm then loadCommunityFFI else loadProcessedCommunity defaultCommunityPath
#else
    community <- loadProcessedCommunity $ if useDefaultCommunity then defaultCommunityPath else optCommunityPath
#endif

    C.infoMessage "Loading community.db..."

    hackage <- loadHackageDB =<< if useDefaultHackage then lookupHackagePath else return optHackagePath
    C.infoMessage "Loading hackage..."

    runSubmit community hackage (submit token optOutput optUpload) & printAppResult

runSubmit :: CommunityDB -> HackageDB -> Sem '[CommunityEnv, HackageEnv, WithMyErr, Embed IO, Final IO] a -> IO (Either MyException a)
runSubmit community hackage = runFinal . embedToFinal . errorToIOFinal . runReader hackage . runReader community
