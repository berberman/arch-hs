{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import qualified Colourista as C
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

    let useDefaultHackage = isInfixOf "YOUR_HACKAGE_MIRROR" $ optHackagePath
        useDefaultCommunity = "/var/lib/pacman/sync/community.db" == optCommunityPath

    when useDefaultHackage $ C.skipMessage "You didn't pass -h, use hackage index file from default path."
    when useDefaultCommunity $ C.skipMessage "You didn't pass -c, use community db file from default path."

    token <- lookupEnv "HACKAGE_API_TOKEN"

    when (token == Nothing) $ C.warningMessage "You didn't set HACKAGE_API_TOKEN, dry run only."

    let hasOutput = not $ null optOutput
    when (hasOutput) $ do
      C.infoMessage $ "Output will be dumped to " <> (T.pack optOutput) <> "."
      exist <- doesFileExist optOutput
      when exist $
        C.warningMessage $ "File " <> (T.pack optOutput) <> " already existed, overwrite it."
    C.infoMessage "Start running..."
    when (not $ optUpload || hasOutput) $
      C.warningMessage "Run diff and check only."

    community <- loadProcessedCommunity $ if useDefaultCommunity then defaultCommunityPath else optCommunityPath
    C.infoMessage "Loading community.db..."

    hackage <- loadHackageDB =<< if useDefaultHackage then lookupHackagePath else return optHackagePath
    C.infoMessage "Loading hackage..."

    runSubmit community hackage (submit token optOutput optUpload) & printAppResult

runSubmit :: CommunityDB -> HackageDB -> Sem '[CommunityEnv, HackageEnv, WithMyErr, Embed IO, Final IO] a -> IO (Either MyException a)
runSubmit community hackage = runFinal . embedToFinal . errorToIOFinal . (runReader hackage) . (runReader community)
