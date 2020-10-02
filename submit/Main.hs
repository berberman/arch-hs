{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import qualified Colourista as C
import qualified Data.Text as T
import Distribution.ArchHs.Community
import Distribution.ArchHs.Exception
import Distribution.ArchHs.Internal.Prelude
import Distribution.ArchHs.Types
import Submit
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)

main :: IO ()
main = printHandledIOException $
  do
    Options {..} <- runArgsParser
    let useDefaultCommunity = "/var/lib/pacman/sync/community.db" == optCommunityPath

    when useDefaultCommunity $ C.skipMessage "You didn't pass -c, use community db file from default path."

    community <- loadProcessedCommunity $ if useDefaultCommunity then defaultCommunityPath else optCommunityPath
    C.infoMessage "Loading community.db..."

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
      C.warningMessage "Run diff only."
    runSubmit community (submit token optOutput optUpload) & printAppResult

runSubmit :: CommunityDB -> Sem '[CommunityEnv, WithMyErr, Embed IO, Final IO] a -> IO (Either MyException a)
runSubmit community = runFinal . embedToFinal . errorToIOFinal . (runReader community)
