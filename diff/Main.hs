{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import qualified Colourista as C
import qualified Data.Map as Map
import qualified Data.Text as T
import Diff
import Distribution.ArchHs.Community
  ( defaultCommunityPath,
    loadProcessedCommunity,
  )
import Distribution.ArchHs.Exception
import Distribution.ArchHs.Internal.Prelude
import Distribution.ArchHs.PP (prettyFlagAssignments)
import Distribution.ArchHs.Types

main :: IO ()
main = printHandledIOException $
  do
    Options {..} <- runArgsParser
    let useDefaultCommunity = "/var/lib/pacman/sync/community.db" == optCommunityPath
        isFlagEmpty = Map.null optFlags

    when useDefaultCommunity $ C.skipMessage "You didn't pass -c, use community db file from default path."

    when isFlagEmpty $ C.skipMessage "You didn't pass -f, different flag values may make difference in dependency resolving."
    when (not isFlagEmpty) $ do
      C.infoMessage "You assigned flags:"
      putStrLn . prettyFlagAssignments $ optFlags

    community <- loadProcessedCommunity $ if useDefaultCommunity then defaultCommunityPath else optCommunityPath
    C.infoMessage "Loading community.db..."

    C.infoMessage "Start running..."
    runDiff community optFlags (diffCabal optPackageName optVersionA optVersionB) >>= \case
      Left x -> C.errorMessage $ "Runtime Error: " <> (T.pack . show $ x)
      Right r -> putStrLn r >> C.successMessage "Success!"

runDiff :: CommunityDB -> FlagAssignments -> Sem '[CommunityEnv, FlagAssignmentsEnv, Trace, DependencyRecord, WithMyErr, Embed IO, Final IO] a -> IO (Either MyException a)
runDiff community flags = runFinal . embedToFinal . errorToIOFinal . evalState (Map.empty) . ignoreTrace . (runReader flags) . (runReader community)
