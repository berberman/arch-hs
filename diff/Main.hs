{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Monad (unless)
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc.Render.Terminal
import Diff
import Distribution.ArchHs.CommunityDB
import Distribution.ArchHs.Exception
import Distribution.ArchHs.Internal.Prelude
import Distribution.ArchHs.PP
import Distribution.ArchHs.Types

main :: IO ()
main = printHandledIOException $
  do
    Options {..} <- runArgsParser
    let isFlagEmpty = Map.null optFlags

    unless isFlagEmpty $ do
      printInfo "You assigned flags:"
      putDoc $ prettyFlagAssignments optFlags <> line

#ifdef ALPM
    let src = T.pack $ if optAlpm then "libalpm" else defaultCommunityDBPath
    printInfo $ "Loading community.db from " <> src
    community <- if optAlpm then loadCommunityDBFFI else loadCommunityDB defaultCommunityDBPath
#else
    printInfo $ "Loading community.db from " <> T.pack optCommunityDBPath
    community <- loadCommunityDB optCommunityDBPath
#endif

    printInfo "Start running..."
    runDiff community optFlags (diffCabal optPackageName optVersionA optVersionB) & printAppResult

runDiff :: CommunityDB -> FlagAssignments -> Sem '[CommunityEnv, FlagAssignmentsEnv, Trace, DependencyRecord, WithMyErr, Embed IO, Final IO] a -> IO (Either MyException a)
runDiff community flags = runFinal . embedToFinal . errorToIOFinal . evalState Map.empty . ignoreTrace . runReader flags . runReader community
