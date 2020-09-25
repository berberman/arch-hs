{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import qualified Colourista as C
import qualified Control.Exception as CE
import Control.Monad (when)
import qualified Data.Map as Map
import qualified Data.Text as T
import Diff
import Distribution.ArchHs.PP (prettyFlagAssignments)
import Distribution.ArchHs.Types

main :: IO ()
main = CE.catch @CE.IOException
  ( do
      Options {..} <- runArgsParser

      let isFlagEmpty = Map.null optFlags
      when isFlagEmpty $ C.skipMessage "You didn't pass -f, different flag values may make difference in dependency resolving."
      when (not isFlagEmpty) $ do
        C.infoMessage "You assigned flags:"
        putStrLn . prettyFlagAssignments $ optFlags

      C.infoMessage "Start running..."
      runDiff optFlags (diffCabal optPackageName optVersionA optVersionB) >>= \case
        Left x -> C.errorMessage $ "Runtime Error: " <> (T.pack . show $ x)
        Right r -> putStrLn r >> C.successMessage "Success!"
  )
  $ \e -> C.errorMessage $ "IOException: " <> (T.pack . show $ e)

runDiff :: FlagAssignments -> Sem '[FlagAssignmentsEnv, Trace, WithMyErr, Embed IO, Final IO] a -> IO (Either MyException a)
runDiff flags = runFinal . embedToFinal . errorToIOFinal . ignoreTrace . (runReader flags)
