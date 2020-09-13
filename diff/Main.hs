{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import qualified Colourista as C
import qualified Control.Exception as CE
import Control.Monad (when)
import Data.List (isInfixOf)
import qualified Data.Text as T
import Diff
import Distribution.Hackage.DB (HackageDB)
import Hackage
import Polysemy
import Polysemy.Error
import Polysemy.Reader
import Types
import qualified Data.Map as Map

main :: IO ()
main = CE.catch @CE.IOException
  ( do
      Options {..} <- runArgsParser
      let useDefaultHackage = isInfixOf "YOUR_HACKAGE_MIRROR" $ optHackagePath
      when useDefaultHackage $ C.skipMessage "You didn't pass -h, use hackage index file from default places."
      hackage <- loadHackageDB =<< if useDefaultHackage then lookupHackagePath else return optHackagePath
      C.infoMessage "Loading hackage..."
      runDiff hackage (diffCabal optPackageName optVersionA optVersionB) >>= \case
        Left x -> C.errorMessage $ "Error " <> (T.pack . show $ x)
        Right r -> putStrLn r >> C.successMessage "Success!"
  )
  $ \e -> C.errorMessage $ "IOException " <> (T.pack . show $ e)

runDiff ::
  HackageDB ->
  Sem '[HackageEnv, FlagAssignmentEnv,WithMyErr, Embed IO, Final IO] a ->
  IO (Either MyException a)
runDiff hackage = runFinal . embedToFinal . errorToIOFinal . runReader (Map.empty).runReader hackage
