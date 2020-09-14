{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import qualified Colourista as C
import qualified Control.Exception as CE
import qualified Data.Map as Map
import qualified Data.Text as T
import Diff
import Polysemy
import Polysemy.Error
import Polysemy.Reader
import Types

main :: IO ()
main = CE.catch @CE.IOException
  ( do
      Options {..} <- runArgsParser
      C.infoMessage "Start running..."
      runDiff (diffCabal optPackageName optVersionA optVersionB) >>= \case
        Left x -> C.errorMessage $ "Error " <> (T.pack . show $ x)
        Right r -> putStrLn r >> C.successMessage "Success!"
  )
  $ \e -> C.errorMessage $ "IOException " <> (T.pack . show $ e)

runDiff :: Sem '[FlagAssignmentEnv, WithMyErr, Embed IO, Final IO] a -> IO (Either MyException a)
runDiff = runFinal . embedToFinal . errorToIOFinal . runReader (Map.empty)