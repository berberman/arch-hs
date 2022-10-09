{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import Args
import Check
import Control.Monad (unless)
import qualified Data.Map.Strict as Map
import Distribution.ArchHs.Exception
import Distribution.ArchHs.Hackage
import Distribution.ArchHs.Internal.Prelude
import Distribution.ArchHs.Name (isHaskellPackage)
import Distribution.ArchHs.Options
import Distribution.ArchHs.PP
import Distribution.ArchHs.Types
import Network.HTTP.Client
import Network.HTTP.Client.TLS (newTlsManager)
import Submit
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)

main :: IO ()
main = runArgsParser >>= runMode

runCheck ::
  CommunityDB ->
  HackageDB ->
  Bool ->
  IO (Either MyException ())
runCheck community hackage includeGHC =
  ( runFinal
      . embedToFinal @IO
      . errorToIOFinal
      . runReader hackage
      . runReader community
  )
    (check includeGHC)

runSubmit ::
  CommunityDB ->
  HackageDB ->
  Manager ->
  Maybe String ->
  FilePath ->
  Bool ->
  IO (Either MyException ())
runSubmit community hackage manager token output upload =
  ( runFinal
      . embedToFinal @IO
      . errorToIOFinal
      . runReader manager
      . runReader hackage
      . runReader community
  )
    (submit token output upload)

runMode :: Mode -> IO ()
runMode = \case
  Submit CommonOptions {..} SubmitOptions {..} -> do
    community <- loadCommunityDBFromOptions optCommunityDB
    hackage <- loadHackageDBFromOptions optHackage

    token <- lookupEnv "HACKAGE_API_TOKEN"

    when (null token) $
      printWarn "You didn't set HACKAGE_API_TOKEN, dry run only."

    let hasOutput = not $ null optOutput
    when hasOutput $ do
      printInfo $ "Output will be dumped to" <+> pretty optOutput
      exist <- doesFileExist optOutput
      when exist $
        printWarn $
          "File" <+> pretty optOutput <+> "already existed" <+> "overwrite it"

    printInfo "Start running..."
    unless (optUpload || hasOutput) $
      printWarn "Run diff and check only"

    manager <- newTlsManager
    runSubmit community hackage manager token optOutput optUpload & printAppResult
  Check CommonOptions {..} CheckOptions {..} -> do
    community <- loadCommunityDBFromOptions optCommunityDB
    hackage <- loadHackageDBFromOptions optHackage
    runCheck community hackage optShowGHCLibs & printAppResult
  List CommunityDBOptions {..} ListOptions {..} -> do
    community <- loadCommunityDBFromOptions
    putStrLn $
      unlines
        [ unArchLinuxName name <> (if optWithVersion then ": " <> version else "")
          | (name, _version -> version) <- Map.toList community,
            isHaskellPackage name
        ]
