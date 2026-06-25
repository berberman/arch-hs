{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import Args
import Check
import Control.Monad (unless)
import qualified Data.Map.Strict as Map
import Distribution.ArchHs.Core (subsumeGHCVersion)
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
  ExtraDB ->
  HackageDB ->
  RawHackageDB ->
  Bool ->
  Bool ->
  Bool ->
  IO (Either MyException ())
runCheck extra hackage rawHackage includeGHC runDepCheck verbose =
  ( runFinal
      . embedToFinal @IO
      . errorToIOFinal
      . evalState (Map.empty :: Map.Map PackageName [VersionRange])
      . ignoreTrace
      . runReader (Map.empty :: FlagAssignments)
      . runReader rawHackage
      . runReader hackage
      . runReader extra
  )
    ( if runDepCheck
        then subsumeGHCVersion $ check includeGHC runDepCheck verbose
        else runReader nullVersion $ check includeGHC runDepCheck verbose
    )

runSubmit ::
  ExtraDB ->
  HackageDB ->
  Manager ->
  Maybe String ->
  FilePath ->
  Bool ->
  IO (Either MyException ())
runSubmit extra hackage manager token output upload =
  ( runFinal
      . embedToFinal @IO
      . errorToIOFinal
      . runReader manager
      . runReader hackage
      . runReader extra
  )
    (submit token output upload)

runMode :: Mode -> IO ()
runMode = \case
  Submit CommonOptions {..} SubmitOptions {..} -> do
    extra <- loadExtraDBFromOptions optExtraDB
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
    runSubmit extra hackage manager token optOutput optUpload & printAppResult
  Check CommonOptions {..} CheckOptions {..} -> do
    extra <- loadExtraDBFromOptions optExtraDB
    (hackage, rawHackage) <-
      if optDepCheck
        then loadHackageDBsFromOptions optHackage
        else do
          hackage <- loadHackageDBFromOptions optHackage
          pure (hackage, Map.empty)
    runCheck extra hackage rawHackage optShowGHCLibs optDepCheck optVerbose & printAppResult
  List ExtraDBOptions {..} ListOptions {..} -> do
    extra <- loadExtraDBFromOptions
    putStrLn $
      unlines
        [ unArchLinuxName name <> (if optWithVersion then ": " <> version else "")
          | (name, _version -> version) <- Map.toList extra,
            isHaskellPackage name
        ]
