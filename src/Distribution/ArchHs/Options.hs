{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Copyright: (c) 2020-2021 berberman
-- SPDX-License-Identifier: MIT
-- Maintainer: berberman <berberman@yandex.com>
-- Stability: experimental
-- Portability: portable
-- This module contains CLI parsers used in executables.
-- "Options.Applicative.Simple" is re-exported.
module Distribution.ArchHs.Options
  ( -- * Load Extra DB
    ExtraDBOptions (..),
    extraDBOptionsParser,

    -- * Load files DB
    FilesDBOptions (..),
    filesDBOptionsParser,
    pacmanDBOptionsParser,

    -- * Load Hackage DB
    HackageDBOptions (..),
    hackageDBOptionsParser,

    -- * Parse flags
    optFlagAssignmentParser,
    optFlagReader,

    -- * Readers
    optPackageNameReader,
    optVersionReader,
    module Options.Applicative.Simple,
  )
where

import qualified Data.Map.Strict as Map
import Distribution.ArchHs.ExtraDB
import Distribution.ArchHs.FilesDB
import Distribution.ArchHs.Hackage
import Distribution.ArchHs.Internal.Prelude
import Distribution.ArchHs.PP
import Distribution.ArchHs.Types
import Options.Applicative.Simple

-----------------------------------------------------------------------------

-- | Parsed options for loading [extra]
newtype ExtraDBOptions = ExtraDBOptions
  { loadExtraDBFromOptions :: IO ExtraDB
  }

-- | CLI options parser of 'ExtraDBOptions'
--
-- When alpm is enabled, it also reads a flag @alpm@.
extraDBOptionsParser :: Parser ExtraDBOptions
extraDBOptionsParser =
  mkExtraDBOptions <$> alpmOptionsParser <*> extraDBPathParser

extraDBPathParser :: Parser FilePath
extraDBPathParser =
  strOption $
    long "extra"
      <> metavar "PATH"
      <> short 'c'
      <> help "Path to extra.db"
      <> showDefault
      <> value defaultExtraDBPath

mkExtraDBOptions :: Bool -> FilePath -> ExtraDBOptions
mkExtraDBOptions useAlpm path =
  ExtraDBOptions
    ( do
        let src = if useAlpm then "libalpm" else path
        printInfo $ "Loading extra.db from" <+> pretty src
#ifdef ALPM
        if useAlpm
          then loadExtraDBFFI
          else loadExtraDB path
#else
        loadExtraDB path
#endif
    )
-----------------------------------------------------------------------------

-- | Parsed options for loading 'FilesDB'
newtype FilesDBOptions = FilesDBOptions
  { loadFilesDBFromOptions :: DBKind -> IO FilesDB
  }

-- | CLI options parser of 'ExtraDBOptions'
--
-- When alpm is enabled, it also reads a flag @alpm@.
filesDBOptionsParser :: Parser FilesDBOptions
filesDBOptionsParser =
  mkFilesDBOptions <$> alpmOptionsParser <*> filesDBPathParser

filesDBPathParser :: Parser FilePath
filesDBPathParser =
  strOption $
    long "files"
      <> metavar "PATH"
      <> short 'f'
      <> help
        "Path of dir that includes core.files, extra.files and extra.files"
      <> showDefault
      <> value defaultFilesDBDir

mkFilesDBOptions :: Bool -> FilePath -> FilesDBOptions
mkFilesDBOptions useAlpm path =
  FilesDBOptions
    ( \db ->
        do
          let src = if useAlpm then "libalpm" else path
          printInfo $
            "Loading" <+> ppDBKind db <+> "files from" <+> pretty src
#ifdef ALPM
          if useAlpm then loadFilesDBFFI db else loadFilesDB db path
#else
          loadFilesDB db path
#endif
    )

-- | CLI options parser for commands that need both pacman database loaders.
--
-- When alpm is enabled, it reads @alpm@ once and applies it to both loaders.
pacmanDBOptionsParser :: Parser (ExtraDBOptions, FilesDBOptions)
pacmanDBOptionsParser =
  mkPacmanDBOptions
    <$> alpmOptionsParser
    <*> extraDBPathParser
    <*> filesDBPathParser
  where
    mkPacmanDBOptions useAlpm extraPath filesPath =
      (mkExtraDBOptions useAlpm extraPath, mkFilesDBOptions useAlpm filesPath)

alpmOptionsParser :: Parser Bool
#ifndef ALPM
alpmOptionsParser = pure False
#else
alpmOptionsParser =
  switch $
    long "alpm"
      <> help "Use libalpm to load pacman databases"
#endif
-----------------------------------------------------------------------------

-- | Parsed options for loading 'HackageDB'
newtype HackageDBOptions = HackageDBOptions
  { loadHackageDBFromOptions :: IO HackageDB
  }

-- | CLI options parser that reads a string option @hackage@.
hackageDBOptionsParser :: Parser HackageDBOptions
hackageDBOptionsParser =
  HackageDBOptions
    <$> fmap
      ( \s ->
          do
            hackagePath <- if null s then lookupHackagePath else pure s
            printInfo $ "Loading hackage from" <+> pretty hackagePath
            loadHackageDB hackagePath
      )
      ( strOption $
          long "hackage"
            <> metavar "PATH"
            <> short 'h'
            <> help "Path to hackage index tarball"
            <> showDefault
            <> value ""
      )

-----------------------------------------------------------------------------

-- | Read a flag assignment like @package_name:flag_name:true|false@
optFlagReader :: ReadM (String, String, Bool)
optFlagReader = eitherReader $ \s -> case splitOn ":" s of
  [name, fname, fvalue] -> case fvalue of
    "true" -> Right (name, fname, True)
    "false" -> Right (name, fname, False)
    _ -> Left "Unknown boolean value, it should be 'true' or 'false'"
  _ -> Left "Failed to parse flag assignment"

-- | CLI options parser of flag assignments
optFlagAssignmentParser :: Parser (Map.Map PackageName FlagAssignment)
optFlagAssignmentParser =
  fmap toFlagAssignment <$> many $
    option optFlagReader $
      long "flag"
        <> metavar "package_name:flag_name:true|false"
        <> short 'f'
        <> help "A sinlge flag assignment for a package - e.g. inline-c:gsl-example:true"

toFlagAssignment :: [(String, String, Bool)] -> Map.Map PackageName FlagAssignment
toFlagAssignment xs =
  Map.map toAssignment $
    foldr (\(name, fname, fvalue) acc -> Map.insertWith (<>) (mkPackageName name) [(mkFlagName fname, fvalue)] acc) Map.empty xs
  where
    toAssignment = foldr (\(fname, fvalue) acc -> insertFlagAssignment fname fvalue acc) (mkFlagAssignment [])

-----------------------------------------------------------------------------

-- | Read a 'Version'
-- This function calls 'simpleParsec'.
optVersionReader :: ReadM Version
optVersionReader =
  eitherReader
    ( \s -> case simpleParsec s of
        Just v -> Right v
        _ -> Left $ "Failed to parse version: " <> s
    )

-- | Read a 'PackageName'
-- This function never fails, because it just wraps the input string with 'mkPackageName'.
optPackageNameReader :: ReadM PackageName
optPackageNameReader = eitherReader $ Right . mkPackageName
