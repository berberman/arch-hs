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
  ( -- * Load Community DB
    CommunityDBOptions (..),
    communityDBOptionsParser,

    -- * Load files DB
    FilesDBOptions (..),
    filesDBOptionsParser,

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
import Distribution.ArchHs.CommunityDB
import Distribution.ArchHs.FilesDB
import Distribution.ArchHs.Hackage
import Distribution.ArchHs.Internal.Prelude
import Distribution.ArchHs.PP
import Distribution.ArchHs.Types
import Options.Applicative.Simple

-----------------------------------------------------------------------------

-- | Parsed options for loading [community]
newtype CommunityDBOptions = CommunityDBOptions
  { loadCommunityDBFromOptions :: IO CommunityDB
  }

-- | CLI options parser of 'CommunityDBOptions'
--
-- When alpm is enabled, it reads a flag @no-alpm-community@;
-- otherwise it reads a string option @community@.
communityDBOptionsParser :: Parser CommunityDBOptions

#ifndef ALPM
communityDBOptionsParser =
  CommunityDBOptions
    <$> fmap
      ( \s ->
          do
            printInfo $ "Loading community.db from" <+> pretty s
            loadCommunityDB s
      )
      ( strOption $
          long "community"
            <> metavar "PATH"
            <> short 'c'
            <> help "Path to community.db"
            <> showDefault
            <> value defaultCommunityDBPath
      )
#else
communityDBOptionsParser =
  CommunityDBOptions
    <$> fmap
      ( \b ->
          do
            let src = if b then "libalpm" else defaultCommunityDBPath
            printInfo $ "Loading community.db from" <+> pretty src
            if b
              then loadCommunityDBFFI
              else loadCommunityDB defaultCommunityDBPath
      )
      ( flag
          True
          False
          ( long "no-alpm-community"
              <> help "Do not use libalpm to parse community db"
          )
      )
#endif
-----------------------------------------------------------------------------

-- | Parsed options for loading 'FilesDB'
newtype FilesDBOptions = FilesDBOptions
  { loadFilesDBFromOptions :: DBKind -> IO FilesDB
  }

-- | CLI options parser of 'CommunityDBOptions'
--
-- When alpm is enabled, it reads a flag @no-alpm-files@;
-- otherwise it reads a string option @files@.
filesDBOptionsParser :: Parser FilesDBOptions

#ifndef ALPM
filesDBOptionsParser =
  FilesDBOptions
    <$> fmap
      ( \s db ->
          do
            printInfo $
              "Loading" <+> ppDBKind db <+> "files from" <+> pretty s
            loadFilesDB db s
      )
      ( strOption $
          long "files"
            <> metavar "PATH"
            <> short 'f'
            <> help
              "Path of dir that includes core.files, extra.files and community.files"
            <> showDefault
            <> value defaultFilesDBDir
      )
#else
filesDBOptionsParser =
  FilesDBOptions
    <$> fmap
      ( \b db ->
          do
            let src = if b then "libalpm" else defaultFilesDBDir
            printInfo $
              "Loading" <+> ppDBKind db <+> "files from" <+> pretty src
            if b then loadFilesDBFFI db else loadFilesDB db defaultFilesDBDir
      )
      ( flag
          True
          False
          ( long "no-alpm-files"
              <> help "Do not use libalpm to parse files db"
          )
      )
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
