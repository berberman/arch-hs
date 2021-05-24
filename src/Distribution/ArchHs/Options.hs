{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Copyright: (c) 2020-2021 berberman
-- SPDX-License-Identifier: MIT
-- Maintainer: berberman <berberman@yandex.com>
-- Stability: experimental
-- Portability: portable
-- This module contains CLI parsers to load three types of databases.
-- See "Distribution.ArchHs.Hackage", "Distribution.ArchHs.FilesDB", and "Distribution.ArchHs.CommunityDB".
module Distribution.ArchHs.Options
  ( CommunityDBOptions (..),
    communityDBOptionsParser,
    FilesDBOptions (..),
    filesDBOptionsParser,
    HackageDBOptions (..),
    hackageDBOptionsParser,
  )
where

import Distribution.ArchHs.CommunityDB
import Distribution.ArchHs.FilesDB
import Distribution.ArchHs.Hackage
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
