{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

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

newtype CommunityDBOptions = CommunityDBOptions
  { loadCommunityDBFromOptions :: IO CommunityDB
  }

#ifndef ALPM
communityDBOptionsParser :: Parser CommunityDBOptions
communityDBOptionsParser =
  CommunityDBOptions
    <$> ( ( flip fmap $
              strOption $
                long
                  "community"
                  <> metavar "PATH"
                  <> short 'c'
                  <> help "Path to community.db"
                  <> showDefault
                  <> value defaultCommunityDBPath
          )
            $ \s -> do
              printInfo $ "Loading community.db from" <+> pretty s
              loadCommunityDB s
        )
#else
communityDBOptionsParser :: Parser CommunityDBOptions
communityDBOptionsParser =
  CommunityDBOptions
    <$> ( ( flip fmap $
              flag
                True
                False
                ( long "no-alpm-community"
                    <> help "Do not use libalpm to parse community db"
                )
          )
            $ \b -> do
              let src = if b then "libalpm" else defaultCommunityDBPath
              printInfo $ "Loading community.db from" <+> pretty src
              if b
                then loadCommunityDBFFI
                else loadCommunityDB defaultCommunityDBPath
        )
#endif
-----------------------------------------------------------------------------

newtype FilesDBOptions = FilesDBOptions
  { loadFilesDBFromOptions :: DBKind -> IO FilesDB
  }

#ifndef ALPM
filesDBOptionsParser :: Parser FilesDBOptions
filesDBOptionsParser =
  FilesDBOptions
    <$> ( ( flip fmap $
              strOption $
                long "files"
                  <> metavar "PATH"
                  <> short 'f'
                  <> help "Path of dir that includes core.files, extra.files and community.files"
                  <> showDefault
                  <> value defaultFilesDBDir
          )
            $ \s db -> do
              printInfo $ "Loading" <+> ppDBKind db <+> "files from" <+> pretty s
              loadFilesDB db s
        )
#else
filesDBOptionsParser :: Parser FilesDBOptions
filesDBOptionsParser =
  FilesDBOptions
    <$> ( ( flip fmap $
              flag
                True
                False
                ( long "no-alpm-files"
                    <> help "Do not use libalpm to parse files db"
                )
          )
            $ \b db -> do
              let src = if b then "libalpm" else defaultFilesDBDir
              printInfo $ "Loading" <+> ppDBKind db <+> "files from" <+> pretty src
              if b then loadFilesDBFFI db else loadFilesDB db defaultFilesDBDir
        )
#endif
-----------------------------------------------------------------------------

newtype HackageDBOptions = HackageDBOptions
  { loadHackageDBFromOptions :: IO HackageDB
  }

hackageDBOptionsParser :: Parser HackageDBOptions
hackageDBOptionsParser =
  HackageDBOptions
    <$> ( ( flip fmap $
              strOption $
                long "hackage"
                  <> metavar "PATH"
                  <> short 'h'
                  <> help "Path to hackage index tarball"
                  <> showDefault
                  <> value ""
          )
            $ \s -> do
              hackagePath <- if null s then lookupHackagePath else pure s
              printInfo $ "Loading hackage from" <+> pretty hackagePath
              loadHackageDB hackagePath
        )

-----------------------------------------------------------------------------
