{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Args
  ( Options (..),
    runArgsParser,
  )
where

import qualified Data.Map.Strict as Map
import Distribution.ArchHs.Internal.Prelude
import Distribution.ArchHs.OptionReader
import Distribution.ArchHs.Types

#ifndef ALPM
import Distribution.ArchHs.CommunityDB (defaultCommunityDBPath)
import Distribution.ArchHs.FilesDB (defaultFilesDBDir)
#endif

data Options = Options
  { optHackagePath :: FilePath,
#ifndef ALPM
    optCommunityDBPath :: FilePath,
    optFilesDBPath :: FilePath,
#endif
    optOutputDir :: FilePath,
    optFlags :: FlagAssignments,
    optSkip :: [String],
    optExtraCabalPath :: [FilePath],
    optAur :: Bool,
    optStdoutTrace :: Bool,
    optFileTrace :: FilePath,
    optUusi :: Bool,
    optForce :: Bool,
    optMetaDir :: FilePath,
    optJson :: FilePath,
#ifdef ALPM
    optAlpm :: Bool,
#endif
    optTarget :: PackageName
  }
  deriving stock (Show)

cmdOptions :: Parser Options
cmdOptions =
  Options
    <$> strOption
      ( long "hackage"
          <> metavar "PATH"
          <> short 'h'
          <> help "Path to hackage index tarball"
          <> showDefault
          <> value ""
      )
#ifndef ALPM
      <*> strOption
        ( long "community"
            <> metavar "PATH"
            <> short 'c'
            <> help "Path to community.db"
            <> showDefault
            <> value defaultCommunityDBPath
        )
      <*> strOption
        ( long "files"
            <> metavar "PATH"
            <> short 'f'
            <> help "Path of dir that includes core.files, extra.files and community.files"
            <> showDefault
            <> value defaultFilesDBDir
        )
#endif
      <*> strOption
        ( long "output"
            <> metavar "PATH"
            <> short 'o'
            <> help "Output path to generated PKGBUILD files (empty means dry run)"
            <> value ""
        )
      <*> option
        optFlagReader
        ( long "flags"
            <> metavar "package_name:flag_name:true|false,..."
            <> short 'f'
            <> help "Flag assignments for packages - e.g. inline-c:gsl-example:true (separated by ',')"
            <> value Map.empty
        )
      <*> option
        optSkippedReader
        ( long "skip"
            <> metavar "component_name,..."
            <> short 's'
            <> help "Skip a runnable component (executable, test suit, or benchmark) in dependency calculation"
            <> value []
        )
      <*> option
        optExtraCabalReader
        ( long "extra"
            <> metavar "PATH_1,..."
            <> short 'e'
            <> help "Extra cabal files' path - e.g. /home/berberman/arch-hs/arch-hs.cabal"
            <> value []
        )
      <*> switch
        ( long "aur"
            <> short 'a'
            <> help "Enable AUR searching"
        )
      <*> switch
        ( long "trace"
            <> help "Print trace to stdout"
        )
      <*> strOption
        ( long "trace-file"
            <> metavar "PATH"
            <> help "Path to trace file (empty means do not write trace to file)"
            <> value ""
        )
      <*> switch
        ( long "uusi"
            <> help "Splice uusi into prepare()"
        )
      <*> switch
        ( long "force"
            <> help "Try to package even if target is provided"
        )
      <*> strOption
        ( long "meta"
            <> metavar "PATH"
            <> help "Path to target meta package"
            <> value ""
        )
      <*> strOption
        ( long "json"
            <> metavar "PATH"
            <> help "Path to json output (empty means do not write output as json to file)"
            <> value ""
        )
#ifdef ALPM
      <*> switch
        ( long "alpm"
            <> help "Use libalpm to parse community db"
        )
#endif
      <*> argument optPackageNameReader (metavar "TARGET")

runArgsParser :: IO Options
runArgsParser =
  execParser $
    info
      (cmdOptions <**> helper)
      ( fullDesc
          <> progDesc "Try to reach the TARGET QAQ."
          <> header "arch-hs - a program generating PKGBUILD for hackage packages."
      )
