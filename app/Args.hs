{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Args
  ( Options (..),
    runArgsParser,
  )
where

import qualified Data.Map.Strict as Map
import Distribution.ArchHs.Types (FlagAssignments)
import Distribution.Types.PackageName (PackageName)
import OptionParse

data Options = Options
  { optHackagePath :: FilePath,
    optCommunityPath :: FilePath,
    optOutputDir :: FilePath,
    optFlags :: FlagAssignments,
    optSkip :: [String],
    optExtraCabalPath :: [FilePath],
    optAur :: Bool,
    optStdoutTrace :: Bool,
    optFileTrace :: FilePath,
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
          <> value "~/.cabal/packages/YOUR_HACKAGE_MIRROR/01-index.tar | 00-index.tar"
      )
      <*> strOption
        ( long "community"
            <> metavar "PATH"
            <> short 'c'
            <> help "Path to community.db"
            <> showDefault
            <> value "/var/lib/pacman/sync/community.db"
        )
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