{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Args
  ( Options (..),
    runArgsParser,
  )
where

import Distribution.ArchHs.Internal.Prelude
import Distribution.ArchHs.Options
import Distribution.ArchHs.Types
import Distribution.ArchHs.Utils (archHsVersion)

data Options = Options
  { optHackage :: HackageDBOptions,
    optCommunityDB :: CommunityDBOptions,
    optFilesDB :: FilesDBOptions,
    optOutputDir :: FilePath,
    optFlags :: FlagAssignments,
    optSkip :: [String],
    optExtraCabalDirs :: [FilePath],
    optAur :: Bool,
    optStdoutTrace :: Bool,
    optFileTrace :: FilePath,
    optUusi :: Bool,
    optForce :: Bool,
    optMetaDir :: FilePath,
    optJson :: FilePath,
    optTarget :: PackageName
  }

cmdOptions :: Parser Options
cmdOptions =
  Options
    <$> hackageDBOptionsParser
      <*> communityDBOptionsParser
      <*> filesDBOptionsParser
      <*> strOption
        ( long "output"
            <> metavar "PATH"
            <> short 'o'
            <> help "Output path to generated PKGBUILD files (empty means dry run)"
            <> value ""
        )
      <*> optFlagAssignmentParser
      <*> many
        ( strOption
            ( long "skip"
                <> metavar "component_name"
                <> short 's'
                <> help "Skip a runnable component (executable, test suit, or benchmark) in dependency calculation"
            )
        )
      <*> many
        ( strOption
            ( long "extra"
                <> metavar "PATH_1,..."
                <> short 'e'
                <> help "Paths to directories that contain extra cabal files to include - e.g. /home/berberman/arch-hs"
            )
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
      <*> argument optPackageNameReader (metavar "TARGET")

runArgsParser :: IO Options
runArgsParser = do
  (x, ()) <-
    simpleOptions
      archHsVersion
      "arch-hs - generate PKGBUILD for Haskell packages in Hackage"
      "arch-hs is a CLI tool automating the PKGBUILD generation for Haskell packages, with dependency resolving and template filling"
      cmdOptions
      empty
  pure x
