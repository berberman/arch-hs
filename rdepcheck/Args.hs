module Args
  ( Options (..),
    cmdOptions,
    runArgsParser,
  )
where

import Distribution.ArchHs.Internal.Prelude
import Distribution.ArchHs.Options
import Distribution.ArchHs.Types
import Distribution.ArchHs.Utils (archHsVersion)

data Options = Options
  { optFlags :: FlagAssignments,
    optCommunityDB :: CommunityDBOptions,
    optHackage :: HackageDBOptions,
    optPackageName :: PackageName
  }

cmdOptions :: Parser Options
cmdOptions =
  Options
    <$> optFlagAssignmentParser
    <*> communityDBOptionsParser
    <*> hackageDBOptionsParser
    <*> argument optPackageNameReader (metavar "TARGET")

runArgsParser :: IO Options
runArgsParser = do
  (x, ()) <-
    simpleOptions
      archHsVersion
      "arch-hs-rdepcheck - check the version of a dependent Haskell package"
      "arch-hs-rdepcheck is a CLI tool that shows all reverse dependencies of a Haskell package in [community], giving the version range how it is depended on"
      cmdOptions
      empty
  pure x
