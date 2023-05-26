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
    optExtraDB :: ExtraDBOptions,
    optPackageName :: PackageName,
    optVersionA :: Version,
    optVersionB :: Version
  }

cmdOptions :: Parser Options
cmdOptions =
  Options
    <$> optFlagAssignmentParser
    <*> extraDBOptionsParser
    <*> argument optPackageNameReader (metavar "TARGET")
    <*> argument optVersionReader (metavar "VERSION_A")
    <*> argument optVersionReader (metavar "VERSION_B")

runArgsParser :: IO Options
runArgsParser = do
  (x, ()) <-
    simpleOptions
      archHsVersion
      "arch-hs-diff - create diff between different versions of package description"
      "arch-hs-diff is a CLI tool that shows the differences of package description between two versions of a package, and remind us if some required packages in extra repo can't satisfy the version constraints, or they are non-existent"
      cmdOptions
      empty
  pure x
