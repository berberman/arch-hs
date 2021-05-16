module Args
  ( Options (..),
    cmdOptions,
    runArgsParser,
  )
where

import qualified Data.Map as Map
import Distribution.ArchHs.Internal.Prelude
import Distribution.ArchHs.OptionReader
import Distribution.ArchHs.Options
import Distribution.ArchHs.Types
import Distribution.ArchHs.Utils (archHsVersion)
import Options.Applicative.Simple

data Options = Options
  { optFlags :: FlagAssignments,
    optCommunityDB :: CommunityDBOptions,
    optPackageName :: PackageName,
    optVersionA :: Version,
    optVersionB :: Version
  }

cmdOptions :: Parser Options
cmdOptions =
  Options
    <$> option
      optFlagReader
      ( long "flags"
          <> metavar "package_name:flag_name:true|false,..."
          <> short 'f'
          <> help "Flag assignments for packages - e.g. inline-c:gsl-example:true (separated by ',')"
          <> value Map.empty
      )
    <*> communityDBOptionsParser
    <*> argument optPackageNameReader (metavar "TARGET")
    <*> argument optVersionReader (metavar "VERSION_A")
    <*> argument optVersionReader (metavar "VERSION_B")

runArgsParser :: IO Options
runArgsParser =
  fst
    <$> simpleOptions
      archHsVersion
      "arch-hs-diff - create diff between different versions of package description"
      "arch-hs-diff is a CLI tool that shows the differences of package description between two versions of a package, and remind us if some required packages in community repo can't satisfy the version constraints, or they are non-existent"
      cmdOptions
      (pure ())
