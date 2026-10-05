module Plan.Args (Options (..), cmdOptions, runArgsParser) where

import Distribution.ArchHs.Internal.Prelude
import Distribution.ArchHs.Options
import Distribution.ArchHs.Types
import Distribution.ArchHs.Utils (archHsVersion)
import System.Exit (die)

data Options = Options
  { optFlags :: FlagAssignments,
    optExtraDB :: ExtraDBOptions,
    optHackage :: HackageDBOptions,
    optSolve :: Bool,
    optDebug :: Bool,
    optTargets :: [(PackageName, Maybe Version)]
  }

cmdOptions :: Parser (Either String Options)
cmdOptions =
  makeOptions
    <$> optFlagAssignmentParser
    <*> extraDBOptionsParser
    <*> hackageDBOptionsParser
    <*> switch (long "solve" <> help "Expand to blocking dependencies and reverse dependencies, minimizing release steps; supplied versions are minimums")
    <*> switch (long "debug" <> help "Show solver progress and metadata checks on stderr")
    <*> some (strArgument (metavar "TARGET [VERSION]..."))
  where
    makeOptions flags extra hackage solve debug targets =
      Options flags extra hackage solve debug <$> parsePackageTargets targets

runArgsParser :: IO Options
runArgsParser = do
  (result, ()) <-
    simpleOptions
      archHsVersion
      "arch-hs-plan - plan coordinated Haskell package updates"
      "Check candidate dependencies and repository reverse dependencies as one update set. An omitted VERSION selects the next preferred Hackage release, or the next stable upstream release for ghc. With --solve, automatically add blocking dependencies and reverse dependencies and try successively newer releases, minimizing total release steps. Without --solve, packages outside TARGETs stay at repository versions. Requesting ghc loads upstream bundled-library metadata and rechecks repository Haskell packages with the proposed compiler; otherwise the installed toolchain stays fixed. Uses latest local Cabal revisions; this checks metadata compatibility, not builds."
      cmdOptions
      empty
  either die pure result
