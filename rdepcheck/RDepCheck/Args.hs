module RDepCheck.Args
  ( Options (..),
    cmdOptions,
    runArgsParser,
  )
where

import Distribution.ArchHs.Internal.Prelude
import Distribution.ArchHs.Options
import Distribution.ArchHs.Types
import Distribution.ArchHs.Utils (archHsVersion)
import System.Exit (die)

data Options = Options
  { optFlags :: FlagAssignments,
    optExtraDB :: ExtraDBOptions,
    optHackage :: HackageDBOptions,
    optTargets :: [(PackageName, Maybe Version)]
  }

cmdOptions :: Parser (Either String Options)
cmdOptions =
  makeOptions
    <$> optFlagAssignmentParser
    <*> extraDBOptionsParser
    <*> hackageDBOptionsParser
    <*> some (strArgument (metavar "TARGET [VERSION]..."))
  where
    makeOptions flags extra hackage targets =
      Options flags extra hackage <$> parseTargets targets

parseTargets :: [String] -> Either String [(PackageName, Maybe Version)]
parseTargets [] = Right []
parseTargets (target : rest) =
  case simpleParsec target of
    Nothing -> Left $ "Invalid target package name: " <> target
    Just name -> case rest of
      version : remaining | Just candidate <- simpleParsec version ->
        ((name, Just candidate) :) <$> parseTargets remaining
      _ -> ((name, Nothing) :) <$> parseTargets rest

runArgsParser :: IO Options
runArgsParser = do
  (x, ()) <-
    simpleOptions
      archHsVersion
      "arch-hs-rdepcheck - inspect reverse dependency version ranges"
      "arch-hs-rdepcheck shows reverse dependencies of one or more Haskell packages in [extra] and the version ranges they require. Each TARGET may be followed by a VERSION to check. It compares the latest cabal revision with revision 0 and shows both when their ranges differ. If VERSION is provided, it counts newly unmet ranges as rdep and already unmet ranges as rdep-old. The combined counts and exit status use the latest revision, with failure only for newly unmet ranges."
      cmdOptions
      empty
  either die pure x
