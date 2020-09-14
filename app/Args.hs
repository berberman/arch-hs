{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Args
  ( Options (..),
    runArgsParser,
  )
where

import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Void (Void)
import Options.Applicative
import System.FilePath (takeExtension)
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M

data Options = Options
  { optHackagePath :: FilePath,
    optCommunityPath :: FilePath,
    optOutputDir :: FilePath,
    optFlags :: [(String, String, Bool)],
    optSkip :: [String],
    optExtraCabalPath :: [FilePath],
    optAur :: Bool,
    optTarget :: String
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
            <> value []
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
      <*> strArgument (metavar "TARGET")

optFlagReader :: ReadM [(String, String, Bool)]
optFlagReader =
  eitherReader
    ( \s -> case M.parse optFlagParser "" s of
        Right x -> Right x
        Left err -> Left $ M.errorBundlePretty err
    )

optFlagParser :: M.Parsec Void String [(String, String, Bool)]
optFlagParser =
  ( do
      pkg <- M.manyTill M.anySingle $ M.single ':'
      flg <- M.manyTill M.anySingle $ M.single ':'
      b <- bool
      return (pkg, flg, b)
  )
    `M.sepBy` ","
  where
    bool = do
      s <- M.string "true" <|> M.string "false"
      case s of
        "true" -> return True
        "false" -> return False
        _ -> fail $ "unknown bool: " <> s

optSkippedReader :: ReadM [String]
optSkippedReader = eitherReader $ Right . splitOn ","

optExtraCabalReader :: ReadM [FilePath]
optExtraCabalReader = eitherReader $ \x ->
  let splitted = splitOn "," x
      check = map (\e -> if takeExtension x == ".cabal" then (e, True) else (e, False)) splitted
      failed = map fst . filter (not . snd) $ check
      successful = map fst . filter snd $ check
   in if failed /= [] then Left ("Unexpected file name: " <> intercalate ", " failed) else Right successful

runArgsParser :: IO Options
runArgsParser =
  execParser $
    info
      (cmdOptions <**> helper)
      ( fullDesc
          <> progDesc "Try to reach the TARGET QAQ."
          <> header "arch-hs - a program generating PKGBUILD for hackage packages."
      )