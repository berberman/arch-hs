module Args
  ( CommonOptions (..),
    commonOptionsParser,
    SubmitOptions (..),
    submitOptionsParser,
    Mode (..),
    runArgsParser,
  )
where

import Distribution.ArchHs.Options
import Distribution.ArchHs.Utils (archHsVersion)
import Options.Applicative.Simple

-----------------------------------------------------------------------------

data CommonOptions = CommonOptions
  { optHackage :: HackageDBOptions,
    optCommunityDB :: CommunityDBOptions
  }

commonOptionsParser :: Parser CommonOptions
commonOptionsParser =
  CommonOptions <$> hackageDBOptionsParser <*> communityDBOptionsParser

-----------------------------------------------------------------------------

data SubmitOptions = SubmitOptions
  { optCommon :: CommonOptions,
    optOutput :: FilePath,
    optUpload :: Bool
  }

submitOptionsParser :: Parser SubmitOptions
submitOptionsParser =
  SubmitOptions
    <$> commonOptionsParser
    <*> option
      str
      ( long "output"
          <> metavar "PATH"
          <> short 'o'
          <> help "Output path of generated .csv file"
          <> value ""
      )
    <*> switch
      ( long "upload"
          <> short 'u'
          <> help "Upload to hackage"
      )

-----------------------------------------------------------------------------

data Mode = Submit SubmitOptions | Check CommonOptions

runArgsParser :: IO Mode
runArgsParser =
  snd <$> do
    simpleOptions
      archHsVersion
      "arch-hs-sync - sync metadata of Haskell packages between [community] and Hackage"
      "arch-hs-sync is designed to be used by Hackage distribution maintainers"
      (pure ())
      $ do
        addCommand
          "submit"
          "submit distribution information to Hackage"
          Submit
          submitOptionsParser
        addCommand
          "check"
          "check inconsistencies of Haskell packages version"
          Check
          commonOptionsParser
