module Args
  ( CommonOptions (..),
    SubmitOptions (..),
    CheckOptions (..),
    ListOptions (..),
    Mode (..),
    runArgsParser,
  )
where

import Distribution.ArchHs.Options
import Distribution.ArchHs.Utils (archHsVersion)

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
  { optOutput :: FilePath,
    optUpload :: Bool
  }

submitOptionsParser :: Parser SubmitOptions
submitOptionsParser =
  SubmitOptions
    <$> option
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

newtype CheckOptions = CheckOptions
  { optShowGHCLibs :: Bool
  }

checkOptionsParser :: Parser CheckOptions
checkOptionsParser =
  CheckOptions
    <$> switch (long "show-ghc-libs" <> help "Include GHC and GHC libs")

-----------------------------------------------------------------------------

newtype ListOptions = ListOptions
  { optWithVersion :: Bool
  }

listOptionsParser :: Parser ListOptions
listOptionsParser =
  ListOptions
    <$> switch (short 'v' <> long "with-version" <> help "Show package version")

-----------------------------------------------------------------------------

data Mode
  = Submit CommonOptions SubmitOptions
  | Check CommonOptions CheckOptions
  | List CommunityDBOptions ListOptions

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
          id
          (Submit <$> commonOptionsParser <*> submitOptionsParser)
        addCommand
          "check"
          "check inconsistencies of Haskell packages version"
          id
          (Check <$> commonOptionsParser <*> checkOptionsParser)
        addCommand
          "list"
          "list all Haskell packages in [community]"
          id
          (List <$> communityDBOptionsParser <*> listOptionsParser)
