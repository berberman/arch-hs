{-# LANGUAGE OverloadedStrings #-}

module Submit
  ( Options (..),
    runArgsParser,
    submit,
  )
where

import qualified Colourista                           as C
import qualified Control.Exception                    as CE
import qualified Data.ByteString.Char8                as BS
import qualified Data.Map.Strict                      as Map
import           Data.Maybe                           (fromJust)
import qualified Data.Text                            as T
import           Distribution.ArchHs.Internal.Prelude
import           Distribution.ArchHs.Local
import           Distribution.ArchHs.Name
import           Distribution.ArchHs.Types
import           Network.HTTP.Req
import           Options.Applicative                  hiding (header)
import qualified Options.Applicative
import           Text.CSV

data Options = Options
  { optCommunityPath :: FilePath,
    optOutput        :: FilePath,
    optUpload        :: Bool
  }

cmdOptions :: Parser Options
cmdOptions =
  Options
    <$> strOption
      ( long "community"
          <> metavar "PATH"
          <> short 'c'
          <> help "Path to community.db"
          <> showDefault
          <> value "/var/lib/pacman/sync/community.db"
      )
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

runArgsParser :: IO Options
runArgsParser =
  execParser $
    info
      (cmdOptions <**> helper)
      ( fullDesc
          <> progDesc "Try to reach the TARGET QAQ."
          <> Options.Applicative.header "arch-hs-submit - a program submitting haskell packages in community to hackage arch distro."
      )

genCSV :: Member CommunityEnv r => Sem r CSV
genCSV = do
  db <- ask @CommunityDB
  let communityPackages = Map.toList db
      fields =
        communityPackages
          ^.. each
            . filtered (\(name, _) -> isHaskellPackage name)
            & mapped
            %~ _1
            <<%~ toHackageName
            & sortBy (\x y -> (x ^. _2 . _1) `compare` (y ^. _2 . _1))

      prefix = "https://www.archlinux.org/packages/community/x86_64/"
      processField (communityName, (hackageName, version)) =
        let communityName' =
              if (hackageName `elem` ghcLibList || hackageName == "ghc")
                then "ghc"
                else unCommunityName communityName
         in [unPackageName hackageName, version, prefix <> communityName']
  return $ processField <$> fields

submit :: Members [CommunityEnv, Embed IO] r => Maybe String -> FilePath -> Bool -> Sem r ()
submit token output upload = do
  v <- printCSV <$> genCSV
  embed $
    when (not . null $ output) $ do
      C.infoMessage $ "Write file: " <> T.pack output
      writeFile output v
  embed $
    CE.catch @HttpException
      ( when (token /= Nothing && upload) $ do
          C.infoMessage "Uploading..."
          let api = https "hackage.haskell.org" /: "distro" /: "Arch" /: "packages"
              r =
                req PUT api (ReqBodyBs . BS.pack $ v) bsResponse $
                  header "X-ApiKey" (BS.pack . fromJust $ token) <> header "Content-Type" "text/csv"
          result <- runReq defaultHttpConfig r
          C.infoMessage $ "StatusCode: " <> (T.pack . show $ responseStatusCode result)
          C.infoMessage $ "ResponseMessage: " <> (decodeUtf8 $ responseStatusMessage result)
          C.infoMessage "ResponseBody:"
          putStrLn . BS.unpack $ responseBody result
      )
      $ \e -> C.errorMessage $ "HttpException: " <> (T.pack . show $ e)
  return ()
