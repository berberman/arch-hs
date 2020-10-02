{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Submit
  ( Options (..),
    runArgsParser,
    submit,
  )
where

import qualified Colourista as C
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Data.Void (Void)
import Distribution.ArchHs.Exception
import Distribution.ArchHs.Internal.Prelude
import Distribution.ArchHs.Local
import Distribution.ArchHs.Name
import Distribution.ArchHs.Types
import Network.HTTP.Req
import Options.Applicative hiding (header)
import qualified Options.Applicative
import qualified Text.Megaparsec as M
import Text.Megaparsec.Char as M

data Options = Options
  { optCommunityPath :: FilePath,
    optOutput :: FilePath,
    optUpload :: Bool
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
    <*> Options.Applicative.option
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

type DistroRecord = (String, String, String)

type DistroCSV = [DistroRecord]

renderDistroCSV :: DistroCSV -> String
renderDistroCSV = init . unlines . fmap (\(a, b, c) -> wrap a <> "," <> wrap b <> "," <> wrap c)
  where
    wrap x = "\"" <> x <> "\""

distroCSVParser :: M.Parsec Void String DistroCSV
distroCSVParser = M.sepBy distroRecordParser newline

distroRecordParser :: M.Parsec Void String DistroRecord
distroRecordParser =
  (M.between (M.char '"') (M.char '"') (M.many $ M.noneOf (",\"\n" :: String)) `M.sepBy` (M.char ',')) >>= \case
    (a : b : c : []) -> return (a, b, c)
    _ -> fail "Failed to parse record"

parseDistroCSV :: String -> DistroCSV
parseDistroCSV s = case M.parse distroCSVParser "DistroCSV" s of
  Left err -> fail $ M.errorBundlePretty err
  Right x -> x

genCSV :: Member CommunityEnv r => Sem r DistroCSV
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
         in (unPackageName hackageName, version, prefix <> communityName')
  return $ processField <$> fields

submit :: Members [CommunityEnv, WithMyErr, Embed IO] r => Maybe String -> FilePath -> Bool -> Sem r ()
submit token output upload = do
  csv <- genCSV
  let v = renderDistroCSV csv
  embed $
    when (not . null $ output) $ do
      C.infoMessage $ "Write file: " <> T.pack output
      writeFile output v
  check csv
  interceptHttpException $
    when (token /= Nothing && upload) $ do
      C.infoMessage "Uploading..."
      let api = https "hackage.haskell.org" /: "distro" /: "Arch" /: "packages"
          r =
            req PUT api (ReqBodyBs . BS.pack $ v) bsResponse $
              header "Authorization" (BS.pack $ "X-ApiKey " <> fromJust token) <> header "Content-Type" "text/csv"
      result <- runReq defaultHttpConfig r
      C.infoMessage $ "StatusCode: " <> (T.pack . show $ responseStatusCode result)
      C.infoMessage $ "ResponseMessage: " <> (decodeUtf8 $ responseStatusMessage result)
      C.infoMessage "ResponseBody:"
      putStrLn . BS.unpack $ responseBody result

check :: Members [WithMyErr, Embed IO] r => DistroCSV -> Sem r ()
check community = do
  let api = https "hackage.haskell.org" /: "distro" /: "Arch" /: "packages.csv"
      r = req GET api NoReqBody bsResponse mempty
  embed $ C.infoMessage "Downloading csv from hackage..."
  result <- interceptHttpException $ runReq defaultHttpConfig r
  let bs = responseBody result
      hackage = parseDistroCSV . T.unpack $ decodeUtf8 bs

  let diffOld = hackage \\ community
      diffNew = community \\ hackage
      ppRecord b (name, version, url) = (if b then C.formatWith [C.green] else C.formatWith [C.red]) $ "(" <> name <> ", " <> version <> ", " <> url <> ")"

  embed $ case (diffNew <> diffOld) of
    [] -> return ()
    _ -> do
      putStrLn $ C.formatWith [C.magenta] "Diff:"
      putStr . unlines $ fmap (ppRecord False) diffOld
      putStrLn $ replicate 68 '-'
      putStr . unlines $ fmap (ppRecord True) diffNew

  embed . putStrLn $ "Found " <> show (length hackage) <> " packages with submitted distribution information in hackage, and " <> show (length community) <> " haskell packages in community."
