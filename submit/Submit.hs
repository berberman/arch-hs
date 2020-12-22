{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Submit
  ( Options (..),
    runArgsParser,
    submit,
  )
where

import Control.Monad (unless)
import Data.Algorithm.Diff (getGroupedDiff)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromJust)
import qualified Data.Text as T
import Data.Void (Void)
import Distribution.ArchHs.Exception
import Distribution.ArchHs.Hackage
import Distribution.ArchHs.Internal.Prelude
import Distribution.ArchHs.Local
import Distribution.ArchHs.Name
import Distribution.ArchHs.PP
import Distribution.ArchHs.Types
import Distribution.ArchHs.Utils (filterFirstDiff, filterSecondDiff, mapDiff, noDiff)
import Network.HTTP.Req
import Options.Applicative hiding (header)
import qualified Options.Applicative
import qualified Text.Megaparsec as M
import Text.Megaparsec.Char as M

#ifndef ALPM
import Distribution.ArchHs.CommunityDB (defaultCommunityDBPath)
#endif

data Options = Options
  { optHackagePath :: FilePath,
#ifndef ALPM
    optCommunityDBPath :: FilePath,
#else
    optAlpm :: Bool,
#endif
    optOutput :: FilePath,
    optUpload :: Bool
  }

cmdOptions :: Parser Options
cmdOptions =
  Options
    <$> strOption
      ( long "hackage"
          <> metavar "PATH"
          <> short 'h'
          <> help "Path to hackage index tarball"
          <> showDefault
          <> value ""
      )
#ifndef ALPM
    <*> strOption
      ( long "community"
          <> metavar "PATH"
          <> short 'c'
          <> help "Path to community.db"
          <> showDefault
          <> value defaultCommunityDBPath
      )
#else
      <*> switch
        ( long "alpm"
            <> help "Use libalpm to parse community db"
        )
#endif
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
  (M.between (M.char '"') (M.char '"') (M.many $ M.noneOf (",\"\n" :: String)) `M.sepBy` M.char ',') >>= \case
    [a, b, c] -> return (a, b, c)
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
            . filtered (isHaskellPackage . (^. _1))
          <&> (_1 <<%~ toHackageName)
          & sortBy (\x y -> (x ^. _2 . _1) `compare` (y ^. _2 . _1))
      prefix = "https://www.archlinux.org/packages/community/x86_64/"
      processField (archLinuxName, (hackageName, version)) =
        let archLinuxName' =
              if hackageName `elem` ghcLibList || hackageName == "ghc"
                then "ghc"
                else unArchLinuxName archLinuxName
         in (unPackageName hackageName, version, prefix <> archLinuxName')
  return $ processField <$> fields

submit :: Members [HackageEnv, CommunityEnv, WithMyErr, Embed IO] r => Maybe String -> FilePath -> Bool -> Sem r ()
submit token output upload = do
  csv <- genCSV
  let v = renderDistroCSV csv
  embed $
    unless (null output) $ do
      printInfo $ "Write file: " <> T.pack output
      writeFile output v
  check csv
  interceptHttpException $
    when ((not . null) token && upload) $ do
      printInfo "Uploading..."
      let api = https "hackage.haskell.org" /: "distro" /: "Arch" /: "packages"
          r =
            req PUT api (ReqBodyBs . BS.pack $ v) bsResponse $
              header "Authorization" (BS.pack $ "X-ApiKey " <> fromJust token) <> header "Content-Type" "text/csv"
      result <- runReq defaultHttpConfig r
      printInfo $ "StatusCode: " <> (T.pack . show $ responseStatusCode result)
      printInfo $ "ResponseMessage: " <> decodeUtf8 (responseStatusMessage result)
      printInfo "ResponseBody:"
      putStrLn . BS.unpack $ responseBody result

check :: Members [HackageEnv, WithMyErr, Embed IO] r => DistroCSV -> Sem r ()
check community = do
  printInfo "Checking generated csv file..."

  let hackageNames = fmap (\(a, _, _) -> a) community
      f = fmap (\case Left (PkgNotFound x) -> Just (unArchLinuxName $ toArchLinuxName x); _ -> Nothing)

  failed <- catMaybes . f <$> mapM (\x -> try @MyException (getLatestCabal $ mkPackageName x)) hackageNames

  unless (null failed) $
    printWarn "Following packages in community are not linked to hackage:"

  embed . putStrLn . unlines $ failed

  let api = https "hackage.haskell.org" /: "distro" /: "Arch" /: "packages.csv"
      r = req GET api NoReqBody bsResponse mempty
  printInfo "Downloading csv from hackage..."
  result <- interceptHttpException $ runReq defaultHttpConfig r
  let bs = responseBody result
      hackage = parseDistroCSV . T.unpack $ decodeUtf8 bs

  let diff = getGroupedDiff hackage community
      diffOld = mconcat . ppDiffColored . mapDiff (fmap ppRecord) <$> filterFirstDiff diff
      diffNew = mconcat . ppDiffColored . mapDiff (fmap ppRecord) <$> filterSecondDiff diff
      ppRecord (name, version, url) = "(" <> name <> ", " <> version <> ", " <> url <> ")\n"
      j g x = if null x then "[]" else g x

  embed . putDoc . annMagneta $ "Diff" <> colon <> line
  embed . putDoc . indent 2 $
    if noDiff diff
      then "[]"
      else
        j hsep diffOld
          <> splitLine
          <> j hsep diffNew

  embed . putDoc $
    "Found"
      <+> pretty (length hackage)
      <+> "packages with submitted distribution information in hackage, and"
      <+> pretty (length community)
      <+> "haskell packages in"
      <+> ppCommunity
      <> dot
      <> line
