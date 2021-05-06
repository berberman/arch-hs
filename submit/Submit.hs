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
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
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
import Network.HTTP.Client
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
      <*> flag True False
        ( long "no-alpm"
            <> help "Not to use libalpm to parse community db"
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

genCSV :: Members [CommunityEnv, HackageEnv, Embed IO] r => Sem r DistroCSV
genCSV = do
  communityDb <- ask @CommunityDB
  notLinked <- getAndPrintNotLinked

  let communityPackages = Map.toList communityDb
      inHackage = (`notElem` notLinked)
      fields =
        communityPackages
          ^.. each
            . filtered (((&&) <$> inHackage <*> isHaskellPackage) . (^. _1))
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

submit :: Members [HackageEnv, CommunityEnv, WithMyErr, Reader Manager, Embed IO] r => Maybe String -> FilePath -> Bool -> Sem r ()
submit token output upload = do
  csv <- genCSV
  let v = renderDistroCSV csv
  embed $
    unless (null output) $ do
      printInfo $ "Write file" <> colon <+> pretty output
      writeFile output v
  check csv
  manager <- ask
  interceptHttpException $
    when ((not . null) token && upload) $ do
      printInfo "Uploading..."
      initialRequest <- parseRequest "https://hackage.haskell.org/distro/Arch/packages"
      let req =
            initialRequest
              { method = "PUT",
                requestBody = RequestBodyBS . BS.pack $ v,
                requestHeaders =
                  [ ("Authorization", "X-ApiKey " <> BS.pack (fromJust token)),
                    ("Content-Type", "text/csv")
                  ]
              }
      result <- httpLbs req manager
      printInfo $ "Status" <> colon <+> viaShow (responseStatus result)
      printInfo "ResponseBody:"
      printInfo . pretty . decodeUtf8 . LBS.toStrict $ responseBody result

check :: Members [HackageEnv, WithMyErr, Reader Manager, Embed IO] r => DistroCSV -> Sem r ()
check community = do
  printInfo "Checking generated csv file..."

  req <- interceptHttpException $ parseRequest "https://hackage.haskell.org/distro/Arch/packages.csv"
  printInfo "Downloading csv from hackage..."
  manager <- ask
  result <- interceptHttpException $ httpLbs req manager
  let bs = responseBody result
      hackage = parseDistroCSV . T.unpack . decodeUtf8 . LBS.toStrict $ bs

  let diff = getGroupedDiff hackage community
      diffOld = filterFirstDiff diff >>= (ppDiffColored . mapDiff (fmap ppRecord))
      diffNew = filterSecondDiff diff >>= (ppDiffColored . mapDiff (fmap ppRecord))
      ppRecord (name, version, url) = "(" <> name <> ", " <> version <> ", " <> url <> ")"
      j g x = if null x then "[]" else g x
  embed . putDoc . annMagneta $ "Diff" <> colon <> line
  embed . putDoc . indent 2 $
    if noDiff diff
      then "[]"
      else
        j cat diffOld
          <> splitLine
          <> j cat diffNew
          <> line

  embed . putDoc $
    "Found"
      <+> pretty (length hackage)
      <+> "packages with submitted distribution information in hackage, and"
      <+> pretty (length community)
      <+> "haskell packages in"
      <+> ppCommunity
      <> dot
      <> line

getAndPrintNotLinked :: Members [CommunityEnv, HackageEnv, Embed IO] r => Sem r [ArchLinuxName]
getAndPrintNotLinked = do
  communityHaskellPackages <- filter isHaskellPackage . Map.keys <$> ask @CommunityDB
  hackagePackages <- Map.keys <$> ask @HackageDB
  let result = filter (\x -> toHackageName x `notElem` hackagePackages) communityHaskellPackages

  unless (null result) $
    printWarn $ "Following packages in" <+> ppCommunity <+> "are not linked to hackage:"

  embed . putStr . unlines $ unArchLinuxName <$> result
  return result
