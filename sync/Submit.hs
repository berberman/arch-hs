{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Submit (submit) where

import Control.Monad (unless)
import Data.Algorithm.Diff (getGroupedDiff)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import Data.List (sortOn)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Distribution.ArchHs.Exception
import Distribution.ArchHs.Internal.Prelude
import Distribution.ArchHs.Name (isGHCLibs)
import Distribution.ArchHs.PP
import Distribution.ArchHs.Types
import Distribution.ArchHs.Utils
import Distribution.Package (packageName)
import Network.HTTP.Client
import Submit.CSV
import Utils

-- | Generate Distro CSV from Community DB
genCSV :: Members [CommunityEnv, HackageEnv, WithMyErr, Embed IO] r => Sem r DistroCSV
genCSV = do
  linked <- linkedHaskellPackages
  pure $
    sortOn
      (^. _1)
      [ (unPackageName hackageName, version, prefix <> tweakedName)
        | (archLinuxName, version, packageName -> hackageName) <- linked,
          let tweakedName =
                if isGHCLibs hackageName
                  then "ghc"
                  else unArchLinuxName archLinuxName
              prefix = "https://www.archlinux.org/packages/community/x86_64/"
      ]

-- | Check and submit distro CSV to Hackage
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

-- | Download Distro CSV from Hackage and print differences from @community@
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
