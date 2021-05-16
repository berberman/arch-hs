{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Submit.CSV
  ( DistroRecord,
    DistroCSV,
    renderDistroCSV,
    parseDistroCSV,
  )
where

import Data.Void (Void)
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M

type DistroRecord = (String, String, String)

type DistroCSV = [DistroRecord]

renderDistroCSV :: DistroCSV -> String
renderDistroCSV = init . unlines . fmap (\(a, b, c) -> wrap a <> "," <> wrap b <> "," <> wrap c)
  where
    wrap x = "\"" <> x <> "\""

distroCSVParser :: M.Parsec Void String DistroCSV
distroCSVParser = M.sepBy distroRecordParser M.newline

distroRecordParser :: M.Parsec Void String DistroRecord
distroRecordParser =
  (M.between (M.char '"') (M.char '"') (M.many $ M.noneOf (",\"\n" :: String)) `M.sepBy` M.char ',') >>= \case
    [a, b, c] -> return (a, b, c)
    _ -> fail "Failed to parse record"

parseDistroCSV :: String -> DistroCSV
parseDistroCSV s = case M.parse distroCSVParser "DistroCSV" s of
  Left err -> fail $ M.errorBundlePretty err
  Right x -> x
