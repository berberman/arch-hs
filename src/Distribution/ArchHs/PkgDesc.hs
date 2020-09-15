{-# LANGUAGE RecordWildCards #-}

-- | Copyright: (c) 2020 berberman
-- SPDX-License-Identifier: MIT
-- Maintainer: berberman <1793913507@qq.com>
-- This module provides parser of @desc@ file in pacman db.
module Distribution.ArchHs.PkgDesc
  ( PkgDesc (..),
    DescParser,
    descParser,
    descFieldsParser,
    runDescParser,
  )
where

import qualified Data.Map.Strict as Map
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

-- | A parser takes 'String' as input, without user state.
type DescParser = Parsec Void String

-- | Package description file of a installed system package,
-- which lies in @repo.db@ file.
data PkgDesc = PkgDesc
  { name :: String,
    version :: String,
    desc :: String,
    url :: String,
    license :: String,
    depends :: [String],
    makeDepends :: [String]
  }

-- Common fields
{- fieldList =
  [ "FILENAME",
    "NAME",
    "BASE",
    "VERSION",
    "DESC",
    "CSIZE",
    "ISIZE",
    "URL",
    "LICENSE",
    "ARCH",
    "BUILDDATE",
    "PACKAGER",
    "DEPENDS",
    "MAKEDEPENDS",
    "PROVIDES",
    "OPTDEPENDS",
    "REPLACES",
    "CONFLICTS"
  ] -}

-- | Parse fields of @desc@.
descFieldsParser :: DescParser (Map.Map String [String])
descFieldsParser =
  Map.fromList
    <$> ( do
            sep
            field <- many (upperChar <|> digitChar)
            sep
            _ <- newline
            content <- manyTill line (lookAhead sep <|> () <$ eol <|> eof)
            return (field, content)
        )
    `manyTill` eof
  where
    sep = () <$ char '%'
    line = manyTill anySingle newline

-- | Parse a desc file.
descParser :: DescParser PkgDesc
descParser =
  descFieldsParser
    >>= ( \fields -> do
            name <- lookupSingle fields "NAME"
            version <- lookupSingle fields "VERSION"
            desc <- lookupSingle fields "DESC"
            url <- lookupSingle fields "URL"
            license <- lookupSingle fields "LICENSE"
            depends <- lookupList fields "DEPENDS"
            makeDepends <- lookupList fields "MAKEDEPENDS"
            return PkgDesc {..}
        )
  where
    lookupSingle fields f = case Map.lookup f fields of
      (Just x) -> case x of
        (e : _) -> return e
        _ -> fail $ "Expect a singleton " <> f
      _ -> fail $ "Unable to find field " <> f
    lookupList fields f = return $ case Map.lookup f fields of
      (Just x) -> x
      _ -> []

-- | Run the desc parser.
runDescParser :: String -> Either (ParseErrorBundle String Void) PkgDesc
runDescParser = parse descParser "Desc"