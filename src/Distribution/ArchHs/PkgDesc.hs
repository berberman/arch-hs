{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards    #-}

-- | Copyright: (c) 2020 berberman
-- SPDX-License-Identifier: MIT
-- Maintainer: berberman <1793913507@qq.com>
-- Stability: experimental
-- Portability: portable
-- This module provides parser of @desc@ file in pacman db.
module Distribution.ArchHs.PkgDesc
  ( PkgDesc (..),
    DescParser,
    descParser,
    descFieldsParser,
    runDescFieldsParser,
    runDescParser,
  )
where

import qualified Data.Map.Strict                      as Map
import           Data.Void                            (Void)
import           Distribution.ArchHs.Internal.Prelude
import           Text.Megaparsec
import           Text.Megaparsec.Char

-- | A parser takes 'String' as input, without user state.
type DescParser = Parsec Void String

-- | Package description file of a installed system package,
-- which lies in @repo.db@ file.
data PkgDesc = PkgDesc
  { _name        :: String,
    _version     :: String,
    _desc        :: String,
    _url         :: Maybe String,
    _license     :: Maybe String,
    _provides    :: [String],
    _optDepends  :: [String],
    _replaces    :: [String],
    _conflicts   :: [String],
    _depends     :: [String],
    _makeDepends :: [String]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

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
            field <- manyTill anySingle sep
            _ <- newline
            content <- manyTill line (lookAhead sep <|> eof)
            return (field, filter (/= "") content)
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
            _name <- lookupSingle fields "NAME"
            _version <- lookupSingle fields "VERSION"
            _desc <- lookupSingle fields "DESC"
            _url <- lookupSingleMaybe fields "URL"
            _license <- lookupSingleMaybe fields "LICENSE"
            _depends <- lookupList fields "DEPENDS"
            _makeDepends <- lookupList fields "MAKEDEPENDS"
            _provides <- lookupList fields "PROVIDES"
            _optDepends <- lookupList fields "OPTDEPENDS"
            _replaces <- lookupList fields "REPLACES"
            _conflicts <- lookupList fields "CONFLICTS"
            return PkgDesc {..}
        )
  where
    lookupSingle fields f = case Map.lookup f fields of
      (Just x) -> case x of
        (e : _) -> return e
        _       -> fail $ "Expect a singleton " <> f
      _ -> fail $ "Unable to find field " <> f
    lookupSingleMaybe fields f = return $ case Map.lookup f fields of
      (Just x) -> case x of
        (e : _) -> Just e
        _       -> Nothing
      _ -> Nothing
    lookupList fields f = return $ case Map.lookup f fields of
      (Just x) -> x
      _        -> []

-- | Run the desc fields parser.
runDescFieldsParser :: String -> String -> Either (ParseErrorBundle String Void) (Map.Map String [String])
runDescFieldsParser = parse descFieldsParser

-- | Run the desc parser.
runDescParser :: String -> String -> Either (ParseErrorBundle String Void) PkgDesc
runDescParser = parse descParser
