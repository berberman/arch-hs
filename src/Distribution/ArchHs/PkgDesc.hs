{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}

-- | Copyright: (c) 2020-2021 berberman
-- SPDX-License-Identifier: MIT
-- Maintainer: berberman <berberman@yandex.com>
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
    promoteDependent,
    containsDep,
  )
where

import Control.Monad (void)
import qualified Data.Map.Strict as Map
import Data.Void (Void)
import Distribution.ArchHs.Internal.Prelude
import Distribution.ArchHs.Types
import Distribution.ArchHs.Utils (extractFromEVR)
import Text.Megaparsec
import Text.Megaparsec.Char

-- | A parser takes 'String' as input, without user state.
type DescParser = Parsec Void String

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

-- | Promote a versioned dependent to package description
promoteDependent :: PkgDependent -> Maybe PkgDesc
promoteDependent PkgDependent {..} =
  ( \ver ->
      PkgDesc
        { _name = _pdName,
          _version = ver,
          _desc = "",
          _url = Nothing,
          _provides = [],
          _optDepends = [],
          _replaces = [],
          _conflicts = [],
          _makeDepends = [],
          _depends = [],
          _checkDepends = []
        }
  )
    <$> _pdVersion

-- | Check if a name is in 'PkgDependentList'
containsDep :: PkgDependentList -> ArchLinuxName -> Bool
containsDep deps name = name `elem` (_pdName <$> deps)

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
    sep = void $ char '%'
    line = manyTill anySingle newline

-- | Parse a desc file.
descParser :: DescParser PkgDesc
descParser =
  descFieldsParser
    >>= ( \fields -> do
            _name <- ArchLinuxName <$> lookupSingle fields "NAME"
            _version <- extractFromEVR <$> lookupSingle fields "VERSION"
            _desc <- lookupSingle fields "DESC"
            _url <- lookupSingleMaybe fields "URL"
            _depends <- toDepList =<< lookupList fields "DEPENDS"
            _makeDepends <- toDepList =<< lookupList fields "MAKEDEPENDS"
            _provides <- toDepList =<< lookupList fields "PROVIDES"
            _optDepends <- toDepList =<< lookupList fields "OPTDEPENDS"
            _replaces <- toDepList =<< lookupList fields "REPLACES"
            _conflicts <- toDepList =<< lookupList fields "CONFLICTS"
            _checkDepends <- toDepList =<< lookupList fields "CHECKDEPENDS"
            return PkgDesc {..}
        )
  where
    toDepList = mapM $ \t -> case splitOn "=" t of
      [name, version] -> pure $ PkgDependent (ArchLinuxName name) (Just version)
      [name] -> pure $ PkgDependent (ArchLinuxName name) Nothing
      _ -> fail $ "Unable to parse dep list " <> t
    lookupSingle fields f = case Map.lookup f fields of
      (Just x) -> case x of
        (e : _) -> return e
        _ -> fail $ "Expect a singleton " <> f
      _ -> fail $ "Unable to find field " <> f
    lookupSingleMaybe fields f = return $ case Map.lookup f fields of
      (Just x) -> case x of
        (e : _) -> Just e
        _ -> Nothing
      _ -> Nothing
    lookupList fields f = return $ case Map.lookup f fields of
      (Just x) -> x
      _ -> []

-- | Run the desc fields parser.
runDescFieldsParser :: String -> String -> Either (ParseErrorBundle String Void) (Map.Map String [String])
runDescFieldsParser = parse descFieldsParser

-- | Run the desc parser.
runDescParser :: String -> String -> Either (ParseErrorBundle String Void) PkgDesc
runDescParser = parse descParser
