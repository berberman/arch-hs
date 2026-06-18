{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Copyright: (c) 2020-2021 berberman
-- SPDX-License-Identifier: MIT
-- Maintainer: berberman <berberman@yandex.com>
-- Stability: experimental
-- Portability: portable
-- This module provides parser of @desc@ file in pacman db.
module Distribution.ArchHs.PkgDesc
  ( PkgDesc (..),
    parseDescEntry,
    parseDescFields,
    promoteDependent,
    containsDep,
  )
where

import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import qualified Data.Text as T
import Distribution.ArchHs.Internal.Prelude
import Distribution.ArchHs.Types
import Distribution.ArchHs.Utils (extractFromEVR)

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

-- | Parse fields of a pacman @desc@ file.
parseDescFields :: T.Text -> Map.Map T.Text [T.Text]
parseDescFields = go Map.empty . dropWhile T.null . T.lines
  where
    go acc [] = acc
    go acc (line : rest)
      | Just field <- fieldName line =
        let (content, rest') = break (isJust . fieldName) rest
         in go (Map.insert field (filter (not . T.null) content) acc) (dropWhile T.null rest')
      | otherwise = go acc rest

    fieldName line
      | Just rest <- T.stripPrefix "%" line,
        Just field <- T.stripSuffix "%" rest =
        Just field
      | otherwise = Nothing

-- | Parse a pacman @desc@ file into a package description.
parseDescEntry :: T.Text -> Maybe PkgDesc
parseDescEntry txt = do
  _name <- ArchLinuxName . T.unpack <$> lookupSingle "NAME"
  _version <- extractFromEVR . T.unpack <$> lookupSingle "VERSION"
  _desc <- T.unpack <$> lookupSingle "DESC"
  _depends <- toDepList $ lookupList "DEPENDS"
  _makeDepends <- toDepList $ lookupList "MAKEDEPENDS"
  _provides <- toDepList $ lookupList "PROVIDES"
  _optDepends <- toDepList $ lookupList "OPTDEPENDS"
  _replaces <- toDepList $ lookupList "REPLACES"
  _conflicts <- toDepList $ lookupList "CONFLICTS"
  _checkDepends <- toDepList $ lookupList "CHECKDEPENDS"
  let _url = T.unpack <$> lookupSingleMaybe "URL"
  pure PkgDesc {..}
  where
    fields = parseDescFields txt

    lookupSingle field = case Map.lookup field fields of
      Just (x : _) -> Just x
      _ -> Nothing

    lookupSingleMaybe field = case Map.lookup field fields of
      Just (x : _) -> Just x
      _ -> Nothing

    lookupList field = Map.findWithDefault [] field fields

    toDepList = traverse $ \t -> case splitOn "=" $ T.unpack t of
      [name, version] -> Just $ PkgDependent (ArchLinuxName name) (Just version)
      [name] -> Just $ PkgDependent (ArchLinuxName name) Nothing
      _ -> Nothing
