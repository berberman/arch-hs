{-# LANGUAGE TypeApplications #-}

-- | Copyright: (c) 2020-2021 berberman
-- SPDX-License-Identifier: MIT
-- Maintainer: berberman <berberman@yandex.com>
-- Stability: experimental
-- Portability: portable
-- This module provides functions operating with 'HackageDB' and 'GenericPackageDescription'.
module Distribution.ArchHs.Hackage
  ( lookupHackagePath,
    loadHackageDB,
    loadRawHackageDB,
    loadHackageDBs,
    insertDB,
    parseCabalFile,
    getLatestCabal,
    getNewerVersions,
    getCabal,
    getCabalIncludingDeprecated,
    getPackageFlag,
    traverseHackage,
    getLatestSHA256,
    HackageDB,
    RawHackageDB,
  )
where

import Control.Monad (filterM)
import qualified Data.ByteString as BS
import Data.List (maximumBy)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromJust)
import Data.Ord (comparing)
import Distribution.ArchHs.Exception
import Distribution.ArchHs.Internal.Prelude
import Distribution.ArchHs.Types
import Distribution.ArchHs.Utils (getPkgName, getPkgVersion)
import Distribution.Hackage.DB (HackageDB, VersionData (VersionData, cabalFile), readTarball, tarballHashes)
import Distribution.Hackage.DB.Path (cabalStateDir)
import qualified Distribution.Hackage.DB.Parsed as Parsed
import qualified Distribution.Hackage.DB.Unparsed as Unparsed
import Distribution.PackageDescription.Parsec (parseGenericPackageDescriptionMaybe)
import System.Directory (doesDirectoryExist, doesFileExist, getModificationTime, listDirectory)

type RawHackageDB = Unparsed.HackageDB

-- | Look up hackage tarball path from Cabal's package index cache.
-- Preferred to the newest @01-index.tar@, whereas fallback to the newest @00-index.tar@.
lookupHackagePath :: IO FilePath
lookupHackagePath = do
  root <- (</> "packages") <$> cabalStateDir
  candidates <- hackageIndexCandidates root
  case newestIndex "01-index.tar" candidates <|> newestIndex "00-index.tar" candidates of
    Just x -> return x
    Nothing -> fail $ "Unable to find hackage index tarball from " <> root
  where
    hackageIndexCandidates root = do
      exists <- doesDirectoryExist root
      if exists
        then do
          rootIndexes <- indexesIn root
          dirs <- fmap (root </>) <$> listDirectory root
          repoDirs <- filterM doesDirectoryExist dirs
          repoIndexes <- concat <$> traverse indexesIn repoDirs
          pure $ rootIndexes <> repoIndexes
        else pure []

    indexesIn dir =
      catMaybes
        <$> traverse
          ( \name -> do
              let path = dir </> name
              exists <- doesFileExist path
              if exists
                then do
                  time <- getModificationTime path
                  pure $ Just (name, path, time)
                else pure Nothing
          )
          ["01-index.tar", "00-index.tar"]

    newestIndex name candidates =
      case filter (\(candidateName, _, _) -> candidateName == name) candidates of
        [] -> Nothing
        xs -> Just $ (\(_, path, _) -> path) (maximumBy (comparing (\(_, _, time) -> time)) xs)

-- | Read and parse hackage index tarball.
loadHackageDB :: FilePath -> IO HackageDB
loadHackageDB = readTarball Nothing

-- | Read the Hackage index tarball without applying preferred-version ranges.
loadRawHackageDB :: FilePath -> IO RawHackageDB
loadRawHackageDB = Unparsed.readTarball Nothing

-- | Read Hackage once and expose both preferred and raw views.
loadHackageDBs :: FilePath -> IO (HackageDB, RawHackageDB)
loadHackageDBs path = do
  raw <- loadRawHackageDB path
  pure (Parsed.parseDB raw, raw)

-- | Insert a 'GenericPackageDescription' into 'HackageDB'.
insertDB :: GenericPackageDescription -> HackageDB -> HackageDB
insertDB cabal = Map.insert name packageData
  where
    name = getPkgName $ packageDescription cabal
    version = getPkgVersion $ packageDescription cabal
    versionData = VersionData cabal Map.empty
    packageData = Map.singleton version versionData

-- | Read and parse @.cabal@ file.
parseCabalFile :: FilePath -> IO GenericPackageDescription
parseCabalFile path = do
  bs <- BS.readFile path
  case parseGenericPackageDescriptionMaybe bs of
    Just x -> return x
    _ -> fail $ "Failed to parse .cabal from " <> path

withLatestVersion :: Members [HackageEnv, WithMyErr] r => (VersionData -> a) -> PackageName -> Sem r a
withLatestVersion f name = do
  db <- ask @HackageDB
  case Map.lookup name db of
    (Just m) -> case Map.lookupMax m of
      Just (_, vdata) -> return $ f vdata
      Nothing -> throw $ VersionNotFound name nullVersion
    Nothing -> throw $ PkgNotFound name

-- | Get the latest 'GenericPackageDescription'.
getLatestCabal :: Members [HackageEnv, WithMyErr] r => PackageName -> Sem r GenericPackageDescription
getLatestCabal = withLatestVersion cabalFile

-- | Get all Hackage versions newer than the given version.
--
-- The parsed 'HackageDB' has already applied Hackage's preferred-version
-- range, which excludes deprecated versions.
getNewerVersions :: Members [HackageEnv, WithMyErr] r => PackageName -> Version -> Sem r [Version]
getNewerVersions name version = do
  db <- ask @HackageDB
  case Map.lookup name db of
    Just packageData -> return [hackageVersion | hackageVersion <- Map.keys packageData, hackageVersion > version]
    Nothing -> throw $ PkgNotFound name

-- | Get the latest SHA256 sum of the tarball .
getLatestSHA256 :: Members [HackageEnv, WithMyErr] r => PackageName -> Sem r (Maybe String)
getLatestSHA256 = withLatestVersion (\vdata -> tarballHashes vdata Map.!? "sha256")

-- | Get 'GenericPackageDescription' with a specific version.
getCabal :: Members [HackageEnv, WithMyErr] r => PackageName -> Version -> Sem r GenericPackageDescription
getCabal name version = do
  db <- ask @HackageDB
  case Map.lookup name db of
    (Just m) -> case Map.lookup version m of
      Just vdata -> return $ vdata & cabalFile
      Nothing -> throw $ VersionNotFound name version
    Nothing -> throw $ PkgNotFound name

-- | Get a specific cabal file without applying preferred-version ranges.
--
-- Upgrade candidates should use 'getCabal' so deprecated Hackage versions stay
-- hidden. Reverse dependency checks need this exact lookup because [extra] can
-- temporarily contain a version that Hackage later deprecated.
getCabalIncludingDeprecated :: Members [RawHackageEnv, WithMyErr] r => PackageName -> Version -> Sem r GenericPackageDescription
getCabalIncludingDeprecated name version = do
  db <- ask @RawHackageDB
  case Map.lookup name db of
    Just packageData -> case Map.lookup version (Unparsed.versions packageData) of
      Just vdata ->
        case parseGenericPackageDescriptionMaybe (Unparsed.cabalFile vdata) of
          Just cabal -> pure cabal
          Nothing -> throw $ CabalNoParse name version
      Nothing -> throw $ VersionNotFound name version
    Nothing -> throw $ PkgNotFound name

-- | Get flags of a package.
getPackageFlag :: Members [HackageEnv, WithMyErr] r => PackageName -> Sem r [PkgFlag]
getPackageFlag name = do
  cabal <- getLatestCabal name
  return $ cabal & genPackageFlags

-- | Traverse hackage packages.
traverseHackage :: (Member HackageEnv r, Applicative f) => ((PackageName, GenericPackageDescription) -> f b) -> Sem r (f [b])
traverseHackage f = do
  db <- ask @HackageDB
  let x =
        Map.toList
          . Map.map (cabalFile . (^. _2) . fromJust)
          . Map.filter (/= Nothing)
          $ Map.map Map.lookupMax db
  return $ traverse f x
