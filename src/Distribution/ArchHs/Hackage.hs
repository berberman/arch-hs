-- | Copyright: (c) 2020 berberman
-- SPDX-License-Identifier: MIT
-- Maintainer: berberman <1793913507@qq.com>
-- Stability: experimental
-- Portability: portable
-- This module provides functions operating with 'HackageDB' and 'GenericPackageDescription'.
module Distribution.ArchHs.Hackage
  ( lookupHackagePath,
    loadHackageDB,
    insertDB,
    parseCabalFile,
    getLatestCabal,
    getCabal,
    getPackageFlag,
    traverseHackage,
    getLatestSHA256,
  )
where

import qualified Data.ByteString                        as BS
import qualified Data.Map                               as Map
import           Data.Maybe                             (fromJust)
import           Distribution.ArchHs.Exception
import           Distribution.ArchHs.Internal.Prelude
import           Distribution.ArchHs.Types
import           Distribution.ArchHs.Utils              (getPkgName,
                                                         getPkgVersion)
import           Distribution.Hackage.DB                (HackageDB, VersionData (VersionData, cabalFile),
                                                         readTarball,
                                                         tarballHashes)
import           Distribution.PackageDescription.Parsec (parseGenericPackageDescriptionMaybe)
import           System.Directory                       (findFile,
                                                         getHomeDirectory,
                                                         listDirectory)

-- | Look up hackage tarball path from @~/.cabal@.
-- Arbitrary hackage mirror is potential to be selected.
-- Preferred to @01-index.tar@, whereas fallback to @00-index.tar@.
lookupHackagePath :: IO FilePath
lookupHackagePath = do
  home <- (\d -> d </> ".cabal" </> "packages") <$> getHomeDirectory
  subs <- fmap (home </>) <$> listDirectory home
  legacy <- findFile subs "00-index.tar"
  new <- findFile subs "01-index.tar"
  case new <|> legacy of
    Just x -> return x
    Nothing -> fail $ "Unable to find hackage index tarball from " <> show subs

-- | Read and parse hackage index tarball.
loadHackageDB :: FilePath -> IO HackageDB
loadHackageDB = readTarball Nothing

-- | Insert a 'GenericPackageDescription' into 'HackageDB'.
insertDB :: GenericPackageDescription -> HackageDB -> HackageDB
insertDB cabal db = Map.insert name packageData db
  where
    name = getPkgName $ packageDescription cabal
    version = getPkgVersion $ packageDescription cabal
    versionData = VersionData cabal $ Map.empty
    packageData = Map.singleton version versionData

-- | Read and parse @.cabal@ file.
parseCabalFile :: FilePath -> IO GenericPackageDescription
parseCabalFile path = do
  bs <- BS.readFile path
  case parseGenericPackageDescriptionMaybe bs of
    Just x -> return x
    _      -> fail $ "Failed to parse .cabal from " <> path

withLatestVersion :: Members [HackageEnv, WithMyErr] r => (VersionData -> a) -> PackageName -> Sem r a
withLatestVersion f name = do
  db <- ask @HackageDB
  case Map.lookup name db of
    (Just m) -> case Map.lookupMax m of
      Just (_, vdata) -> return $ f vdata
      Nothing         -> throw $ VersionNotFound name nullVersion
    Nothing -> throw $ PkgNotFound name

-- | Get the latest 'GenericPackageDescription'.
getLatestCabal :: Members [HackageEnv, WithMyErr] r => PackageName -> Sem r GenericPackageDescription
getLatestCabal = withLatestVersion cabalFile

-- | Get the latest SHA256 sum of the tarball .
getLatestSHA256 :: Members [HackageEnv, WithMyErr] r => PackageName -> Sem r String
getLatestSHA256 = withLatestVersion (\vdata -> tarballHashes vdata Map.! "sha256")

-- | Get 'GenericPackageDescription' with a specific version.
getCabal :: Members [HackageEnv, WithMyErr] r => PackageName -> Version -> Sem r GenericPackageDescription
getCabal name version = do
  db <- ask @HackageDB
  case Map.lookup name db of
    (Just m) -> case Map.lookup version m of
      Just vdata -> return $ vdata & cabalFile
      Nothing    -> throw $ VersionNotFound name version
    Nothing -> throw $ PkgNotFound name

-- | Get flags of a package.
getPackageFlag :: Members [HackageEnv, WithMyErr] r => PackageName -> Sem r [Flag]
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
