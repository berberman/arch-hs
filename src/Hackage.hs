module Hackage
  ( lookupHackagePath,
    loadHackageDB,
    insertDB,
    parseCabalFile,
    getLatestCabal,
    getCabal,
    getPackageFlag,
    traverseHackage,
  )
where

import qualified Data.ByteString as BS
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Distribution.Hackage.DB
import Distribution.PackageDescription.Parsec (parseGenericPackageDescriptionMaybe)
import Distribution.Types.Flag (Flag)
import Distribution.Types.GenericPackageDescription (GenericPackageDescription, genPackageFlags)
import Distribution.Types.PackageName (PackageName)
import Distribution.Version (Version, nullVersion)
import Lens.Micro
import System.Directory
  ( findFile,
    getHomeDirectory,
    listDirectory,
  )
import System.FilePath ((</>))
import Types
import Utils

lookupHackagePath :: IO FilePath
lookupHackagePath = do
  home <- (\d -> d </> ".cabal" </> "packages") <$> getHomeDirectory
  subs <- fmap (home </>) <$> listDirectory home
  target <- findFile subs "01-index.tar"
  case target of
    Just x -> return x
    Nothing -> fail $ "Unable to find hackage index [01-index.tar] from " ++ show subs

loadHackageDB :: FilePath -> IO HackageDB
loadHackageDB = readTarball Nothing

insertDB :: GenericPackageDescription -> HackageDB -> HackageDB
insertDB cabal db = Map.insert name packageData db
  where
    name = getPkgName cabal
    version = getPkgVersion cabal
    versionData = VersionData cabal $ Map.empty
    packageData = Map.singleton version versionData

parseCabalFile :: FilePath -> IO GenericPackageDescription
parseCabalFile path = do
  bs <- BS.readFile path
  case parseGenericPackageDescriptionMaybe bs of
    Just x -> return x
    _ -> fail $ "Failed to parse .cabal from " ++ path

getLatestCabal :: Members [HackageEnv, WithMyErr] r => PackageName -> Sem r GenericPackageDescription
getLatestCabal name = do
  db <- ask @HackageDB
  case Map.lookup name db of
    (Just m) -> case Map.lookupMax m of
      Just (_, vdata) -> return $ vdata & cabalFile
      Nothing -> throw $ VersionError name nullVersion
    Nothing -> throw $ PkgNotFound name

getCabal :: Members [HackageEnv, WithMyErr] r => PackageName -> Version -> Sem r GenericPackageDescription
getCabal name version = do
  db <- ask @HackageDB
  case Map.lookup name db of
    (Just m) -> case Map.lookup version m of
      Just vdata -> return $ vdata & cabalFile
      Nothing -> throw $ VersionError name version
    Nothing -> throw $ PkgNotFound name

getPackageFlag :: Members [HackageEnv, WithMyErr] r => PackageName -> Sem r [Flag]
getPackageFlag name = do
  cabal <- getLatestCabal name
  return $ cabal & genPackageFlags

traverseHackage :: (Member HackageEnv r, Applicative f) => ((PackageName, GenericPackageDescription) -> f b) -> Sem r (f [b])
traverseHackage f = do
  db <- ask @HackageDB
  let x =
        Map.toList
          . Map.map (cabalFile . (^. _2) . fromJust)
          . Map.filter (/= Nothing)
          $ Map.map Map.lookupMax db
  return $ traverse f x