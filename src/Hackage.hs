module Hackage
  ( defaultHackagePath,
    defaultHackageDB,
    loadHackageDB,
    getLatestCabal,
    getPackageFlag,
    traverseHackage
  )
where

import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Distribution.Hackage.DB (HackageDB, cabalFile, readTarball)
import Distribution.Types.PackageName (PackageName)
import Lens.Micro
import System.Directory
  ( findFile,
    getHomeDirectory,
    listDirectory,
  )
import System.FilePath ((</>))
import Types
import Distribution.Types.GenericPackageDescription (genPackageFlags, GenericPackageDescription)
import Distribution.Types.Flag (Flag)

defaultHackagePath :: IO FilePath
defaultHackagePath = do
  home <- (\d -> d </> ".cabal" </> "packages") <$> getHomeDirectory
  subs <- fmap (home </>) <$> listDirectory home
  target <- findFile subs "00-index.tar"
  case target of
    Just x -> return x
    Nothing -> fail $ "Unable to find hackage index [00-index.tar] from " ++ show subs

defaultHackageDB :: IO HackageDB
defaultHackageDB = defaultHackagePath >>= loadHackageDB

loadHackageDB :: FilePath -> IO HackageDB
loadHackageDB = readTarball Nothing

getLatestCabal :: Members [HackageEnv, WithMyErr] r => PackageName -> Sem r GenericPackageDescription
getLatestCabal name = do
  db <- ask @HackageDB
  case Map.lookup name db of
    (Just m) -> case Map.lookupMax m of
      Just (_, vdata) -> return $ vdata & cabalFile
      Nothing -> throw VersionError
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