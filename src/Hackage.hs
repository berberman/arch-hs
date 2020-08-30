module Hackage
  ( defaultHackagePath,
    defaultHackageDB,
    loadHackageDB,
    getLatestCabal,
    getPackageFlag,
  )
where

import qualified Data.Map as Map
import Distribution.Hackage.DB (HackageDB, cabalFile, readTarball)
import Distribution.Types.GenericPackageDescription
import Distribution.Types.PackageName (PackageName)
import Lens.Micro
import System.Directory
import System.FilePath
import Types

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