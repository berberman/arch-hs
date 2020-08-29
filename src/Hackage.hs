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
import Types

defaultHackagePath :: FilePath
defaultHackagePath = "/home/berberman/.cabal/packages/mirrors.tuna.tsinghua.edu.cn/00-index.tar"

defaultHackageDB :: IO HackageDB
defaultHackageDB = loadHackageDB defaultHackagePath

loadHackageDB :: FilePath -> IO HackageDB
loadHackageDB path = do
  putStrLn $ "Hackage index: " ++ path
  readTarball Nothing path

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