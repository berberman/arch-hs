module Hackage
  ( defaultHackagePath,
    defaultHackageDB,
    loadHackageDB,
    getLatestCabal,
    getPkgName,
    getPackageFlag
  )
where

import Control.Monad.Except
import qualified Data.Map as Map
import Distribution.Hackage.DB (HackageDB, cabalFile, readTarball)
import Distribution.Types.GenericPackageDescription
import Distribution.Types.PackageDescription
import qualified Distribution.Types.PackageId as I
import Distribution.Types.PackageName (PackageName)
import Lens.Micro
import Lens.Micro.Mtl
import Types

defaultHackagePath :: FilePath
defaultHackagePath = "/home/berberman/.cabal/packages/mirrors.tuna.tsinghua.edu.cn/00-index.tar"

defaultHackageDB :: IO HackageDB
defaultHackageDB = loadHackageDB defaultHackagePath

loadHackageDB :: FilePath -> IO HackageDB
loadHackageDB path = do
  putStrLn $ "Hackage index: " ++ path
  readTarball Nothing path

getLatestCabal :: (Monad m) => PackageName -> HsM m GenericPackageDescription
getLatestCabal name = do
  db <- view hackage
  case Map.lookup name db of
    (Just m) -> case Map.lookupMax m of
      Just (_, vdata) -> return $ vdata & cabalFile
      Nothing -> throwError VersionError
    Nothing -> throwError $ PkgNotFound name

getPkgName :: GenericPackageDescription -> PackageName
getPkgName = I.pkgName . package . packageDescription

getPackageFlag :: (Monad m) => PackageName -> HsM m [Flag]
getPackageFlag name = do
  cabal <- getLatestCabal name
  return $ cabal & genPackageFlags