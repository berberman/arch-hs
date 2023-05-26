{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Copyright: (c) 2020-2021 berberman
-- SPDX-License-Identifier: MIT
-- Maintainer: berberman <berberman@yandex.com>
-- Stability: experimental
-- Portability: portable
-- This module provides functions operating with @extra.db@ of pacman.
module Distribution.ArchHs.ExtraDB
  ( defaultExtraDBPath,
    loadExtraDB,
    isInExtra,
    versionInExtra,
    getPkgDesc,
#ifdef ALPM
    loadExtraDBFFI,
#endif
  )
where

import Conduit
import qualified Data.Conduit.Tar as Tar
import qualified Data.Conduit.Zlib as Zlib
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Distribution.ArchHs.Exception
import Distribution.ArchHs.Internal.Prelude
import Distribution.ArchHs.Name
import Distribution.ArchHs.PkgDesc
import Distribution.ArchHs.Types
import Data.Maybe (mapMaybe)

-----------------------------------------------------------------------------

#ifdef ALPM
{-# LANGUAGE ForeignFunctionInterface #-}

import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Distribution.ArchHs.Utils (extractFromEVR)
import Foreign.C.String (CString, peekCString)
import Foreign.Ptr (FunPtr, freeHaskellFunPtr)

foreign import ccall "wrapper"
  wrap :: (CString -> CString -> CString -> CString -> IO ()) -> IO (FunPtr (CString -> CString -> CString -> CString -> IO ()))

foreign import ccall "clib.h query_extra"
  query_extra :: FunPtr (CString -> CString -> CString -> CString -> IO ()) -> FunPtr (CString -> CString -> CString -> CString -> IO ()) -> IO ()

type RawPkgSet = IORef (Map.Map ArchLinuxName ((ArchLinuxVersion, String, String), IORef (Map.Map String [(ArchLinuxName, Maybe ArchLinuxVersion)])))

pkgCallback :: RawPkgSet -> CString -> CString -> CString -> CString -> IO ()
pkgCallback ref name version desc url = do
  name' <- peekCString name
  version' <- peekCString version
  desc' <- peekCString desc
  url' <- peekCString url
  m <- newIORef Map.empty
  modifyIORef' ref (Map.insert (ArchLinuxName name') ((extractFromEVR version', desc', url'), m))

listCallback :: RawPkgSet -> CString -> CString -> CString -> CString -> IO ()
listCallback ref name key dm dv = do
  name' <- peekCString name
  key' <- peekCString key
  dm' <- peekCString dm
  dv' <- peekCString dv
  s <- snd . (\r -> r Map.! ArchLinuxName name') <$> readIORef ref
  modifyIORef' s (Map.insertWith (++) key' [(ArchLinuxName dm', if null dv' then Nothing else Just (extractFromEVR dv'))])

-- | The same purpose as 'loadExtra' but use alpm to query extra db instead.
loadExtraDBFFI :: IO ExtraDB
loadExtraDBFFI = do
  ref <- newIORef Map.empty
  pkgCallbackW <- wrap $ pkgCallback ref
  listCallbackW <- wrap $ listCallback ref
  query_extra pkgCallbackW listCallbackW
  freeHaskellFunPtr pkgCallbackW
  freeHaskellFunPtr listCallbackW
  s <- readIORef ref
  re <-
    mapM
      ( \(name, ((ver, desc, url), r)) -> do
          l <- readIORef r
          let f (Just xs) = uncurry PkgDependent <$> xs
              f Nothing = []
          pure
            ( name,
              PkgDesc
                { _name = name,
                  _version = ver,
                  _desc = desc,
                  _url = if null url then Nothing else Just url,
                  _depends = f $ l Map.!? "depends",
                  _provides = f $ l Map.!? "provides",
                  _conflicts = f $ l Map.!? "conflicts",
                  _optDepends = f $ l Map.!? "optdepends",
                  _makeDepends = f $ l Map.!? "makedepends",
                  _checkDepends = f $ l Map.!? "checkdepends",
                  _replaces = f $ l Map.!? "replaces"
                }
            )
      )
      $ Map.toList s
  pure $ Map.fromList re
#endif
-----------------------------------------------------------------------------

-- | Default path to @extra.db@.
defaultExtraDBPath :: FilePath
defaultExtraDBPath = "/" </> "var" </> "lib" </> "pacman" </> "sync" </> "extra.db"

loadExtraDBC ::
  (MonadResource m, PrimMonad m, MonadThrow m) =>
  FilePath ->
  ConduitT i (ArchLinuxName, PkgDesc) m ()
loadExtraDBC path = do
  sourceFileBS path .| Zlib.ungzip .| Tar.untarChunks .| Tar.withEntries action
  where
    action header =
      when (Tar.headerFileType header == Tar.FTNormal) $ do
        x <- mconcat <$> sinkList
        let txt = T.unpack . decodeUtf8 $ x
            parsed = runDescParser (Tar.headerFilePath header) txt
            result = case parsed of
              Right desc ->
                desc
                  : ( if _name desc == ArchLinuxName "ghc"
                        || _name desc == ArchLinuxName "ghc-libs"
                        then mapMaybe promoteDependent (_provides desc)
                        else []
                    )
              -- Drop it if failed to parse
              Left _ -> []
        yieldMany $ (\desc -> (_name desc, desc)) <$> result

-- | Load @extra.db@ from @path@.
-- @desc@ files in the db will be parsed by @descParser@.
loadExtraDB :: FilePath -> IO ExtraDB
loadExtraDB path = Map.fromList <$> runConduitRes (loadExtraDBC path .| sinkList)

-----------------------------------------------------------------------------

-- | Check if a package exists in archlinux extra repo.
-- See 'HasMyName'.
isInExtra :: (HasMyName n, Member ExtraEnv r) => n -> Sem r Bool
isInExtra name = ask @ExtraDB >>= \db -> return $ toArchLinuxName name `Map.member` db

-- | Get the version of a package in archlinux extra repo.
-- If the package does not exist, 'PkgNotFound' will be thrown.
versionInExtra :: (HasMyName n, Members [ExtraEnv, WithMyErr] r) => n -> Sem r ArchLinuxVersion
versionInExtra name = _version <$> getPkgDesc name

-- | Get the pkgdesc a package in archlinux extra repo.
-- If the package does not exist, 'PkgNotFound' will be thrown.
getPkgDesc :: (HasMyName n, Members [ExtraEnv, WithMyErr] r) => n -> Sem r PkgDesc
getPkgDesc name =
  ask @ExtraDB >>= \db -> case db Map.!? toArchLinuxName name of
    Just x -> pure x
    _ -> throw $ PkgNotFound name
