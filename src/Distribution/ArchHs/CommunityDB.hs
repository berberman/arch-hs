{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Copyright: (c) 2020-2021 berberman
-- SPDX-License-Identifier: MIT
-- Maintainer: berberman <berberman@yandex.com>
-- Stability: experimental
-- Portability: portable
-- This module provides functions operating with @community.db@ of pacman.
module Distribution.ArchHs.CommunityDB
  ( defaultCommunityDBPath,
    loadCommunityDB,
    isInCommunity,
    versionInCommunity,
    getPkgDesc,
#ifdef ALPM
    loadCommunityDBFFI,
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
import qualified Data.Sequence as Seq
import Data.Foldable (toList)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Foreign.C.String (CString, peekCString)
import Foreign.Ptr (FunPtr, freeHaskellFunPtr)

foreign import ccall "wrapper"
  wrap :: (CString -> CString -> IO ()) -> IO (FunPtr (CString -> CString -> IO ()))

foreign import ccall "clib.h query_community"
  query_community :: FunPtr (CString -> CString -> IO ()) -> IO ()

callback :: IORef (Seq.Seq (ArchLinuxName, ArchLinuxVersion)) -> CString -> CString -> IO ()
callback ref x y = do
  x' <- peekCString x
  y' <- peekCString y
  modifyIORef' ref (Seq.|> (ArchLinuxName x', extractFromEVR y'))

-- | The same purpose as 'loadCommunity' but use alpm to query community db instead.
loadCommunityDBFFI :: IO CommunityDB
loadCommunityDBFFI = do
  ref <- newIORef Seq.empty
  callbackW <- wrap $ callback ref
  query_community callbackW
  freeHaskellFunPtr callbackW
  Map.fromList . toList <$> readIORef ref
#endif

-----------------------------------------------------------------------------

-- | Default path to @community.db@.
defaultCommunityDBPath :: FilePath
defaultCommunityDBPath = "/" </> "var" </> "lib" </> "pacman" </> "sync" </> "community.db"

loadCommunityDBC ::
  (MonadResource m, PrimMonad m, MonadThrow m) =>
  FilePath ->
  ConduitT i (ArchLinuxName, PkgDesc) m ()
loadCommunityDBC path = do
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

-- | Load @community.db@ from @path@.
-- @desc@ files in the db will be parsed by @descParser@.
loadCommunityDB :: FilePath -> IO CommunityDB
loadCommunityDB path = Map.fromList <$> runConduitRes (loadCommunityDBC path .| sinkList)

-----------------------------------------------------------------------------

-- | Check if a package exists in archlinux community repo.
-- See 'HasMyName'.
isInCommunity :: (HasMyName n, Member CommunityEnv r) => n -> Sem r Bool
isInCommunity name = ask @CommunityDB >>= \db -> return $ toArchLinuxName name `Map.member` db

-- | Get the version of a package in archlinux community repo.
-- If the package does not exist, 'PkgNotFound' will be thrown.
versionInCommunity :: (HasMyName n, Members [CommunityEnv, WithMyErr] r) => n -> Sem r ArchLinuxVersion
versionInCommunity name = _version <$> getPkgDesc name

-- | Get the pkgdesc a package in archlinux community repo.
-- If the package does not exist, 'PkgNotFound' will be thrown.
getPkgDesc :: (HasMyName n, Members [CommunityEnv, WithMyErr] r) => n -> Sem r PkgDesc
getPkgDesc name =
  ask @CommunityDB >>= \db -> case db Map.!? toArchLinuxName name of
    Just x -> pure x
    _ -> throw $ PkgNotFound name
