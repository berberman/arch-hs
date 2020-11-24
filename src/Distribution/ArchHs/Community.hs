{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Copyright: (c) 2020 berberman
-- SPDX-License-Identifier: MIT
-- Maintainer: berberman <1793913507@qq.com>
-- Stability: experimental
-- Portability: portable
-- This module provides functions operating with @community.db@ of pacman.
module Distribution.ArchHs.Community
  ( defaultCommunityPath,
    loadProcessedCommunity,
    isInCommunity,
    versionInCommunity,
    compiledWithAlpm,
#ifdef ALPM
    loadCommunityFFI,
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
import Distribution.ArchHs.Utils

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

callback :: IORef (Seq.Seq (CommunityName, CommunityVersion)) -> CString -> CString -> IO ()
callback ref x y = do
  x' <- peekCString x
  y' <- peekCString y
  modifyIORef' ref (Seq.|> (CommunityName x', extractFromEVR y'))

-- | The same purpose as 'loadCommunity' but use alpm to query community db instead.
loadCommunityFFI :: IO CommunityDB
loadCommunityFFI = do
  ref <- newIORef Seq.empty
  callbackW <- wrap $ callback ref
  query_community callbackW
  freeHaskellFunPtr callbackW
  Map.fromList . toList <$> readIORef ref
#endif

-- | Whether this program enables alpm support.
compiledWithAlpm :: Bool
compiledWithAlpm =
#ifdef ALPM
  True
#else
  False
#endif

-----------------------------------------------------------------------------

-- | Default path to @community.db@.
defaultCommunityPath :: FilePath
defaultCommunityPath = "/" </> "var" </> "lib" </> "pacman" </> "sync" </> "community.db"

loadCommunity ::
  (MonadResource m, PrimMonad m, MonadThrow m) =>
  FilePath ->
  ConduitT i (CommunityName, CommunityVersion) m ()
loadCommunity path = do
  sourceFileBS path .| Zlib.ungzip .| Tar.untarChunks .| Tar.withEntries action
  where
    action header =
      when (Tar.headerFileType header == Tar.FTNormal) $ do
        x <- mconcat <$> sinkList
        let txt = T.unpack . decodeUtf8 $ x
            result =
              let provided r = r Map.! "PROVIDES"
                  parseProvidedTerm t = let s = splitOn "=" t in (s ^. ix 0, s ^. ix 1)
               in case runDescFieldsParser (Tar.headerFilePath header) txt of
                    Right r -> case head $ r Map.! "NAME" of
                      "ghc" -> parseProvidedTerm <$> provided r
                      "ghc-libs" -> parseProvidedTerm <$> provided r
                      _ -> [(head $ r Map.! "NAME", extractFromEVR . head $ r Map.! "VERSION")]
                    -- Drop it if failed to parse
                    Left _ -> []

        yieldMany $ result & each . _1 %~ CommunityName

-- | Load @community.db@ from @path@.
-- @desc@ files in the db will be parsed by @descParser@.
loadProcessedCommunity :: (MonadUnliftIO m, PrimMonad m, MonadThrow m) => FilePath -> m CommunityDB
loadProcessedCommunity path = Map.fromList <$> runConduitRes (loadCommunity path .| sinkList)

-----------------------------------------------------------------------------

-- | Check if a package exists in archlinux community repo.
-- See 'HasMyName'.
isInCommunity :: (HasMyName n, Member CommunityEnv r) => n -> Sem r Bool
isInCommunity name = ask @CommunityDB >>= \db -> return $ toCommunityName name `Map.member` db

-- | Get the version of a package in archlinux community repo.
-- If the package does not exist, 'PkgNotFound' will be thrown.
versionInCommunity :: (HasMyName n, Members [CommunityEnv, WithMyErr] r) => n -> Sem r CommunityVersion
versionInCommunity name =
  ask @CommunityDB >>= \db -> case db Map.!? toCommunityName name of
    Just x -> return x
    _ -> throw $ PkgNotFound name
