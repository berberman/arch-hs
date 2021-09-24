{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

-- | Copyright: (c) 2020-2021 berberman
-- SPDX-License-Identifier: MIT
-- Maintainer: berberman <berberman@yandex.com>
-- Stability: experimental
-- Portability: portable
-- This module provides functions operating with 'FilesDB' of pacman.
module Distribution.ArchHs.FilesDB
  ( defaultFilesDBDir,
    loadFilesDB,
#ifdef ALPM
    loadFilesDBFFI,
#endif
    lookupPkg,
    DBKind (..),
    File,
    FilesDB,
  )
where

import Conduit
import qualified Data.Conduit.Tar as Tar
import qualified Data.Conduit.Zlib as Zlib
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Distribution.ArchHs.Internal.Prelude
import Distribution.ArchHs.PkgDesc (runDescFieldsParser)
import Distribution.ArchHs.Types

#ifdef ALPM
{-# LANGUAGE ForeignFunctionInterface #-}
import qualified Data.Sequence as Seq
import Data.Foldable (toList)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Foreign.C.String (newCString, CString, peekCString)
import Foreign.Ptr (FunPtr, freeHaskellFunPtr)

foreign import ccall "wrapper"
  wrap :: (CString -> CString -> IO ()) -> IO (FunPtr (CString -> CString -> IO ()))

foreign import ccall "clib.h query_files"
  query_files :: CString -> FunPtr (CString -> CString -> IO ()) -> IO ()

callback :: IORef (Seq.Seq (ArchLinuxName, FilePath)) -> CString -> CString -> IO ()
callback ref x y = do
  x' <- peekCString x
  y' <- peekCString y
  modifyIORef' ref (Seq.|> (ArchLinuxName x', y'))

-- | The same purpose as 'loadFilesDB' but use alpm to query files db instead.
loadFilesDBFFI :: DBKind -> IO FilesDB
loadFilesDBFFI (show -> db) = do
  ref <- newIORef Seq.empty
  db' <- newCString db
  callbackW <- wrap $ callback ref
  query_files db' callbackW
  freeHaskellFunPtr callbackW
  list <- toList <$> readIORef ref
  return $ foldr (\(k,v)-> Map.insertWith (<>) k [v]) Map.empty list
#endif

-- | Default directory containing files dbs (@/var/lib/pacman/sync@).
defaultFilesDBDir :: FilePath
defaultFilesDBDir = "/" </> "var" </> "lib" </> "pacman" </> "sync"

loadFilesDBC ::
  (MonadResource m, PrimMonad m, MonadThrow m) =>
  DBKind ->
  FilePath ->
  ConduitT i Result m ()
loadFilesDBC db dir = do
  sourceFileBS (dir </> show db <> ".files") .| Zlib.ungzip .| Tar.untarChunks .| Tar.withEntries action
  where
    action header
      | Tar.FTNormal <- Tar.headerFileType header,
        [fp, t] <- splitOn "/" $ Tar.headerFilePath header =
        do
          x <- mconcat <$> sinkList
          let txt = decodeUtf8 x
          case t of
            "files" -> yield . Files fp $ [T.unpack fname | (extract -> Just fname) <- tail $ T.lines txt]
            "desc" -> case runDescFieldsParser fp (T.unpack txt) of
              Right r | [name] <- r Map.! "NAME" -> yield . Desc fp $ ArchLinuxName name
              _ -> return ()
            _ -> return ()
      | otherwise = return ()
    extract :: T.Text -> Maybe T.Text
    extract s
      | Just x <- T.stripPrefix "usr/lib/" s,
        T.isSuffixOf ".so" x || T.isSuffixOf ".pc" x =
        Just $ T.takeWhileEnd (/= '/') x
      | otherwise = Nothing

mergeResult :: Monad m => ConduitT Result (ArchLinuxName, [File]) m ()
mergeResult = do
  rName <- await
  rFiles <- await
  case () of
    ()
      | Just (Desc fpd name) <- rName,
        Just (Files fpf files) <- rFiles,
        fpd == fpf ->
        when (files /= []) (yield (name, files)) >> mergeResult
    _ -> return ()

-- | Load a @db@ from @dir@
loadFilesDB :: DBKind -> FilePath -> IO FilesDB
loadFilesDB db dir = Map.fromList <$> runConduitRes (loadFilesDBC db dir .| mergeResult .| sinkList)

-- | Lookup which Arch Linux package contains this @file@ from given files db.
-- This query is bad in performance, since it traverses the entire db.
lookupPkg :: File -> FilesDB -> [ArchLinuxName]
lookupPkg file = Map.foldrWithKey (\k v acc -> if file `elem` v then k : acc else acc) []

data Result = Files FilePath [File] | Desc FilePath ArchLinuxName
  deriving stock (Show)

-- | Three files repos: @core@, @community@, and @extra@
data DBKind = Core | Community | Extra

instance Show DBKind where
  show Core = "core"
  show Community = "community"
  show Extra = "extra"

-- | A file's name
type File = String

-- | Representation of @repo.db@.
type FilesDB = Map.Map ArchLinuxName [File]
