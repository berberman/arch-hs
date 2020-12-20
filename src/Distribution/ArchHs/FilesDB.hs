{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

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

loadFilesDBFFI :: DBKind -> IO FilesDB
loadFilesDBFFI ( show -> db) = do
  ref <- newIORef Seq.empty
  db' <- newCString db
  callbackW <- wrap $ callback ref
  query_files db' callbackW
  freeHaskellFunPtr callbackW
  list <- toList <$> readIORef ref
  return $ foldr (\(k,v)-> Map.insertWith (<>) k [v] ) Map.empty  list
#endif

-- | Default path to files db.
defaultFilesDBDir :: FilePath
defaultFilesDBDir = "/" </> "var" </> "lib" </> "pacman" </> "sync"

loadFilesDBC ::
  (MonadResource m, PrimMonad m, MonadThrow m) =>
  DBKind ->
  FilePath ->
  ConduitT i Result m ()
loadFilesDBC db path = do
  sourceFileBS (path </> show db <> ".files") .| Zlib.ungzip .| Tar.untarChunks .| Tar.withEntries action
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
    extract s
      | Just x <- T.stripPrefix "usr/lib/" s,
        T.isSuffixOf ".so" x =
        Just $ T.takeWhileEnd (/= '/') x
      | Just x <- T.stripPrefix "usr/lib/pkgconfig" s,
        T.isSuffixOf ".pc" x =
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

loadFilesDB :: DBKind -> FilePath -> IO FilesDB
loadFilesDB db path = Map.fromList <$> runConduitRes (loadFilesDBC db path .| mergeResult .| sinkList)

-- Bad in performace
lookupPkg :: File -> FilesDB -> [ArchLinuxName]
lookupPkg file = Map.foldrWithKey (\k v acc -> if file `elem` v then k : acc else acc) []

data Result = Files FilePath [FilePath] | Desc FilePath ArchLinuxName
  deriving stock (Show)

data DBKind = Core | Community | Extra

instance Show DBKind where
  show Core = "core"
  show Community = "community"
  show Extra = "extra"

type File = String

type FilesDB = Map.Map ArchLinuxName [File]