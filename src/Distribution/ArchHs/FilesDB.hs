{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Distribution.ArchHs.FilesDB
  ( defaultFilesDBDir,
    loadFilesDB,
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

-- | Default path to files db.
defaultFilesDBDir :: FilePath
defaultFilesDBDir = "/" </> "var" </> "lib" </> "pacman" </> "sync"

dbPath :: DBKind -> FilePath
dbPath Core = "core.files"
dbPath Community = "community.files"
dbPath Extra = "extra.files"

loadFilesDBC ::
  (MonadResource m, PrimMonad m, MonadThrow m) =>
  DBKind ->
  FilePath ->
  ConduitT i Result m ()
loadFilesDBC db path = do
  sourceFileBS (path </> dbPath db) .| Zlib.ungzip .| Tar.untarChunks .| Tar.withEntries action
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

type File = String

type FilesDB = Map.Map ArchLinuxName [File]