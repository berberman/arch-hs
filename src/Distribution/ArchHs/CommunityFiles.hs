{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Distribution.ArchHs.CommunityFiles
  ( defaultCommunityFilesPath,
    loadCommunityFiles,
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

-- | Default path to @community.files@.
defaultCommunityFilesPath :: FilePath
defaultCommunityFilesPath = "/" </> "var" </> "lib" </> "pacman" </> "sync" </> "community.files"

data Result = Files FilePath [FilePath] | Desc FilePath CommunityName
  deriving stock (Show)

loadCommunityFilesC ::
  (MonadResource m, PrimMonad m, MonadThrow m) =>
  FilePath ->
  ConduitT i Result m ()
loadCommunityFilesC path = do
  sourceFileBS path .| Zlib.ungzip .| Tar.untarChunks .| Tar.withEntries action
  where
    action header
      | Tar.FTNormal <- Tar.headerFileType header,
        [fp, t] <- splitOn "/" $ Tar.headerFilePath header =
        do
          x <- mconcat <$> sinkList
          let txt = T.unpack . decodeUtf8 $ x
          case t of
            "files" -> yield . Files fp . tail $ lines txt
            "desc" -> case runDescFieldsParser fp txt of
              Right r | [name] <- r Map.! "NAME" -> yield . Desc fp $ CommunityName name
              _ -> return ()
            _ -> return ()
      | otherwise = return ()

mergeResult :: Monad m => ConduitT Result (CommunityName, [FilePath]) m ()
mergeResult = do
  rName <- await
  rFiles <- await
  case () of
    ()
      | Just (Desc fpd name) <- rName,
        Just (Files fpf files) <- rFiles,
        fpd == fpf ->
        yield (name, files) >> mergeResult
    _ -> return ()

loadCommunityFiles :: FilePath -> IO CommunityFiles
loadCommunityFiles path = Map.fromList <$> runConduitRes (loadCommunityFilesC path .| mergeResult .| sinkList)