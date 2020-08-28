module Community
  ( defaultCommunityPath,
    loadCommunity,
    cookCommunity,
    collectDefaultCommunity,
    isInCommunity,
  )
where

import Conduit
import Control.Monad (when)
import Data.Char (toLower)
import qualified Data.Conduit.Tar as Tar
import qualified Data.Conduit.Zlib as Zlib
import Data.List (intercalate)
import Data.List.Split (splitOn)
import qualified Data.Set as S
import Distribution.Types.PackageName (PackageName, unPackageName)
import Lens.Micro.Mtl
import Types

defaultCommunityPath :: FilePath
defaultCommunityPath = "/var/lib/pacman/sync/community.db"

loadCommunity ::
  (MonadResource m, PrimMonad m, MonadThrow m) =>
  FilePath ->
  ConduitT i FilePath m ()
loadCommunity path = do
  liftIO . putStrLn $ "Community db: " ++ path
  sourceFileBS path .| Zlib.ungzip .| Tar.untarChunks .| Tar.withEntries action
  where
    action header =
      when (Tar.headerFileType header == Tar.FTNormal) $
        yield $ Tar.headerFilePath header

cookCommunity :: (Monad m) => ConduitT FilePath FilePath m ()
cookCommunity = mapC (go . (splitOn "-"))
  where
    go list = case length list of
      3 -> list !! 0
      s ->
        if list !! 0 == "haskell"
          then intercalate "-" . fst . splitAt (s - 3) . tail $ list
          else intercalate "-" . fst . splitAt (s - 2) $ list

collectDefaultCommunity :: (MonadUnliftIO m, PrimMonad m, MonadThrow m) => m CommunityDB
collectDefaultCommunity = fmap S.fromList $ runConduitRes $ loadCommunity defaultCommunityPath .| cookCommunity .| sinkList

isInCommunity :: (Monad m) => PackageName -> HsM m Bool
isInCommunity name = do
  db <- view community
  return $ case splitOn "-" . unPackageName $ name of
    ("haskell" : xs) -> intercalate "-" xs `elem` db
    _ -> (fmap toLower $ unPackageName name) `elem` db