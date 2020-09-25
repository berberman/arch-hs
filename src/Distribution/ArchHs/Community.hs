{-# LANGUAGE OverloadedStrings #-}

-- | Copyright: (c) 2020 berberman
-- SPDX-License-Identifier: MIT
-- Maintainer: berberman <1793913507@qq.com>
-- This module provides functios operating with @community.db@ of pacman.
module Distribution.ArchHs.Community
  ( defaultCommunityPath,
    loadProcessedCommunity,
    isInCommunity,
  )
where

import Conduit
import Control.Monad (when)
import qualified Data.Conduit.Tar as Tar
import qualified Data.Conduit.Zlib as Zlib
import Data.List (intercalate)
import Data.List.Split (splitOn)
import qualified Data.Set as Set
import Distribution.ArchHs.Types
import Distribution.ArchHs.Utils (toLower')
import Distribution.Types.PackageName (PackageName, unPackageName)
import System.FilePath ((</>))

-- | Default path to @community.db@.
defaultCommunityPath :: FilePath
defaultCommunityPath = "/" </> "var" </> "lib" </> "pacman" </> "sync" </> "community.db"

loadCommunity ::
  (MonadResource m, PrimMonad m, MonadThrow m) =>
  FilePath ->
  ConduitT i FilePath m ()
loadCommunity path = do
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

-- | Load @community.db@ from @path@, removing @haskell-@ prefix.
loadProcessedCommunity :: (MonadUnliftIO m, PrimMonad m, MonadThrow m) => FilePath -> m CommunityDB
loadProcessedCommunity path = fmap Set.fromList $ runConduitRes $ loadCommunity path .| cookCommunity .| sinkList

-- | Check if a package from hackage exists in archlinux community repo.
-- A name conversion occurs during the checking to fit 'loadProcessedCommunity'.
--
-- >>> "aeson" --> "aeson"
-- >>> "Cabal" --> "cabal"
-- >>> "haskell-a" --> "a"
isInCommunity :: Member CommunityEnv r => PackageName -> Sem r Bool
isInCommunity name =
  ask @CommunityDB >>= \db ->
    return $ case splitOn "-" . unPackageName $ name of
      ("haskell" : xs) -> intercalate "-" xs `elem` db
      _ -> (toLower' $ unPackageName name) `elem` db