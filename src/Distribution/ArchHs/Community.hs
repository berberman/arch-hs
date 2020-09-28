{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Copyright: (c) 2020 berberman
-- SPDX-License-Identifier: MIT
-- Maintainer: berberman <1793913507@qq.com>
-- Stability: experimental
-- Portability: portable
-- This module provides functios operating with @community.db@ of pacman.
module Distribution.ArchHs.Community
  ( defaultCommunityPath,
    loadProcessedCommunity,
    isInCommunity,
  )
where

import           Conduit
import           Control.Monad                  (when)
import qualified Data.Conduit.Tar               as Tar
import qualified Data.Conduit.Zlib              as Zlib
import           Data.List                      (intercalate, stripPrefix)
import           Data.List.Split                (splitOn)
import qualified Data.Map.Strict                as Map
import qualified Data.Text                      as T
import           Data.Text.Encoding             (decodeUtf8)
import qualified Debug.Trace                    as D
import           Distribution.ArchHs.PkgDesc
import           Distribution.ArchHs.Types
import           Distribution.ArchHs.Utils      (toLower')
import           Distribution.Types.PackageName (PackageName, unPackageName)
import           System.FilePath                ((</>))
import           Text.Megaparsec                (errorBundlePretty)

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
            (name, version) = case runDescFieldsParser (Tar.headerFilePath header) txt of
              Right r -> (head $ r Map.! "NAME", head $ r Map.! "VERSION")
              Left e  -> D.trace (errorBundlePretty e) ("Err", "Err")
            extractedVer = head $
              splitOn "-" $ case splitOn ":" version of
                (_ : v : []) -> v
                v : []       -> v
                _            -> fail "err"

        yield (name, extractedVer)

cookCommunity :: (Monad m) => ConduitT (CommunityName, CommunityVersion) (CommunityName, CommunityVersion) m ()
cookCommunity = mapC (_1 %~ go)
  where
    go s = case stripPrefix "haskell-" s of
      Just r  -> r
      Nothing -> s

-- | Load @community.db@ from @path@, removing @haskell-@ prefix.
loadProcessedCommunity :: (MonadUnliftIO m, PrimMonad m, MonadThrow m) => FilePath -> m CommunityDB
loadProcessedCommunity path = Map.fromList <$> (runConduitRes $ loadCommunity path .| cookCommunity .| sinkList)

-- | Check if a package from hackage exists in archlinux community repo.
-- The following name conversion occurs during the checking to work with 'loadProcessedCommunity'.
--
-- >>> "aeson" --> "aeson"
-- >>> "Cabal" --> "cabal"
-- >>> "haskell-a" --> "a"
isInCommunity :: Member CommunityEnv r => PackageName -> Sem r Bool
isInCommunity name =
  ask @CommunityDB >>= \db ->
    return $ case splitOn "-" . unPackageName $ name of
      ("haskell" : xs) -> intercalate "-" xs `Map.member` db
      _                -> (toLower' $ unPackageName name) `Map.member` db
