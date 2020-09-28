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
    versionInCommunity,
  )
where

import           Conduit
import           Control.Monad                 (when)
import qualified Data.Conduit.Tar              as Tar
import qualified Data.Conduit.Zlib             as Zlib
import           Data.List.Split               (splitOn)
import qualified Data.Map.Strict               as Map
import qualified Data.Text                     as T
import           Data.Text.Encoding            (decodeUtf8)
import qualified Debug.Trace                   as D
import           Distribution.ArchHs.Exception
import           Distribution.ArchHs.Name
import           Distribution.ArchHs.PkgDesc
import           Distribution.ArchHs.Types
import           System.FilePath               ((</>))
import           Text.Megaparsec               (errorBundlePretty)

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
                      _ -> [(head $ r Map.! "NAME", extractVer . head $ r Map.! "VERSION")]
                    Left e -> D.trace (errorBundlePretty e) [("Err", "Err")]
            extractVer ver = head $
              splitOn "-" $ case splitOn ":" ver of
                (_ : v : []) -> v
                v : []       -> v
                _            -> fail "err"

        yieldMany $ result & each . _1 %~ CommunityName

-- | Load @community.db@ from @path@, removing @haskell-@ prefix.
loadProcessedCommunity :: (MonadUnliftIO m, PrimMonad m, MonadThrow m) => FilePath -> m CommunityDB
loadProcessedCommunity path = Map.fromList <$> (runConduitRes $ loadCommunity path .| sinkList)

-- | Check if a package from hackage exists in archlinux community repo.
-- The following name conversion occurs during the checking to work with 'loadProcessedCommunity'.
--
-- >>> "aeson" --> "aeson"
-- >>> "Cabal" --> "cabal"
-- >>> "haskell-a" --> "a"
isInCommunity :: (HasMyName n, Member CommunityEnv r) => n -> Sem r Bool
isInCommunity name = ask @CommunityDB >>= \db -> return $ (toCommunityName name) `Map.member` db

versionInCommunity :: (HasMyName n, Members [CommunityEnv, WithMyErr] r) => n -> Sem r CommunityVersion
versionInCommunity name =
  ask @CommunityDB >>= \db -> case db Map.!? (toCommunityName name) of
    Just x -> return x
    _      -> throw $ PkgNotFound name
