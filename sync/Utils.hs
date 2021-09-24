{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Utils
  ( linkedHaskellPackages,
  )
where

import Control.Monad (unless)
import qualified Data.Map.Strict as Map
import Distribution.ArchHs.Exception
import Distribution.ArchHs.Hackage
import Distribution.ArchHs.Internal.Prelude
import Distribution.ArchHs.Name
import Distribution.ArchHs.PP
import Distribution.ArchHs.Types

linkedHaskellPackages ::
  Members [CommunityEnv, HackageEnv, WithMyErr, Embed IO] r =>
  Sem r [(ArchLinuxName, ArchLinuxVersion, GenericPackageDescription)]
linkedHaskellPackages = do
  communityHaskellPackages <- filter (isHaskellPackage . fst) . Map.toList <$> ask @CommunityDB
  hackagePackages <- Map.keys <$> ask @HackageDB
  let go xs ys ((name, version) : pkgs) =
        let hName = toHackageName name
         in if hName `elem` hackagePackages
              then getLatestCabal hName >>= \cabal -> go ((name, version, cabal) : xs) ys pkgs
              else go xs (name : ys) pkgs
      go xs ys [] = pure (xs, ys)
  (linked, unlinked) <- go [] [] communityHaskellPackages
  embed $
    unless (null unlinked) $ do
      printWarn $ "Following packages in" <+> ppCommunity <+> "are not linked to hackage:"
      putStrLn . unlines $ unArchLinuxName <$> unlinked
  pure linked
