{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- | Copyright: (c) 2020 berberman
-- SPDX-License-Identifier: MIT
-- Maintainer: berberman <berberman@yandex.com>
-- Stability: experimental
-- Portability: portable
-- This module supports <https://aur.archlinux.org/ AUR> searching.
module Distribution.ArchHs.Aur
  ( Aur,
    isInAur,
    aurToIO,
  )
where

import qualified Data.Text as T
import Distribution.ArchHs.Exception
import Distribution.ArchHs.Internal.Prelude
import Distribution.ArchHs.Name
import Distribution.ArchHs.Types
import Network.HTTP.Client (Manager)
import Web.ArchLinux

-- | AUR Effect
data Aur m a where
  IsInAur :: HasMyName n => n -> Aur m Bool

makeSem_ ''Aur

-- | Check whether a __haskell__ package exists in AUR
isInAur :: (HasMyName n, Member Aur r) => n -> Sem r Bool

-- | Run 'Aur' effect.
aurToIO :: Manager -> Members [WithMyErr, Embed IO] r => Sem (Aur ': r) a -> Sem r a
aurToIO manager = interpret $ \case
  (IsInAur name) -> do
    result <- embed . runAPIClient manager . searchAur ByName . T.pack $ unArchLinuxName . toArchLinuxName $ name
    case result of
      Left err -> throw $ NetworkException err
      Right AurResponse {_results = [_]} -> return True
      _ -> return False
