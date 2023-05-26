{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Check (check) where

import Control.Monad (forM_, unless)
import Data.List (find)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Distribution.ArchHs.ExtraDB
import Distribution.ArchHs.Core
import Distribution.ArchHs.Exception
import Distribution.ArchHs.Hackage
import Distribution.ArchHs.Internal.Prelude
import Distribution.ArchHs.Name
import Distribution.ArchHs.PP
import Distribution.ArchHs.PkgDesc
import Distribution.ArchHs.Types

data DepSrc = Check | Make | Run
  deriving stock (Show, Eq, Ord)

instance Pretty DepSrc where
  pretty = \case
    Check -> "CheckDepends"
    Make -> "MakeDepends"
    Run -> "Depends"

check ::
  Members
    [ ExtraEnv,
      HackageEnv,
      KnownGHCVersion,
      FlagAssignmentsEnv,
      Trace,
      DependencyRecord,
      WithMyErr,
      Embed IO
    ]
    r =>
  PackageName ->
  Sem r ()
check target = do
  let aTarget = toArchLinuxName target
  exists <- isInExtra aTarget
  unless exists $ throw $ PkgNotFound target
  reverseDeps <-
    ( \xs ->
        [ (desc, [Make | md] <> [Check | cd] <> [Run | d])
          | ( _,
              desc@PkgDesc
                { _name = isHaskellPackage -> isHs,
                  _makeDepends = flip containsDep aTarget -> md,
                  _checkDepends = flip containsDep aTarget -> cd,
                  _depends = flip containsDep aTarget -> d
                }
              ) <-
              xs,
            isHs,
            md || cd || d
        ]
      )
      . Map.toList
      <$> ask @ExtraDB
  forM_ reverseDeps $ \(PkgDesc {..}, src) -> do
    eCabal <-
      try @MyException $
        getCabal (toHackageName _name) =<< case simpleParsec _version of
          Just v -> pure v
          _ -> throw $ VersionNoParse _version
    case eCabal of
      Right cabal -> do
        result <- getDepVersion cabal target src
        embed . putDoc $
          vsep
            ( annMagneta "Reverse dependency" <> colon
                <+> pretty (unArchLinuxName _name)
                : [indent 2 $ pretty s <> colon <+> viaPretty r | (s, r) <- result]
            )
            <> line
      Left e ->
        printWarn $ "Skip" <+> pretty (unArchLinuxName _name) <> colon <+> viaShow e

getDepVersion ::
  Members
    [ KnownGHCVersion,
      FlagAssignmentsEnv,
      DependencyRecord,
      Trace
    ]
    r =>
  GenericPackageDescription ->
  PackageName ->
  [DepSrc] ->
  Sem r [(DepSrc, VersionRange)]
getDepVersion cabal name src = do
  (libDeps, libToolsDeps, _) <- collectLibDeps id cabal
  (subLibDeps, subLibToolsDeps, _) <- collectSubLibDeps id cabal []
  (exeDeps, exeToolsDeps, _) <- collectExeDeps id cabal []
  (testDeps, testToolsDeps, _) <- collectTestDeps id cabal []
  setupDeps <- collectSetupDeps id cabal
  let flatten = mconcat . fmap snd
      deps = libDeps <> concatMap flatten [exeDeps, subLibDeps]
      makeOrCheckDeps = libToolsDeps <> setupDeps <> concatMap flatten [subLibToolsDeps, exeToolsDeps, testDeps, testToolsDeps]
  pure $
    catMaybes
      [ case s of
          Check -> f makeOrCheckDeps
          Make -> f makeOrCheckDeps
          Run -> f deps
        | s <- src,
          let f xs = (_1 .~ s) <$> find ((== name) . fst) xs
      ]
