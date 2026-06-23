{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Distribution.ArchHs.RDepCheck
  ( DepSrc (..),
    ReverseDep (..),
    SkippedReverseDep (..),
    prettySkippedReverseDep,
    reverseDependencyRanges,
    reverseDependencyRangesWithSkips,
    versionFailures,
  )
where

import Control.Monad (forM, unless)
import Data.Either (partitionEithers)
import Data.List (find)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Distribution.ArchHs.Core
import Distribution.ArchHs.Exception
import Distribution.ArchHs.ExtraDB
import Distribution.ArchHs.Hackage
import Distribution.ArchHs.Internal.Prelude
import Distribution.ArchHs.Name
import Distribution.ArchHs.PkgDesc
import Distribution.ArchHs.PP
import Distribution.ArchHs.Types

data DepSrc = Check | Make | Run
  deriving stock (Show, Eq, Ord)

instance Pretty DepSrc where
  pretty = \case
    Check -> "CheckDepends"
    Make -> "MakeDepends"
    Run -> "Depends"

data ReverseDep = ReverseDep
  { reverseDepName :: ArchLinuxName,
    reverseDepRanges :: [(DepSrc, VersionRange)]
  }

data SkippedReverseDep = SkippedReverseDep
  { skippedReverseDepName :: ArchLinuxName,
    skippedReverseDepError :: MyException
  }

prettySkippedReverseDep :: SkippedReverseDep -> Doc AnsiStyle
prettySkippedReverseDep SkippedReverseDep {..} =
  "Skip" <+> pretty (unArchLinuxName skippedReverseDepName) <> colon <+> viaShow skippedReverseDepError

reverseDependencyRanges ::
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
  Sem r [ReverseDep]
reverseDependencyRanges target = do
  (reverseDeps, skipped) <- reverseDependencyRangesWithSkips target
  mapM_ (printWarn . prettySkippedReverseDep) skipped
  pure reverseDeps

reverseDependencyRangesWithSkips ::
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
  Sem r ([ReverseDep], [SkippedReverseDep])
reverseDependencyRangesWithSkips target = do
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
  results <-
    forM reverseDeps $ \(PkgDesc {..}, src) -> do
      eCabal <-
        try @MyException $
          getCabal (toHackageName _name) =<< case simpleParsec _version of
            Just v -> pure v
            _ -> throw $ VersionNoParse _version
      case eCabal of
        Right cabal -> Right . ReverseDep _name <$> getDepVersion cabal target src
        Left e -> pure . Left $ SkippedReverseDep _name e
  pure $ case partitionEithers results of
    (skipped, reverseDeps') -> (reverseDeps', skipped)

versionFailures :: Maybe Version -> [(DepSrc, VersionRange)] -> [(DepSrc, VersionRange)]
versionFailures Nothing _ = []
versionFailures (Just version) result =
  [ (src, range)
    | (src, range) <- result,
      not $ withinRange version range
  ]

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
