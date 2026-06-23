{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Distribution.ArchHs.DepCheck
  ( DependencyFailure (..),
    VersionedList,
    dependencyFailures,
    directDependencies,
    inRange,
  )
where

import Distribution.ArchHs.Core
import Distribution.ArchHs.Exception
import Distribution.ArchHs.ExtraDB (versionInExtra)
import Distribution.ArchHs.Internal.Prelude
import Distribution.ArchHs.Types
import Distribution.ArchHs.Utils

type VersionedList = [(PackageName, VersionRange)]

data DependencyFailure
  = MissingDependency PackageName VersionRange
  | DependencyOutOfRange PackageName VersionRange Version

dependencyFailures ::
  Members [KnownGHCVersion, ExtraEnv, FlagAssignmentsEnv, WithMyErr, Trace, DependencyRecord] r =>
  GenericPackageDescription ->
  Sem r [DependencyFailure]
dependencyFailures cabal = do
  (depends, makedepends) <- directDependencies cabal
  concat <$> traverse dependencyFailure (depends <> makedepends)

dependencyFailure :: Members [ExtraEnv, WithMyErr] r => (PackageName, VersionRange) -> Sem r [DependencyFailure]
dependencyFailure dep =
  inRange dep <&> \case
    Left (name, range) -> [MissingDependency name range]
    Right (name, range, version, False) -> [DependencyOutOfRange name range version]
    Right _ -> []

directDependencies ::
  Members [KnownGHCVersion, FlagAssignmentsEnv, Trace, DependencyRecord] r =>
  GenericPackageDescription ->
  Sem r (VersionedList, VersionedList)
directDependencies cabal = do
  (libDeps, libToolsDeps, _) <- collectLibDeps id cabal
  (subLibDeps, subLibToolsDeps, _) <- collectSubLibDeps id cabal []
  (exeDeps, exeToolsDeps, _) <- collectExeDeps id cabal []
  (testDeps, testToolsDeps, _) <- collectTestDeps id cabal []
  setupDeps <- collectSetupDeps id cabal
  let flatten = mconcat . fmap snd
      l = libDeps
      lt = libToolsDeps
      sl = flatten subLibDeps
      slt = flatten subLibToolsDeps
      e = flatten exeDeps
      et = flatten exeToolsDeps
      t = flatten testDeps
      tt = flatten testToolsDeps
      mySubLibs = fmap (unqualComponentNameToPackageName . fst) subLibDeps
      notMyselfOrSubLib = (&&) <$> (/= getPkgName' cabal) <*> (`notElem` mySubLibs)
      distinct = filter (notMyselfOrSubLib . fst) . nub
      depends = distinct $ l <> sl <> e
      makedepends = distinct (lt <> slt <> et <> t <> tt <> setupDeps) \\ depends
      sort' = sortBy (\x y -> uncurry compare $ getTwo _1 x y)
  return (sort' depends, sort' makedepends)

inRange :: Members [ExtraEnv, WithMyErr] r => (PackageName, VersionRange) -> Sem r (Either (PackageName, VersionRange) (PackageName, VersionRange, Version, Bool))
inRange (name, hRange) =
  try @MyException (versionInExtra name)
    >>= \case
      Right rawVersion ->
        case simpleParsec rawVersion of
          Just version -> return . Right $ (name, hRange, version, withinRange version hRange)
          Nothing -> throw $ VersionNoParse rawVersion
      Left _ -> return . Left $ (name, hRange)
