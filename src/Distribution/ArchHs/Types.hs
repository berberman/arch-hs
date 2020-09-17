{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK ignore-exports #-}

-- | Copyright: (c) 2020 berberman
-- SPDX-License-Identifier: MIT
-- Maintainer: berberman <1793913507@qq.com>
-- Types used in this project.
module Distribution.ArchHs.Types
  ( PkgList,
    ComponentPkgList,
    CommunityDB,
    HackageEnv,
    CommunityEnv,
    FlagAssignmentsEnv,
    WithMyErr,
    MyException (..),
    DependencyType (..),
    DependencyKind (..),
    DependencyProvider (..),
    SolvedPackage (..),
    SolvedDependency (..),
    FlagAssignments,
    depProvider,
    pkgProvider,
    pkgName,
    pkgDeps,
    depName,
    depType,
    DependencyRecord,
    module Polysemy,
    module Polysemy.Error,
    module Polysemy.Reader,
    module Polysemy.State,
    module Lens.Micro,
  )
where

import Control.DeepSeq (NFData)
import Data.Map.Strict (Map)
import Data.Set (Set)
import Distribution.Hackage.DB (HackageDB)
import Distribution.PackageDescription (FlagAssignment)
import Distribution.Pretty (prettyShow)
import Distribution.Types.PackageName (PackageName, unPackageName)
import Distribution.Types.UnqualComponentName (UnqualComponentName, unUnqualComponentName)
import Distribution.Types.Version (Version)
import Distribution.Version (VersionRange)
import GHC.Generics (Generic)
import Lens.Micro
import Lens.Micro.TH (makeLenses)
import Polysemy
import Polysemy.Error
import Polysemy.Reader
import Polysemy.State

-- | A list of 'PackageName'.
type PkgList = [PackageName]

-- | A list of component represented by 'UnqualComponentName' and its dependencies collected in a 'PkgList'.
type ComponentPkgList = [(UnqualComponentName, PkgList)]

-- | Representation of @cummunity.db@.
type CommunityDB = Set String

-- | Reader effect of 'HackageDB'.
type HackageEnv = Reader HackageDB

-- | Reader effect of 'CommunityDB'.
type CommunityEnv = Reader CommunityDB

-- | A map of packages with their 'FlagAssignment'.
type FlagAssignments = Map PackageName FlagAssignment

-- | Reader effect of a map, associating 'PackageName' with its 'FlagAssignment'.
type FlagAssignmentsEnv = Reader FlagAssignments

-- | Unused state effect.
type DependencyRecord = State (Map PackageName [(PackageName, VersionRange)])

-- | Error effect of 'MyException'.
type WithMyErr = Error MyException

-- | Custom exception used in this project.
data MyException
  = PkgNotFound PackageName
  | VersionError PackageName Version
  | TargetExist PackageName DependencyProvider
  | LicenseError PackageName
  deriving stock (Eq)

instance Show MyException where
  show (PkgNotFound name) = "Unable to find [" <> unPackageName name <> "]"
  show (VersionError name version) = "Unable to find [" <> unPackageName name <> "-" <> prettyShow version <> "]"
  show (TargetExist name provider) = "Target [" <> unPackageName name <> "] has been provided by " <> show provider
  show (LicenseError name) = "Unable to find the license of [" <> unPackageName name <> "]"

-- | The type of a dependency. Who requires this?
data DependencyType
  = -- | By a /executable/.
    CExe UnqualComponentName
  | -- | By the /build tools/ of a /executable/.
    CExeBuildTools UnqualComponentName
  | -- | By a /library/.
    CLib
  | -- | By a /test suit/.
    CTest UnqualComponentName
  | -- | By a /benchmark/.
    CBenchmark UnqualComponentName
  | -- | By the /build tools/ of a /library/.
    CLibBuildTools
  | -- | By the /build tools/ of a /test suit/.
    CTestBuildTools UnqualComponentName
  | -- | By the /build tools/ of a /benchmark/.
    CBenchmarkBuildTools UnqualComponentName
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (NFData)

-- | Tags of data constructors of 'DependencyType'.
data DependencyKind
  = Exe
  | ExeBuildTools
  | Lib
  | Test
  | Benchmark
  | LibBuildTools
  | TestBuildTools
  | BenchmarkBuildTools
  deriving stock (Eq)

instance Show DependencyType where
  show (CExe x) = unUnqualComponentName x <> " :: Exe"
  show (CExeBuildTools x) = unUnqualComponentName x <> " :: ExeBuildTools"
  show (CTest x) = unUnqualComponentName x <> " :: Test"
  show (CBenchmark x) = unUnqualComponentName x <> " :: Benchmark"
  show (CTestBuildTools x) = unUnqualComponentName x <> " :: TestBuildTools"
  show (CBenchmarkBuildTools x) = unUnqualComponentName x <> " :: BenchmarkBuildTools"
  show CLib = "Lib"
  show CLibBuildTools = "LibBuildTools"

-- | Provider of a dependency.
data DependencyProvider = ByCommunity | ByAur
  deriving stock (Eq, Generic)
  deriving anyclass (NFData)

instance Show DependencyProvider where
  show ByCommunity = "community"
  show ByAur = "aur"

-- | A solved dependency, holden by 'SolvedPackage'.
data SolvedDependency = SolvedDependency
  { -- | Provider of this dependency.
    _depProvider :: Maybe DependencyProvider,
    -- | Name of the dependency.
    _depName :: PackageName,
    -- | Types of the dependency.
    _depType :: [DependencyType]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

-- | A solved package collected from dgraph. This data type is not designed to be recursively,
-- thus the element type of '_pkgDeps' is 'SolvedDependency', rather than another 'SolvedPackage'.
data SolvedPackage
  = -- | A package which has been provided by somebody, so there is no need to expand its dependencies.
    ProvidedPackage
      { -- | Package name.
        _pkgName :: PackageName,
        -- | Package provider. (The name of 'DependencyProvider' may be confusing...)
        _pkgProvider :: DependencyProvider
      }
  | -- | A package with its dependencies.
    SolvedPackage
      { -- | Package name.
        _pkgName :: PackageName,
        -- | Package dependencies.
        _pkgDeps :: [SolvedDependency]
      }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

makeLenses ''SolvedDependency
makeLenses ''SolvedPackage
