{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Types
  ( PkgList,
    ComponentPkgList,
    CommunityDB,
    HackageEnv,
    CommunityEnv,
    FlagAssignmentEnv,
    WithMyErr,
    MyException (..),
    DependencyType (..),
    DependencyKind (..),
    DependencyProvider (..),
    SolvedPackage (..),
    SolvedDependency (..),
    depProvider,
    pkgProvider,
    pkgName,
    pkgDeps,
    depName,
    depType,
    module Polysemy,
    module Polysemy.Error,
    module Polysemy.Reader,
  )
where

import Control.DeepSeq (NFData)
import Data.Map.Strict (Map)
import qualified Data.Set as S
import qualified Distribution.Hackage.DB as DB
import Distribution.PackageDescription (FlagAssignment)
import Distribution.Types.PackageName (PackageName)
import Distribution.Types.UnqualComponentName (UnqualComponentName, unUnqualComponentName)
import Distribution.Types.Version (Version)
import GHC.Generics (Generic)
import Lens.Micro.TH (makeLenses)
import Polysemy
import Polysemy.Error
import Polysemy.Reader

type PkgList = [PackageName]

type ComponentPkgList = [(UnqualComponentName, PkgList)]

type CommunityDB = S.Set String

type HackageEnv = Reader DB.HackageDB

type CommunityEnv = Reader CommunityDB

type FlagAssignmentEnv = Reader (Map PackageName FlagAssignment)

type WithMyErr = Error MyException

data MyException
  = PkgNotFound PackageName
  | VersionError PackageName Version
  | UrlError PackageName
  | TargetExist PackageName DependencyProvider
  | LicenseError PackageName
  deriving stock (Show, Eq)

data DependencyType
  = CExe UnqualComponentName
  | CExeBuildTools UnqualComponentName
  | CLib
  | CTest UnqualComponentName
  | CBenchmark UnqualComponentName
  | CLibBuildTools
  | CTestBuildTools UnqualComponentName
  | CBenchmarkBuildTools UnqualComponentName
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (NFData)

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
  show (CExe x) = unUnqualComponentName x ++ " ← Exe"
  show (CExeBuildTools x) = unUnqualComponentName x ++ " ← ExeBuildTools"
  show (CTest x) = unUnqualComponentName x ++ " ← Test"
  show (CBenchmark x) = unUnqualComponentName x ++ " ← Benchmark"
  show (CTestBuildTools x) = unUnqualComponentName x ++ " ← TestBuildTools"
  show (CBenchmarkBuildTools x) = unUnqualComponentName x ++ " ← BenchmarkBuildTools"
  show CLib = "Lib"
  show CLibBuildTools = "LibBuildTools"

data DependencyProvider = ByCommunity | ByAur
  deriving stock (Eq, Generic)
  deriving anyclass (NFData)

instance Show DependencyProvider where
  show ByCommunity = "community"
  show ByAur = "aur"

data SolvedDependency = SolvedDependency {_depProvider :: Maybe DependencyProvider, _depName :: PackageName, _depType :: [DependencyType]}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

data SolvedPackage
  = ProvidedPackage {_pkgName :: PackageName, _pkgProvider :: DependencyProvider}
  | SolvedPackage {_pkgName :: PackageName, _pkgDeps :: [SolvedDependency]}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

makeLenses ''SolvedDependency
makeLenses ''SolvedPackage
