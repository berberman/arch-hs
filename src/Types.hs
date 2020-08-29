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
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Distribution.Hackage.DB as DB
import Distribution.PackageDescription (FlagAssignment)
import Distribution.Types.PackageName (PackageName)
import Distribution.Types.UnqualComponentName (UnqualComponentName, unUnqualComponentName)
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

type FlagAssignmentEnv = Reader (M.Map PackageName FlagAssignment)

type WithMyErr = Error MyException

data MyException
  = PkgNotFound PackageName
  | VersionError
  | UrlError PackageName
  | TargetExist PackageName
  | LicenseError PackageName
  deriving stock (Show, Eq)

data DependencyType
  = Exe UnqualComponentName
  | ExeBuildTools UnqualComponentName
  | Lib
  | Test UnqualComponentName
  | Benchmark UnqualComponentName
  | LibBuildTools
  | TestBuildTools UnqualComponentName
  | BenchmarkBuildTools UnqualComponentName
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (NFData)

instance Show DependencyType where
  show (Exe x) = unUnqualComponentName x ++ " ← Exe"
  show (ExeBuildTools x) = unUnqualComponentName x ++ " ← ExeBuildTools"
  show (Test x) = unUnqualComponentName x ++ " ← Test"
  show (Benchmark x) = unUnqualComponentName x ++ " ← Benchmark"
  show (TestBuildTools x) = unUnqualComponentName x ++ " ← TestBuildTools"
  show (BenchmarkBuildTools x) = unUnqualComponentName x ++ " ← BenchmarkBuildTools"
  show Lib = "Lib"
  show LibBuildTools = "LibBuildTools"

data DependencyProvider = ByCommunity | ByAur
  deriving stock (Eq, Generic)
  deriving anyclass (NFData)

instance Show DependencyProvider where
  show ByCommunity = "community"
  show ByAur = "aur"

data SolvedDependency = SolvedDependency {_depProvider :: Maybe DependencyProvider, _depName :: PackageName, _depType :: [DependencyType]}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

data SolvedPackage = ProvidedPackage {_pkgName :: PackageName, _pkgProvider :: DependencyProvider} | SolvedPackage {_pkgName :: PackageName, _pkgDeps :: [SolvedDependency]}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

makeLenses ''SolvedDependency
makeLenses ''SolvedPackage
