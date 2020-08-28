{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

module Types
  ( PkgList,
    ComponentPkgList,
    CommunityDB,
    MyException (..),
    DependencyType (..),
    HsEnv (..),
    HsM,
    SolvedPackage (..),
    SolvedDependency (..),
    hackage,
    community,
    flags,
    provided,
    pkgName,
    pkgDeps,
    depName,
    depType,
  )
where

import Control.DeepSeq (NFData)
import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Distribution.Hackage.DB as DB
import Distribution.PackageDescription (FlagAssignment)
import Distribution.Types.PackageName (PackageName)
import Distribution.Types.UnqualComponentName (UnqualComponentName)
import GHC.Generics (Generic)
import Lens.Micro.TH (makeLenses)

type PkgList = [PackageName]

type ComponentPkgList = [(UnqualComponentName, PkgList)]

type CommunityDB = S.Set String

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
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

data HsEnv = HsEnv {_hackage :: DB.HackageDB, _community :: CommunityDB, _flags :: M.Map PackageName FlagAssignment}

type HsM m = ExceptT MyException (ReaderT HsEnv m)

data SolvedDependency = SolvedDependency {_provided :: Bool, _depName :: PackageName, _depType :: [DependencyType]}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

data SolvedPackage = ProvidedPackage {_pkgName :: PackageName} | SolvedPackage {_pkgName :: PackageName, _pkgDeps :: [SolvedDependency]}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

makeLenses ''SolvedDependency
makeLenses ''SolvedPackage
makeLenses ''HsEnv