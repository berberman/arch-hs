module Utils
  ( getPkgName,
    getPkgVersion,
    toLower',
    dependencyTypeToKind,
    unExe,
    fixName,
  )
where

import Data.Char (toLower)
import Data.List.Split (splitOn)
import Distribution.PackageDescription (GenericPackageDescription, package, packageDescription)
import Distribution.Types.ExeDependency (ExeDependency (..))
import qualified Distribution.Types.PackageId as I
import Distribution.Types.PackageName (PackageName)
import Distribution.Version (Version)
import Types

unExe :: ExeDependency -> PackageName
unExe (ExeDependency name _ _) = name

getPkgName :: GenericPackageDescription -> PackageName
getPkgName = I.pkgName . package . packageDescription

getPkgVersion :: GenericPackageDescription -> Version
getPkgVersion = I.pkgVersion . package . packageDescription

fixName :: String -> String
fixName s = case splitOn "-" s of
  ("haskell" : _) -> toLower' s
  _ -> "haskell-" ++ toLower' s

toLower' :: String -> String
toLower' = fmap toLower

dependencyTypeToKind :: DependencyType -> DependencyKind
dependencyTypeToKind (CExe _) = Exe
dependencyTypeToKind (CExeBuildTools _) = ExeBuildTools
dependencyTypeToKind (CLib) = Lib
dependencyTypeToKind (CTest _) = Test
dependencyTypeToKind (CBenchmark _) = Benchmark
dependencyTypeToKind (CLibBuildTools) = LibBuildTools
dependencyTypeToKind (CTestBuildTools _) = TestBuildTools
dependencyTypeToKind (CBenchmarkBuildTools _) = BenchmarkBuildTools