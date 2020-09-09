module Utils
  ( getPkgName,
    getPkgVersion,
    mapSnd,
    toLower',
    isExe,
    isExeBuildTools,
    isLib,
    isLibBuildTools,
    isTest,
    isTestBuildTools,
    isBenchmark,
    isBenchmarkBuildTools,
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

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (a, b) = (a, f b)

toLower' :: String -> String
toLower' = fmap toLower

isExe :: DependencyType -> Bool
isExe (Exe _) = True
isExe _ = False

isExeBuildTools :: DependencyType -> Bool
isExeBuildTools (ExeBuildTools _) = True
isExeBuildTools _ = False

isLib :: DependencyType -> Bool
isLib Lib = True
isLib _ = False

isTest :: DependencyType -> Bool
isTest (Test _) = True
isTest _ = False

isBenchmark :: DependencyType -> Bool
isBenchmark (Benchmark _) = True
isBenchmark _ = False

isLibBuildTools :: DependencyType -> Bool
isLibBuildTools LibBuildTools = True
isLibBuildTools _ = False

isTestBuildTools :: DependencyType -> Bool
isTestBuildTools (TestBuildTools _) = True
isTestBuildTools _ = False

isBenchmarkBuildTools :: DependencyType -> Bool
isBenchmarkBuildTools (BenchmarkBuildTools _) = True
isBenchmarkBuildTools _ = False