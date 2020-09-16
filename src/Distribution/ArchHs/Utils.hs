-- | Copyright: (c) 2020 berberman
-- SPDX-License-Identifier: MIT
-- Maintainer: berberman <1793913507@qq.com>
-- Miscellaneous functions used crossing modules.
module Distribution.ArchHs.Utils
  ( getPkgName,
    getPkgName',
    getPkgVersion,
    toLower',
    dependencyTypeToKind,
    unExe,
    unExeV,
    unDepV,
    fixName,
    getUrl,
  )
where

import Control.Applicative (Alternative ((<|>)))
import Control.Monad ((<=<))
import Data.Char (toLower)
import Data.List.Split (splitOn)
import Distribution.ArchHs.Types
import Distribution.PackageDescription (GenericPackageDescription, PackageDescription, homepage, package, packageDescription, repoLocation, sourceRepos)
import Distribution.Types.Dependency
import Distribution.Types.ExeDependency (ExeDependency (..))
import qualified Distribution.Types.PackageId as I
import Distribution.Types.PackageName (PackageName, unPackageName)
import Distribution.Utils.ShortText (fromShortText)
import Distribution.Version (Version, VersionRange)

-- | Extract the name from a 'ExeDependency'.
unExe :: ExeDependency -> PackageName
unExe (ExeDependency name _ _) = name

unExeV :: ExeDependency -> (PackageName, VersionRange)
unExeV (ExeDependency name _ v) = (name, v)

unDepV :: Dependency -> (PackageName, VersionRange)
unDepV dep = (depPkgName dep, depVerRange dep)

-- | Extract the package name from 'PackageDescription'.
getPkgName :: PackageDescription -> PackageName
getPkgName = I.pkgName . package

-- | Extract the package from 'GenericPackageDescription'.
getPkgName' :: GenericPackageDescription -> PackageName
getPkgName' = I.pkgName . package . packageDescription

-- | Extract the package version from 'PackageDescription'.
getPkgVersion :: PackageDescription -> Version
getPkgVersion = I.pkgVersion . package

-- | Extract the url from 'PackageDescription'.
-- It tries 'homepage', the head of 'sourceRepos', and finally fallback into hackage website.
getUrl :: PackageDescription -> String
getUrl cabal = fromJust $ home <|> vcs <|> fallback
  where
    f "" = Nothing
    f x = Just x
    fromJust (Just x) = x
    fromJust _ = fail "Impossible."
    safeHead [] = Nothing
    safeHead (x : _) = Just x
    home = f . fromShortText . homepage $ cabal
    vcs = repoLocation <=< safeHead . sourceRepos $ cabal
    fallback = Just $ "https://hackage.haskell.org/package/" <> (unPackageName $ getPkgName cabal)

-- | Convert the hackage name into archlinux package name follow the convention.
--
-- >>> fixName "haskell-A"
-- "haskell-a"
--
-- >>> fixName "QuickCheck"
-- "haskell-quickcheck"
fixName :: String -> String
fixName s = case splitOn "-" s of
  ("haskell" : _) -> toLower' s
  _ -> "haskell-" <> toLower' s

-- | Lower each 'Char's in 'String'.
toLower' :: String -> String
toLower' = fmap toLower

-- | Map 'DependencyType' with its data constructor tag 'DependencyKind'.
dependencyTypeToKind :: DependencyType -> DependencyKind
dependencyTypeToKind (CExe _) = Exe
dependencyTypeToKind (CExeBuildTools _) = ExeBuildTools
dependencyTypeToKind (CLib) = Lib
dependencyTypeToKind (CTest _) = Test
dependencyTypeToKind (CBenchmark _) = Benchmark
dependencyTypeToKind (CLibBuildTools) = LibBuildTools
dependencyTypeToKind (CTestBuildTools _) = TestBuildTools
dependencyTypeToKind (CBenchmarkBuildTools _) = BenchmarkBuildTools