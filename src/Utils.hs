module Utils
  ( getPkgName,
    getPkgName',
    getPkgVersion,
    toLower',
    dependencyTypeToKind,
    unExe,
    fixName,
    getUrl,
  )
where

import Control.Applicative (Alternative ((<|>)))
import Control.Monad ((<=<))
import Data.Char (toLower)
import Data.List.Split (splitOn)
import Distribution.PackageDescription (GenericPackageDescription, PackageDescription, homepage, package, packageDescription, repoLocation, sourceRepos)
import Distribution.Types.ExeDependency (ExeDependency (..))
import qualified Distribution.Types.PackageId as I
import Distribution.Types.PackageName (PackageName, unPackageName)
import Distribution.Utils.ShortText (fromShortText)
import Distribution.Version (Version)
import Types

unExe :: ExeDependency -> PackageName
unExe (ExeDependency name _ _) = name

getPkgName :: PackageDescription -> PackageName
getPkgName = I.pkgName . package

getPkgName' :: GenericPackageDescription -> PackageName
getPkgName' = I.pkgName . package . packageDescription

getPkgVersion :: PackageDescription -> Version
getPkgVersion = I.pkgVersion . package

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

fixName :: String -> String
fixName s = case splitOn "-" s of
  ("haskell" : _) -> toLower' s
  _ -> "haskell-" <> toLower' s

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