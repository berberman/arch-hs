-- | Copyright: (c) 2020 berberman
-- SPDX-License-Identifier: MIT
-- Maintainer: berberman <1793913507@qq.com>
-- Stability: experimental
-- Portability: portable
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
    getTwo,
    buildDependsIfBuild,
    buildToolDependsIfBuild,
    traceCallStack,
    trace',
  )
where

import           Control.Applicative              (Alternative ((<|>)))
import           Control.Monad                    ((<=<))
import           Data.Char                        (toLower)
import           Data.List.Split                  (splitOn)
import           Distribution.ArchHs.Types
import           Distribution.PackageDescription  (GenericPackageDescription,
                                                   PackageDescription, homepage,
                                                   package, packageDescription,
                                                   repoLocation, sourceRepos)
import           Distribution.Types.BuildInfo     (BuildInfo (..))
import           Distribution.Types.Dependency    (Dependency, depPkgName,
                                                   depVerRange)
import           Distribution.Types.ExeDependency (ExeDependency (..))
import qualified Distribution.Types.PackageId     as I
import           Distribution.Types.PackageName   (PackageName, unPackageName)
import           Distribution.Utils.ShortText     (fromShortText)
import           Distribution.Version             (Version, VersionRange)
import           GHC.Stack                        (callStack, prettyCallStack)

-- | Extract the package name from a 'ExeDependency'.
unExe :: ExeDependency -> PackageName
unExe (ExeDependency name _ _) = name

-- | Extract the package name and the version range from a 'ExeDependency'.
unExeV :: ExeDependency -> (PackageName, VersionRange)
unExeV (ExeDependency name _ v) = (name, v)

-- | Extract the 'PackageName' and 'VersionRange' of a 'Dependency'.
unDepV :: Dependency -> (PackageName, VersionRange)
unDepV dep = (depPkgName dep, depVerRange dep)

-- | Extract the package name from 'PackageDescription'.
getPkgName :: PackageDescription -> PackageName
getPkgName = I.pkgName . package

-- | Extract the package name from 'GenericPackageDescription'.
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
    f x  = Just x
    fromJust (Just x) = x
    fromJust _        = fail "Impossible."
    safeHead []      = Nothing
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
  _               -> "haskell-" <> toLower' s

-- | Lower each 'Char's in 'String'.
toLower' :: String -> String
toLower' = fmap toLower

-- | Map 'DependencyType' with its data constructor tag 'DependencyKind'.
dependencyTypeToKind :: DependencyType -> DependencyKind
dependencyTypeToKind (CExe _)                 = Exe
dependencyTypeToKind (CExeBuildTools _)       = ExeBuildTools
dependencyTypeToKind (CLib)                   = Lib
dependencyTypeToKind (CTest _)                = Test
dependencyTypeToKind (CBenchmark _)           = Benchmark
dependencyTypeToKind (CLibBuildTools)         = LibBuildTools
dependencyTypeToKind (CTestBuildTools _)      = TestBuildTools
dependencyTypeToKind (CBenchmarkBuildTools _) = BenchmarkBuildTools
dependencyTypeToKind (CSubLibs _)             = SubLibs
dependencyTypeToKind (CSubLibsBuildTools _)   = SubLibsBuildTools

-- | Apply a 'Getting' to two values respectively, and get the result as a pair.
getTwo :: Getting b s b -> s -> s -> (b, b)
getTwo l a b = (a, b) & both %~ (^. l)

-- | Same as 'targetBuildDepends', but check if this is 'buildable'.
buildDependsIfBuild :: BuildInfo -> [Dependency]
buildDependsIfBuild info = if buildable info then targetBuildDepends info else []

-- | Same as 'buildToolDepends', but check if this is 'buildable'.
buildToolDependsIfBuild :: BuildInfo -> [ExeDependency]
buildToolDependsIfBuild info = if buildable info then buildToolDepends info else []

-- | Trace with prefix @[TRACE]@.
trace' :: MemberWithError Trace r => String -> Sem r ()
trace' s = trace $ "[TRACE]  " <> s

-- | Trace 'GHC.Stack.CallStack'.
traceCallStack :: (HasCallStack, MemberWithError Trace r) => Sem r ()
traceCallStack = do
  trace . prefix $ prettyCallStack callStack
  where
    prefix = unlines . fmap ("[TRACE]  " ++) . lines
