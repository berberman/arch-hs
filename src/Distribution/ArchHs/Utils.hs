{-# LANGUAGE ViewPatterns #-}

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
    dependencyTypeToKind,
    unExe,
    unExeV,
    unLegacyExeV,
    unBuildTools,
    unSystemDependency,
    unDepV,
    getUrl,
    getTwo,
    buildDependsIfBuild,
    buildToolsAndbuildToolDependsIfBuild,
    pkgconfigDependsAndExtraLibsIfBuild,
    traceCallStack,
    trace',
    depNotInGHCLib,
    depNotMyself,
    depIsKind,
    extractFromEVR,
    isProvided,
    filterFirstDiff,
    filterFirstAndBothDiff,
    filterSecondDiff,
    filterSecondAndBothDiff,
    noDiff,
    mapDiff,
  )
where

import Control.Monad ((<=<))
import Data.Algorithm.Diff
import Distribution.ArchHs.Internal.Prelude
import Distribution.ArchHs.Local (ghcLibList)
import Distribution.ArchHs.Types
import Distribution.PackageDescription (repoLocation)
import Distribution.Types.BuildInfo (BuildInfo (..))
import Distribution.Types.Dependency (Dependency, depPkgName, depVerRange)
import Distribution.Types.ExeDependency (ExeDependency (..))
import Distribution.Types.LegacyExeDependency
import qualified Distribution.Types.PackageId as I
import Distribution.Types.PkgconfigDependency
import Distribution.Types.PkgconfigName
import Distribution.Utils.ShortText (fromShortText)
import GHC.Stack (callStack, prettyCallStack)

-- | Extract the package name from a 'ExeDependency'.
unExe :: ExeDependency -> PackageName
unExe (ExeDependency name _ _) = name

-- | Extract the package name and the version range from a 'ExeDependency'.
unExeV :: ExeDependency -> (PackageName, VersionRange)
unExeV (ExeDependency name _ v) = (name, v)

-- | Extract the package name and the version range from a 'LegacyExeDependency'.
unLegacyExeV :: LegacyExeDependency -> (PackageName, VersionRange)
unLegacyExeV (LegacyExeDependency name v) = (mkPackageName name, v)

-- | Extract and join package names and version ranges of '[LegacyExeDependency]' and '[ExeDependency]'.
unBuildTools :: ([LegacyExeDependency], [ExeDependency]) -> [(PackageName, VersionRange)]
unBuildTools (l, e) = (unLegacyExeV <$> l) <> (unExeV <$> e)

unSystemDependency :: ([PkgconfigDependency], [String]) -> [SystemDependency]
unSystemDependency (p, s) = [SystemDependency $ name <> ".pc" | (PkgconfigDependency (unPkgconfigName -> name) _) <- p] <> [SystemDependency $ "lib" <> name <> ".so" | name <- s]

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
    f x = Just x
    fromJust (Just x) = x
    fromJust _ = fail "Impossible."
    home = f . fromShortText . homepage $ cabal
    vcs = repoLocation <=< (^? ix 0) . sourceRepos $ cabal
    fallback = Just $ "https://hackage.haskell.org/package/" <> unPackageName (getPkgName cabal)

-- | Map 'DependencyType' with its data constructor tag 'DependencyKind'.
dependencyTypeToKind :: DependencyType -> DependencyKind
dependencyTypeToKind (CExe _) = Exe
dependencyTypeToKind (CExeBuildTools _) = ExeBuildTools
dependencyTypeToKind CLib = Lib
dependencyTypeToKind (CTest _) = Test
dependencyTypeToKind (CBenchmark _) = Benchmark
dependencyTypeToKind CLibBuildTools = LibBuildTools
dependencyTypeToKind (CTestBuildTools _) = TestBuildTools
dependencyTypeToKind (CBenchmarkBuildTools _) = BenchmarkBuildTools
dependencyTypeToKind (CSubLibs _) = SubLibs
dependencyTypeToKind (CSubLibsBuildTools _) = SubLibsBuildTools

-- | Apply a 'Getting' to two values respectively, and get the result as a pair.
getTwo :: Getting b s b -> s -> s -> (b, b)
getTwo l a b = (a, b) & both %~ (^. l)

-- | Same as 'targetBuildDepends', but check if this is 'buildable'.
buildDependsIfBuild :: BuildInfo -> [Dependency]
buildDependsIfBuild info = whenBuildable [] info targetBuildDepends

-- | 'buildToolDepends' combined with 'buildTools', and check if this is 'buildable'.
-- Actually, we should avoid accessing these two fields directly, in in favor of 'Distribution.Simple.BuildToolDepends.getAllToolDependencies'
buildToolsAndbuildToolDependsIfBuild :: BuildInfo -> ([LegacyExeDependency], [ExeDependency])
buildToolsAndbuildToolDependsIfBuild info = whenBuildable ([], []) info $ \i -> (buildTools i, buildToolDepends i)

pkgconfigDependsAndExtraLibsIfBuild :: BuildInfo -> ([PkgconfigDependency], [String])
pkgconfigDependsAndExtraLibsIfBuild info = whenBuildable ([], []) info $ \i -> (pkgconfigDepends i, extraLibs i)

whenBuildable :: a -> BuildInfo -> (BuildInfo -> a) -> a
whenBuildable def info f
  | buildable info = f info
  | otherwise = def

-- | Trace with prefix @[TRACE]@.
trace' :: MemberWithError Trace r => String -> Sem r ()
trace' s = trace $ "[TRACE]  " <> s

-- | Trace 'GHC.Stack.CallStack'.
traceCallStack :: (HasCallStack, MemberWithError Trace r) => Sem r ()
traceCallStack = do
  trace . prefix $ prettyCallStack callStack
  where
    prefix = unlines . fmap ("[TRACE]  " ++) . lines

-- | 'SolvedDependency' @x@' is not provided by ghc.
depNotInGHCLib :: SolvedDependency -> Bool
depNotInGHCLib x = (x ^. depName) `notElem` ghcLibList

-- | 'SolvedDependency' @x@'s name is not equal to @name@.
depNotMyself :: PackageName -> SolvedDependency -> Bool
depNotMyself name x = x ^. depName /= name

-- | 'SolvedDependency' @x@' has 'DependencyKind' @k@.
depIsKind :: DependencyKind -> SolvedDependency -> Bool
depIsKind k x = k `elem` (x ^. depType <&> dependencyTypeToKind)

-- | Extract package version from epoch-version-release.
--
-- >>> extractFromEVR "8.10.2-1"
-- "8.10.2"
-- >>> extractFromEVR "3:2.4.11-19"
-- "2.4.11"
extractFromEVR :: String -> ArchLinuxVersion
extractFromEVR evr =
  let ev = head $ splitOn "-" evr
   in if ':' `elem` ev then tail $ dropWhile (/= ':') ev else ev

-- | Whether a 'SolvedPackage' is provided
isProvided :: SolvedPackage -> Bool
isProvided (ProvidedPackage _ _) = True
isProvided _ = False

filterFirstDiff :: [Diff a] -> [Diff a]
filterFirstDiff = filter (\case First _ -> True; _ -> False)

filterSecondDiff :: [Diff a] -> [Diff a]
filterSecondDiff = filter (\case Second _ -> True; _ -> False)

filterFirstAndBothDiff :: [Diff a] -> [Diff a]
filterFirstAndBothDiff = filter (\case Second _ -> False; _ -> True)

filterSecondAndBothDiff :: [Diff a] -> [Diff a]
filterSecondAndBothDiff = filter (\case First _ -> False; _ -> True)

noDiff :: [Diff a] -> Bool
noDiff = all (\case Both _ _ -> True; _ -> False)

mapDiff :: (a -> b) -> Diff a -> Diff b
mapDiff f (First x) = First $ f x
mapDiff f (Second x) = Second $ f x
mapDiff f (Both x y) = Both (f x) (f y)