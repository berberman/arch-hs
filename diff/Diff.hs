module Diff
  ( diffCabal,
    Options (..),
    runArgsParser,
  )
where

import qualified Colourista as C
import Core
import Data.List (intercalate, nub, (\\))
import Distribution.PackageDescription
import Distribution.Parsec (simpleParsec)
import Distribution.Pretty (prettyShow)
import qualified Distribution.Types.BuildInfo.Lens as L
import Distribution.Types.Dependency
import Distribution.Types.ExeDependency (ExeDependency (ExeDependency))
import qualified Distribution.Types.PackageId as I
import Distribution.Types.PackageName
import Distribution.Types.UnqualComponentName
import Distribution.Utils.ShortText (fromShortText)
import Distribution.Version
import Hackage
import Lens.Micro
import Local (ghcLibList, ignoreList)
import Options.Applicative
import Polysemy
import Types
import Utils

data Options = Options
  { optHackagePath :: FilePath,
    optPackageName :: PackageName,
    optVersionA :: Version,
    optVersionB :: Version
  }

cmdOptions :: Parser Options
cmdOptions =
  Options
    <$> strOption
      ( long "hackage"
          <> metavar "PATH"
          <> short 'h'
          <> help "Path to 01-index.tar"
          <> showDefault
          <> value "~/.cabal/packages/YOUR_HACKAGE_MIRROR/01-index.tar"
      )
    <*> argument optPackageNameReader (metavar "TARGET")
    <*> argument optVersionReader (metavar "VERSION_A")
    <*> argument optVersionReader (metavar "VERSION_B")

optVersionReader :: ReadM Version
optVersionReader =
  eitherReader
    ( \s -> case simpleParsec s of
        Just v -> Right v
        _ -> Left $ "Failed to parse version: " <> s
    )

optPackageNameReader :: ReadM PackageName
optPackageNameReader = eitherReader $ Right . mkPackageName

runArgsParser :: IO Options
runArgsParser =
  execParser $
    info
      (cmdOptions <**> helper)
      ( fullDesc
          <> progDesc "Try to reach the TARGET QAQ."
          <> header "arch-hs-diff - a program creating diff between different versions of a cabal file."
      )

-----------------------------------------------------------------------------

-- This parts are duplicated from Core.hs with modifications.

type VersionedList = [(PackageName, VersionRange)]

type VersionedComponentList = [(UnqualComponentName, VersionedList)]

unExe' :: ExeDependency -> (PackageName, VersionRange)
unExe' (ExeDependency name _ v) = (name, v)

collectLibDeps :: Members [HackageEnv, FlagAssignmentEnv] r => GenericPackageDescription -> Sem r (VersionedList, VersionedList)
collectLibDeps cabal = do
  case cabal & condLibrary of
    Just lib -> do
      bInfo <- evalConditionTree (getPkgName cabal) lib
      let libDeps = fmap (\x -> (depPkgName x, depVerRange x)) $ targetBuildDepends bInfo
          toolDeps = fmap unExe' $ buildToolDepends bInfo
      return (libDeps, toolDeps)
    Nothing -> return ([], [])

collectRunnableDeps ::
  (Semigroup k, L.HasBuildInfo k, Members [HackageEnv, FlagAssignmentEnv] r) =>
  (GenericPackageDescription -> [(UnqualComponentName, CondTree ConfVar [Dependency] k)]) ->
  GenericPackageDescription ->
  [UnqualComponentName] ->
  Sem r (VersionedComponentList, VersionedComponentList)
collectRunnableDeps f cabal skip = do
  let exes = cabal & f
  bInfo <- filter (not . (`elem` skip) . fst) . zip (exes <&> fst) <$> mapM (evalConditionTree (getPkgName cabal) . snd) exes
  let runnableDeps = bInfo <&> ((_2 %~) $ fmap (\x -> (depPkgName x, depVerRange x)) . targetBuildDepends)
      toolDeps = bInfo <&> ((_2 %~) $ fmap unExe' . buildToolDepends)
  return (runnableDeps, toolDeps)

collectExeDeps :: Members [HackageEnv, FlagAssignmentEnv] r => GenericPackageDescription -> [UnqualComponentName] -> Sem r (VersionedComponentList, VersionedComponentList)
collectExeDeps = collectRunnableDeps condExecutables

collectTestDeps :: Members [HackageEnv, FlagAssignmentEnv] r => GenericPackageDescription -> [UnqualComponentName] -> Sem r (VersionedComponentList, VersionedComponentList)
collectTestDeps = collectRunnableDeps condTestSuites

-----------------------------------------------------------------------------

diffCabal :: Members [HackageEnv, FlagAssignmentEnv, WithMyErr] r => PackageName -> Version -> Version -> Sem r String
diffCabal name a b = do
  ga <- getCabal name a
  gb <- getCabal name b
  let pa = packageDescription ga
      pb = packageDescription gb
  (ba, ma) <- directDependencies ga
  (bb, mb) <- directDependencies gb
  return $
    unlines
      [ "Package: " <> unPackageName name,
        ver pa pb,
        desc pa pb,
        dep "Depends: \n" ba bb,
        dep "MakeDepends: \n    " ma mb
      ]

directDependencies ::
  Members [HackageEnv, FlagAssignmentEnv, WithMyErr] r =>
  GenericPackageDescription ->
  Sem r ([String], [String])
directDependencies cabal = do
  (libDeps, libToolsDeps) <- collectLibDeps cabal
  (exeDeps, exeToolsDeps) <- collectExeDeps cabal []
  (testDeps, testToolsDeps) <- collectTestDeps cabal []
  let connectVersionWithName (n, range) = unPackageName n <> "  " <> prettyShow range
      flatten = fmap connectVersionWithName . mconcat . fmap snd
      l = fmap connectVersionWithName libDeps
      lt = fmap connectVersionWithName libToolsDeps
      e = flatten exeDeps
      et = flatten exeToolsDeps
      t = flatten testDeps
      tt = flatten testToolsDeps
      name = unPackageName $getPkgName cabal
      notInGHCLib = (`notElem` ghcLibList) . mkPackageName
      notInIgnore = (`notElem` ignoreList) . mkPackageName
      notMyself = (/= name)
      distinct =
        filter notInIgnore
          . filter notInGHCLib
          . filter notMyself
          . nub
      depends = distinct $ l ++ e
      makedepends = (distinct $ lt ++ et ++ t ++ tt) \\ depends
  return (depends, makedepends)

diffTerm :: String -> (a -> String) -> a -> a -> String
diffTerm s f a b = let (ra, rb) = (f a, f b) in s <> (if ra == rb then ra else (C.formatWith [C.red] $ ra <> "  ⇒  " <> rb))

desc :: PackageDescription -> PackageDescription -> String
desc = diffTerm "Synopsis: " $ fromShortText . synopsis

ver :: PackageDescription -> PackageDescription -> String
ver = diffTerm "Version: " $ intercalate "." . fmap show . versionNumbers . I.pkgVersion . package

dep :: String -> [String] -> [String] -> String
dep s a b =
  s <> case diffNew of
    [] -> joinToString a
    _ ->
      (C.formatWith [C.indent 4] (joinToString $ fmap (\x -> red (x `elem` diffOld) x) a))
        <> "\n"
        <> replicate 28 '-'
        <> "\n"
        <> (C.formatWith [C.indent 4] (joinToString $ fmap (\x -> green (x `elem` diffNew) x) b))
  where
    diffNew = b \\ a
    diffOld = a \\ b
    joinToString [] = "[]"
    joinToString xs = intercalate "\n    " xs
    red p x = if p then C.formatWith [C.red] x else x
    green p x = if p then C.formatWith [C.green] x else x