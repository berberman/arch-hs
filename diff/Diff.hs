{-# LANGUAGE OverloadedStrings #-}

module Diff
  ( diffCabal,
    Options (..),
    runArgsParser,
  )
where

import qualified Colourista as C
import qualified Control.Exception as CE
import Data.List (intercalate, nub, sort, (\\))
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Distribution.ArchHs.Core
import Distribution.ArchHs.PP (prettyFlags)
import Distribution.ArchHs.Types
import Distribution.ArchHs.Utils
import Distribution.PackageDescription
import Distribution.PackageDescription.Parsec (parseGenericPackageDescriptionMaybe)
import Distribution.Pretty (prettyShow)
import qualified Distribution.Types.BuildInfo.Lens as L
import Distribution.Types.Dependency
import Distribution.Types.PackageName
import Distribution.Types.UnqualComponentName
import Distribution.Utils.ShortText (fromShortText)
import Distribution.Version
import Network.HTTP.Req hiding (header)
import OptionParse

data Options = Options
  { optFlags :: FlagAssignments,
    optPackageName :: PackageName,
    optVersionA :: Version,
    optVersionB :: Version
  }

cmdOptions :: Parser Options
cmdOptions =
  Options
    <$> option
      optFlagReader
      ( long "flags"
          <> metavar "package_name:flag_name:true|false,..."
          <> short 'f'
          <> help "Flag assignments for packages - e.g. inline-c:gsl-example:true (separated by ',')"
          <> value Map.empty
      )
    <*> argument optPackageNameReader (metavar "TARGET")
    <*> argument optVersionReader (metavar "VERSION_A")
    <*> argument optVersionReader (metavar "VERSION_B")

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

collectLibDeps :: Member FlagAssignmentsEnv r => GenericPackageDescription -> Sem r (VersionedList, VersionedList)
collectLibDeps cabal = do
  case cabal & condLibrary of
    Just lib -> do
      bInfo <- evalConditionTree cabal lib
      let libDeps = fmap unDepV $ buildDependsIfBuild bInfo
          toolDeps = fmap unExeV $ buildToolDependsIfBuild bInfo
      return (libDeps, toolDeps)
    Nothing -> return ([], [])

collectRunnableDeps ::
  (Semigroup k, L.HasBuildInfo k, Member FlagAssignmentsEnv r) =>
  (GenericPackageDescription -> [(UnqualComponentName, CondTree ConfVar [Dependency] k)]) ->
  GenericPackageDescription ->
  [UnqualComponentName] ->
  Sem r (VersionedComponentList, VersionedComponentList)
collectRunnableDeps f cabal skip = do
  let exes = cabal & f
  bInfo <- filter (not . (`elem` skip) . fst) . zip (exes <&> fst) <$> mapM (evalConditionTree cabal . snd) exes
  let runnableDeps = bInfo <&> ((_2 %~) $ fmap unDepV . buildDependsIfBuild)
      toolDeps = bInfo <&> ((_2 %~) $ fmap unExeV . buildToolDependsIfBuild)
  return (runnableDeps, toolDeps)

collectExeDeps :: Member FlagAssignmentsEnv r => GenericPackageDescription -> [UnqualComponentName] -> Sem r (VersionedComponentList, VersionedComponentList)
collectExeDeps = collectRunnableDeps condExecutables

collectTestDeps :: Member FlagAssignmentsEnv r => GenericPackageDescription -> [UnqualComponentName] -> Sem r (VersionedComponentList, VersionedComponentList)
collectTestDeps = collectRunnableDeps condTestSuites

getCabalFromHackage :: Members [Embed IO, WithMyErr] r => PackageName -> Version -> Sem r GenericPackageDescription
getCabalFromHackage name version = do
  let urlPath = T.pack $ unPackageName name <> "-" <> prettyShow version
      api = https "hackage.haskell.org" /: "package" /: urlPath /: "revision" /: "0.cabal"
      r = req GET api NoReqBody bsResponse mempty
  embed $ C.infoMessage $ "Downloading cabal file from " <> renderUrl api <> "..."
  response <- embed $ CE.try @HttpException (runReq defaultHttpConfig r)
  result <- case response of
    Left _ -> throw $ VersionError name version
    Right x -> return x
  case parseGenericPackageDescriptionMaybe $ responseBody result of
    Just x -> return x
    _ -> embed @IO $ fail $ "Failed to parse .cabal file from " <> show api

-----------------------------------------------------------------------------

diffCabal :: Members [FlagAssignmentsEnv, WithMyErr, Embed IO] r => PackageName -> Version -> Version -> Sem r String
diffCabal name a b = do
  ga <- getCabalFromHackage name a
  gb <- getCabalFromHackage name b
  let pa = packageDescription ga
      pb = packageDescription gb
      fa = genPackageFlags ga
      fb = genPackageFlags ga
  (ba, ma) <- directDependencies ga
  (bb, mb) <- directDependencies gb
  return $
    unlines
      [ C.formatWith [C.magenta] "Package: " <> unPackageName name,
        ver pa pb,
        desc pa pb,
        url pa pb,
        dep "Depends: \n" ba bb,
        dep "MakeDepends: \n" ma mb,
        flags name fa fb
      ]

directDependencies ::
  Member FlagAssignmentsEnv r =>
  GenericPackageDescription ->
  Sem r ([String], [String])
directDependencies cabal = do
  (libDeps, libToolsDeps) <- collectLibDeps cabal
  (exeDeps, exeToolsDeps) <- collectExeDeps cabal []
  (testDeps, testToolsDeps) <- collectTestDeps cabal []
  let connectVersionWithName (n, range) = unPackageName n <> "  " <> prettyShow range
      flatten = mconcat . fmap snd
      l = libDeps
      lt = libToolsDeps
      e = flatten exeDeps
      et = flatten exeToolsDeps
      t = flatten testDeps
      tt = flatten testToolsDeps
      notMyself = (/= (getPkgName' cabal))
      distinct = filter (notMyself . fst) . nub
      depends = distinct $ l <> e
      makedepends = (distinct $ lt <> et <> t <> tt) \\ depends
  return (fmap connectVersionWithName depends, fmap connectVersionWithName makedepends)

diffTerm :: String -> (a -> String) -> a -> a -> String
diffTerm s f a b =
  let (ra, rb) = (f a, f b)
   in (C.formatWith [C.magenta] s)
        <> (if ra == rb then ra else ((C.formatWith [C.red] ra) <> "  ⇒  " <> C.formatWith [C.green] rb))

desc :: PackageDescription -> PackageDescription -> String
desc = diffTerm "Synopsis: " $ fromShortText . synopsis

ver :: PackageDescription -> PackageDescription -> String
ver = diffTerm "Version: " (prettyShow . getPkgVersion)

url :: PackageDescription -> PackageDescription -> String
url = diffTerm "URL: " getUrl

dep :: String -> [String] -> [String] -> String
dep s a b =
  (C.formatWith [C.magenta] s) <> "    " <> case (diffOld <> diffNew) of
    [] -> joinToString a
    _ ->
      (joinToString $ fmap (\x -> red (x `elem` diffOld) x) a)
        <> "\n"
        <> replicate 38 '-'
        <> "\n"
        <> "    "
        <> (joinToString $ fmap (\x -> green (x `elem` diffNew) x) b)
  where
    diffNew = b \\ a
    diffOld = a \\ b
    joinToString [] = "[]"
    joinToString xs = intercalate "\n    " $ sort xs
    red p x = if p then C.formatWith [C.red] x else x
    green p x = if p then C.formatWith [C.green] x else x

flags :: PackageName -> [Flag] -> [Flag] -> String
flags name a b =
  (C.formatWith [C.magenta] "Flags:\n") <> "  " <> case (diffOld <> diffNew) of
    [] -> joinToString a
    _ ->
      (joinToString a)
        <> "\n"
        <> replicate 38 '-'
        <> "\n"
        <> "    "
        <> (joinToString b)
  where
    diffNew = b \\ a
    diffOld = a \\ b
    joinToString [] = "[]"
    joinToString xs = prettyFlags [(name, xs)]