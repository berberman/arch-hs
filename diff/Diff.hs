{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Diff
  ( diffCabal,
    Options (..),
    runArgsParser,
  )
where

import qualified Colourista as C
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Distribution.ArchHs.Community (versionInCommunity)
import Distribution.ArchHs.Core (evalConditionTree)
import Distribution.ArchHs.Exception
import Distribution.ArchHs.Internal.Prelude
import Distribution.ArchHs.OptionReader
import Distribution.ArchHs.PP (prettyFlags)
import Distribution.ArchHs.Types
import Distribution.ArchHs.Utils
import Distribution.PackageDescription (CondTree, ConfVar)
import Distribution.PackageDescription.Parsec (parseGenericPackageDescriptionMaybe)
import qualified Distribution.Types.BuildInfo.Lens as L
import Distribution.Types.Dependency (Dependency)
import Distribution.Utils.ShortText (fromShortText)
import Network.HTTP.Req hiding (header)

data Options = Options
  { optFlags :: FlagAssignments,
#ifdef ALPM
    optAlpm :: Bool,
#else
  optCommunityPath :: FilePath,
#endif
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
#ifndef ALPM
    <*> strOption
      ( long "community"
          <> metavar "PATH"
          <> short 'c'
          <> help "Path to community.db"
          <> showDefault
          <> value "/var/lib/pacman/sync/community.db"
      )
#else
      <*> switch
        ( long "alpm"
            <> help "Use libalpm to parse community db"
        )
#endif
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

--  Duplicated from Core.hs with modifications.

type VersionedList = [(PackageName, VersionRange)]

type VersionedComponentList = [(UnqualComponentName, VersionedList)]

collectLibDeps :: Members [FlagAssignmentsEnv, Trace, DependencyRecord] r => GenericPackageDescription -> Sem r (VersionedList, VersionedList)
collectLibDeps cabal = do
  case cabal & condLibrary of
    Just lib -> do
      bInfo <- evalConditionTree cabal lib
      let libDeps = unDepV <$> buildDependsIfBuild bInfo
          toolDeps = unExeV <$> buildToolDependsIfBuild bInfo
      mapM_ (uncurry updateDependencyRecord) libDeps
      mapM_ (uncurry updateDependencyRecord) toolDeps
      return (libDeps, toolDeps)
    Nothing -> return ([], [])

collectRunnableDeps ::
  (Semigroup k, L.HasBuildInfo k, Members [FlagAssignmentsEnv, Trace, DependencyRecord] r) =>
  (GenericPackageDescription -> [(UnqualComponentName, CondTree ConfVar [Dependency] k)]) ->
  GenericPackageDescription ->
  [UnqualComponentName] ->
  Sem r (VersionedComponentList, VersionedComponentList)
collectRunnableDeps f cabal skip = do
  let exes = cabal & f
  bInfo <- filter (not . (`elem` skip) . fst) . zip (exes <&> fst) <$> mapM (evalConditionTree cabal . snd) exes
  let deps = bInfo <&> _2 %~ (fmap unDepV . buildDependsIfBuild)
      toolDeps = bInfo <&> _2 %~ (fmap unExeV . buildToolDependsIfBuild)
  mapM_ (uncurry updateDependencyRecord) $ deps ^.. each . _2 ^. each
  mapM_ (uncurry updateDependencyRecord) $ toolDeps ^.. each . _2 ^. each
  return (deps, toolDeps)

collectExeDeps :: Members [FlagAssignmentsEnv, Trace, DependencyRecord] r => GenericPackageDescription -> [UnqualComponentName] -> Sem r (VersionedComponentList, VersionedComponentList)
collectExeDeps = collectRunnableDeps condExecutables

collectTestDeps :: Members [FlagAssignmentsEnv, Trace, DependencyRecord] r => GenericPackageDescription -> [UnqualComponentName] -> Sem r (VersionedComponentList, VersionedComponentList)
collectTestDeps = collectRunnableDeps condTestSuites

updateDependencyRecord :: Member DependencyRecord r => PackageName -> VersionRange -> Sem r ()
updateDependencyRecord name range = modify' $ Map.insertWith (<>) name [range]

-----------------------------------------------------------------------------

getCabalFromHackage :: Members [Embed IO, WithMyErr] r => PackageName -> Version -> Sem r GenericPackageDescription
getCabalFromHackage name version = do
  let urlPath = T.pack $ unPackageName name <> "-" <> prettyShow version
      api = https "hackage.haskell.org" /: "package" /: urlPath /: "revision" /: "0.cabal"
      r = req GET api NoReqBody bsResponse mempty
  embed $ C.infoMessage $ "Downloading cabal file from " <> renderUrl api <> "..."
  response <- interceptHttpException (runReq defaultHttpConfig r)
  case parseGenericPackageDescriptionMaybe $ responseBody response of
    Just x -> return x
    _ -> error $ "Failed to parse .cabal file from " <> show api

directDependencies ::
  Members [FlagAssignmentsEnv, Trace, DependencyRecord] r =>
  GenericPackageDescription ->
  Sem r (VersionedList, VersionedList)
directDependencies cabal = do
  (libDeps, libToolsDeps) <- collectLibDeps cabal
  (exeDeps, exeToolsDeps) <- collectExeDeps cabal []
  (testDeps, testToolsDeps) <- collectTestDeps cabal []
  let flatten = mconcat . fmap snd
      l = libDeps
      lt = libToolsDeps
      e = flatten exeDeps
      et = flatten exeToolsDeps
      t = flatten testDeps
      tt = flatten testToolsDeps
      notMyself = (/= getPkgName' cabal)
      distinct = filter (notMyself . fst) . nub
      depends = distinct $ l <> e
      makedepends = distinct (lt <> et <> t <> tt) \\ depends
  return (depends, makedepends)

-----------------------------------------------------------------------------

diffCabal :: Members [CommunityEnv, FlagAssignmentsEnv, WithMyErr, Trace, DependencyRecord, Embed IO] r => PackageName -> Version -> Version -> Sem r String
diffCabal name a b = do
  ga <- getCabalFromHackage name a
  gb <- getCabalFromHackage name b
  let pa = packageDescription ga
      pb = packageDescription gb
      fa = genPackageFlags ga
      fb = genPackageFlags ga
  (ba, ma) <- directDependencies ga
  (bb, mb) <- directDependencies gb
  queryb <- lookupDiffCommunity ba bb
  querym <- lookupDiffCommunity ma mb
  return $
    unlines
      [ C.formatWith [C.magenta] "Package: " <> unPackageName name,
        ver pa pb,
        desc pa pb,
        url pa pb,
        dep "Depends: \n" ba bb,
        "",
        queryb,
        dep "MakeDepends: \n" ma mb,
        "",
        querym,
        flags name fa fb
      ]

diffTerm :: String -> (a -> String) -> a -> a -> String
diffTerm s f a b =
  let f' = T.unpack . T.strip . T.pack . f
      (ra, rb) = (f' a, f' b)
   in C.formatWith [C.magenta] s
        <> (if ra == rb then ra else C.formatWith [C.red] ra <> "  ⇒  " <> C.formatWith [C.green] rb)

desc :: PackageDescription -> PackageDescription -> String
desc = diffTerm "Synopsis: " $ fromShortText . synopsis

ver :: PackageDescription -> PackageDescription -> String
ver = diffTerm "Version: " (prettyShow . getPkgVersion)

url :: PackageDescription -> PackageDescription -> String
url = diffTerm "URL: " getUrl

splitLine :: String
splitLine = "\n" <> replicate 38 '-' <> "\n"

inRange :: Members [CommunityEnv, WithMyErr] r => (PackageName, VersionRange) -> Sem r (Either (PackageName, VersionRange) (PackageName, VersionRange, Version, Bool))
inRange (name, hRange) =
  try @MyException (versionInCommunity name)
    >>= \case
      Right y -> let version = fromJust . simpleParsec $ y in return . Right $ (name, hRange, version, withinRange version hRange)
      Left _ -> return . Left $ (name, hRange)

lookupDiffCommunity :: Members [CommunityEnv, WithMyErr] r => VersionedList -> VersionedList -> Sem r String
lookupDiffCommunity va vb = do
  let diffNew = vb \\ va
      diffOld = va \\ vb
      color b = C.formatWith [if b then C.green else C.red]
      pp b (Right (name, range, v, False)) =
        "\""
          <> color b (unPackageName name)
          <> "\" is required to be in range ("
          <> color b (prettyShow range)
          <> "), "
          <> "but [community] provides ("
          <> color b (prettyShow v)
          <> ")."
      pp _ (Right _) = ""
      pp b (Left (name, range)) =
        "\""
          <> color b (unPackageName name)
          <> "\" is required to be in range ("
          <> color b (prettyShow range)
          <> "), "
          <> "but [community] does not provide this package."

  new <- fmap (pp True) <$> mapM inRange diffNew
  old <- fmap (pp False) <$> mapM inRange diffOld
  let join = unlines . filter (not . null)
  return $ join old <> join new

dep :: String -> VersionedList -> VersionedList -> String
dep s va vb =
  C.formatWith [C.magenta] s <> "    " <> case diffOld <> diffNew of
    [] -> joinToString a
    _ ->
      joinToString (fmap (\x -> red (x `elem` diffOld) x) a)
        <> splitLine
        <> "    "
        <> joinToString (fmap (\x -> green (x `elem` diffNew) x) b)
  where
    a = joinVersionWithName <$> va
    b = joinVersionWithName <$> vb
    diffNew = b \\ a
    diffOld = a \\ b
    joinToString [] = "[]"
    joinToString xs = intercalate "\n    " $ sort xs
    joinVersionWithName (n, range) = unPackageName n <> "  " <> prettyShow range
    red p x = if p then C.formatWith [C.red] x else x
    green p x = if p then C.formatWith [C.green] x else x

flags :: PackageName -> [Flag] -> [Flag] -> String
flags name a b =
  C.formatWith [C.magenta] "Flags:\n" <> "  " <> case diffOld <> diffNew of
    [] -> joinToString a
    _ ->
      joinToString a
        <> splitLine
        <> "    "
        <> joinToString b
  where
    diffNew = b \\ a
    diffOld = a \\ b
    joinToString [] = "[]"
    joinToString xs = prettyFlags [(name, xs)]
