{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Diff
  ( diffCabal,
  )
where

import Data.Algorithm.Diff
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromJust)
import Distribution.ArchHs.CommunityDB (versionInCommunity)
import Distribution.ArchHs.Core (evalConditionTree)
import Distribution.ArchHs.Exception
import Distribution.ArchHs.Internal.Prelude
import Distribution.ArchHs.PP
import Distribution.ArchHs.Types
import Distribution.ArchHs.Utils
import Distribution.PackageDescription (CondTree, ConfVar)
import Distribution.PackageDescription.Parsec (parseGenericPackageDescriptionMaybe)
import qualified Distribution.Types.BuildInfo.Lens as L
import Distribution.Types.Dependency (Dependency)
import Distribution.Types.SetupBuildInfo
import Distribution.Utils.ShortText (fromShortText)
import Network.HTTP.Client

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
          toolDeps = unBuildTools $ buildToolsAndbuildToolDependsIfBuild bInfo
      mapM_ (uncurry updateDependencyRecord) libDeps
      mapM_ (uncurry updateDependencyRecord) toolDeps
      return (libDeps, toolDeps)
    Nothing -> return ([], [])

collectComponentialDeps ::
  (Semigroup k, L.HasBuildInfo k, Members [FlagAssignmentsEnv, Trace, DependencyRecord] r) =>
  (GenericPackageDescription -> [(UnqualComponentName, CondTree ConfVar [Dependency] k)]) ->
  GenericPackageDescription ->
  [UnqualComponentName] ->
  Sem r (VersionedComponentList, VersionedComponentList)
collectComponentialDeps f cabal skip = do
  let exes = cabal & f
  bInfo <- filter (not . (`elem` skip) . fst) . zip (exes <&> fst) <$> mapM (evalConditionTree cabal . snd) exes
  let deps = bInfo <&> _2 %~ (fmap unDepV . buildDependsIfBuild)
      toolDeps = bInfo <&> _2 %~ (unBuildTools . buildToolsAndbuildToolDependsIfBuild)
  mapM_ (uncurry updateDependencyRecord) $ deps ^.. each . _2 ^. each
  mapM_ (uncurry updateDependencyRecord) $ toolDeps ^.. each . _2 ^. each
  return (deps, toolDeps)

collectExeDeps :: Members [FlagAssignmentsEnv, Trace, DependencyRecord] r => GenericPackageDescription -> [UnqualComponentName] -> Sem r (VersionedComponentList, VersionedComponentList)
collectExeDeps = collectComponentialDeps condExecutables

collectTestDeps :: Members [FlagAssignmentsEnv, Trace, DependencyRecord] r => GenericPackageDescription -> [UnqualComponentName] -> Sem r (VersionedComponentList, VersionedComponentList)
collectTestDeps = collectComponentialDeps condTestSuites

collectSubLibDeps :: Members [FlagAssignmentsEnv, Trace, DependencyRecord] r => GenericPackageDescription -> [UnqualComponentName] -> Sem r (VersionedComponentList, VersionedComponentList)
collectSubLibDeps = collectComponentialDeps condSubLibraries

collectSetupDeps :: Member Trace r => GenericPackageDescription -> Sem r VersionedList
collectSetupDeps cabal = do
  let name = getPkgName' cabal
  trace' $ "Getting setup dependencies of " <> show name
  case setupBuildInfo $ packageDescription cabal of
    Just (SetupBuildInfo deps _) -> do
      let result = unDepV <$> deps
      trace' $ "Found: " <> show result
      return result
    _ -> return []

updateDependencyRecord :: Member DependencyRecord r => PackageName -> VersionRange -> Sem r ()
updateDependencyRecord name range = modify' $ Map.insertWith (<>) name [range]

-----------------------------------------------------------------------------

getCabalFromHackage :: Members [Embed IO, WithMyErr, Reader Manager] r => PackageName -> Version -> Sem r GenericPackageDescription
getCabalFromHackage name version = do
  let urlPath = unPackageName name <> "-" <> prettyShow version
      s = "https://hackage.haskell.org/package/" <> urlPath <> "/revision/0.cabal"
  req <- interceptHttpException $ parseRequest s
  printInfo $ "Downloading cabal file from" <+> pretty s
  manager <- ask @Manager
  response <- interceptHttpException $ httpLbs req manager
  case parseGenericPackageDescriptionMaybe . LBS.toStrict $ responseBody response of
    Just x -> return x
    _ -> error "Failed to parse .cabal file"

directDependencies ::
  Members [FlagAssignmentsEnv, Trace, DependencyRecord] r =>
  GenericPackageDescription ->
  Sem r (VersionedList, VersionedList)
directDependencies cabal = do
  (libDeps, libToolsDeps) <- collectLibDeps cabal
  (subLibDeps, subLibToolsDeps) <- collectSubLibDeps cabal []
  (exeDeps, exeToolsDeps) <- collectExeDeps cabal []
  (testDeps, testToolsDeps) <- collectTestDeps cabal []
  setupDeps <- collectSetupDeps cabal
  let flatten = mconcat . fmap snd
      l = libDeps
      lt = libToolsDeps
      sl = flatten subLibDeps
      slt = flatten subLibToolsDeps
      e = flatten exeDeps
      et = flatten exeToolsDeps
      t = flatten testDeps
      tt = flatten testToolsDeps
      notMyself = (/= getPkgName' cabal)
      distinct = filter (notMyself . fst) . nub
      depends = distinct $ l <> sl <> e
      makedepends = distinct (lt <> slt <> et <> t <> tt <> setupDeps) \\ depends
      sort' = sortBy (\x y -> uncurry compare $ getTwo _1 x y)
  return (sort' depends, sort' makedepends)

-----------------------------------------------------------------------------

diffCabal :: Members [CommunityEnv, FlagAssignmentsEnv, WithMyErr, Trace, DependencyRecord, Reader Manager, Embed IO] r => PackageName -> Version -> Version -> Sem r ()
diffCabal name a b = do
  ga <- getCabalFromHackage name a
  gb <- getCabalFromHackage name b
  let pa = packageDescription ga
      pb = packageDescription gb
      fa = genPackageFlags ga
      fb = genPackageFlags gb
  (ba, ma) <- directDependencies ga
  (bb, mb) <- directDependencies gb
  queryb <- lookupDiffCommunity ba bb
  querym <- lookupDiffCommunity ma mb
  embed . putDoc $
    vsep
      [ annMagneta "Package" <> colon <+> viaPretty name,
        ver pa pb,
        desc pa pb,
        url pa pb,
        dep "Depends" ba bb,
        queryb,
        dep "MakeDepends" ma mb,
        querym,
        flags name fa fb
      ]
      <> line

diffTerm :: String -> (a -> String) -> a -> a -> Doc AnsiStyle
diffTerm s f a b =
  let (ra, rb) = (f a, f b)
   in annMagneta (pretty s) <> colon
        <+> ( if ra == rb
                then pretty ra
                else ppFromTo 2 (annRed (pretty ra)) (annGreen (pretty rb))
            )

desc :: PackageDescription -> PackageDescription -> Doc AnsiStyle
desc = diffTerm "Synopsis" $ fromShortText . synopsis

ver :: PackageDescription -> PackageDescription -> Doc AnsiStyle
ver = diffTerm "Version" (prettyShow . getPkgVersion)

url :: PackageDescription -> PackageDescription -> Doc AnsiStyle
url = diffTerm "URL" getUrl

inRange :: Members [CommunityEnv, WithMyErr] r => (PackageName, VersionRange) -> Sem r (Either (PackageName, VersionRange) (PackageName, VersionRange, Version, Bool))
inRange (name, hRange) =
  try @MyException (versionInCommunity name)
    >>= \case
      Right y -> let version = fromJust . simpleParsec $ y in return . Right $ (name, hRange, version, withinRange version hRange)
      Left _ -> return . Left $ (name, hRange)

lookupDiffCommunity :: Members [CommunityEnv, WithMyErr] r => VersionedList -> VersionedList -> Sem r (Doc AnsiStyle)
lookupDiffCommunity va vb = do
  let diff = getGroupedDiff va vb
      diffOld = mconcat $ unDiff <$> filterFirstDiff diff
      diffNew = mconcat $ unDiff <$> filterSecondDiff diff
      annF b = if b then annGreen else annRed
      pp b (Right (name, range, v, False)) =
        Just $
          dquotes (annF b $ viaPretty name)
            <+> "is required to be in range"
            <+> parens (annF b $ viaPretty range)
            <> comma
            <+> "but"
            <+> ppCommunity
            <+> "provides"
            <+> parens (annF b $ viaPretty v)
            <> dot
      pp _ (Right _) = Nothing
      pp b (Left (name, range)) =
        Just $
          dquotes (annF b $ viaPretty name)
            <+> "is required to be in range"
            <+> parens (annF b $ viaPretty range)
            <> comma
            <+> "but"
            <+> ppCommunity
            <+> "does not provide this package"
            <> dot

  new <- fmap (pp True) <$> mapM inRange diffNew
  old <- fmap (pp False) <$> mapM inRange diffOld
  return $ vsep [cat $ catMaybes old, cat $ catMaybes new]

dep :: Doc AnsiStyle -> VersionedList -> VersionedList -> Doc AnsiStyle
dep s va vb =
  annMagneta s <> colon <> line
    <> if noDiff diff
      then joinToString da
      else
        joinToString da
          <> splitLine
          <> joinToString db
  where
    a = joinVersionWithName <$> va
    b = joinVersionWithName <$> vb
    da = mconcat $ ppDiffColored <$> filterFirstAndBothDiff diff
    db = mconcat $ ppDiffColored <$> filterSecondAndBothDiff diff
    diff = getGroupedDiff a b
    joinToString [] = indent 2 "[]"
    joinToString xs = indent 2 $ vsep xs
    joinVersionWithName (n, range) = unPackageName n <> "  " <> prettyShow range

flags :: PackageName -> [PkgFlag] -> [PkgFlag] -> Doc AnsiStyle
flags name a b =
  annMagneta "Flags" <> colon <> line
    <> if noDiff diff
      then joinToString a
      else
        joinToString a
          <> splitLine
          <> joinToString b
  where
    diff = getGroupedDiff a b
    joinToString [] = indent 2 "[]"
    joinToString xs = indent 2 $ prettyFlags [(name, xs)]
