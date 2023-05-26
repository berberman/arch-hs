{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Diff
  ( diffCabal,
  )
where

import Data.Algorithm.Diff
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (catMaybes, fromJust)
import Distribution.ArchHs.ExtraDB (versionInExtra)
import Distribution.ArchHs.Core
import Distribution.ArchHs.Exception
import Distribution.ArchHs.Internal.Prelude
import Distribution.ArchHs.PP
import Distribution.ArchHs.Types
import Distribution.ArchHs.Utils
import Distribution.PackageDescription.Parsec (parseGenericPackageDescriptionMaybe)
import Distribution.Utils.ShortText (fromShortText)
import Network.HTTP.Client

type VersionedList = [(PackageName, VersionRange)]

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
  Members [KnownGHCVersion, FlagAssignmentsEnv, Trace, DependencyRecord] r =>
  GenericPackageDescription ->
  Sem r (VersionedList, VersionedList)
directDependencies cabal = do
  (libDeps, libToolsDeps, _) <- collectLibDeps id cabal
  (subLibDeps, subLibToolsDeps, _) <- collectSubLibDeps id cabal []
  (exeDeps, exeToolsDeps, _) <- collectExeDeps id cabal []
  (testDeps, testToolsDeps, _) <- collectTestDeps id cabal []
  setupDeps <- collectSetupDeps id cabal
  let flatten = mconcat . fmap snd
      l = libDeps
      lt = libToolsDeps
      sl = flatten subLibDeps
      slt = flatten subLibToolsDeps
      e = flatten exeDeps
      et = flatten exeToolsDeps
      t = flatten testDeps
      tt = flatten testToolsDeps
      mySubLibs = fmap (unqualComponentNameToPackageName . fst) subLibDeps
      notMyselfOrSubLib = (&&) <$> (/= getPkgName' cabal) <*> (`notElem` mySubLibs)
      distinct = filter (notMyselfOrSubLib . fst) . nub
      depends = distinct $ l <> sl <> e
      makedepends = distinct (lt <> slt <> et <> t <> tt <> setupDeps) \\ depends
      sort' = sortBy (\x y -> uncurry compare $ getTwo _1 x y)
  return (sort' depends, sort' makedepends)

-----------------------------------------------------------------------------

diffCabal :: Members [KnownGHCVersion, ExtraEnv, FlagAssignmentsEnv, WithMyErr, Trace, DependencyRecord, Reader Manager, Embed IO] r => PackageName -> Version -> Version -> Sem r ()
diffCabal name a b = do
  ga <- getCabalFromHackage name a
  gb <- getCabalFromHackage name b
  let pa = packageDescription ga
      pb = packageDescription gb
      fa = genPackageFlags ga
      fb = genPackageFlags gb
  (ba, ma) <- directDependencies ga
  (bb, mb) <- directDependencies gb
  queryb <- lookupDiffExtra ba bb
  querym <- lookupDiffExtra ma mb
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

inRange :: Members [ExtraEnv, WithMyErr] r => (PackageName, VersionRange) -> Sem r (Either (PackageName, VersionRange) (PackageName, VersionRange, Version, Bool))
inRange (name, hRange) =
  try @MyException (versionInExtra name)
    >>= \case
      Right y -> let version = fromJust . simpleParsec $ y in return . Right $ (name, hRange, version, withinRange version hRange)
      Left _ -> return . Left $ (name, hRange)

lookupDiffExtra :: Members [ExtraEnv, WithMyErr] r => VersionedList -> VersionedList -> Sem r (Doc AnsiStyle)
lookupDiffExtra va vb = do
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
            <+> ppExtra
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
            <+> ppExtra
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
