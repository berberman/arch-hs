{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Diff
  ( CabalSource (..),
    diffCabal,
    inRange,
  )
where

import Data.Algorithm.Diff
import Control.Monad (unless)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Conduit.Tar as Tar
import Data.IORef (modifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Conduit
import Distribution.ArchHs.Core
import Distribution.ArchHs.Exception
import Distribution.ArchHs.ExtraDB (versionInExtra)
import Distribution.ArchHs.Internal.Prelude
import Distribution.ArchHs.PP
import Distribution.ArchHs.Types
import Distribution.ArchHs.Utils
import Distribution.PackageDescription.Parsec (parseGenericPackageDescriptionMaybe)
import Distribution.Utils.ShortText (fromShortText)
import Network.HTTP.Client

type VersionedList = [(PackageName, VersionRange)]

data CabalSource
  = Online Manager
  | Offline FilePath

-----------------------------------------------------------------------------

getCabalFromHackage :: Members [Embed IO, WithMyErr] r => Manager -> PackageName -> Version -> Sem r GenericPackageDescription
getCabalFromHackage manager name version = do
  let urlPath = unPackageName name <> "-" <> prettyShow version
      s = "https://hackage.haskell.org/package/" <> urlPath <> "/revision/0.cabal"
  req <- interceptHttpException $ parseRequest s
  printInfo $ "Downloading cabal file from" <+> pretty s
  response <- interceptHttpException $ httpLbs req manager
  case parseGenericPackageDescriptionMaybe . LBS.toStrict $ responseBody response of
    Just x -> return x
    _ -> error "Failed to parse .cabal file"

getCabalsFromIndex :: Members [Embed IO, WithMyErr] r => FilePath -> PackageName -> [Version] -> Sem r (Map.Map Version GenericPackageDescription)
getCabalsFromIndex hackagePath name versions = do
  let pkg = unPackageName name
      cabalPaths = Map.fromList [(pkg </> prettyShow version </> (pkg <> ".cabal"), version) | version <- nub versions]
  mapM_ (\cabalPath -> printInfo $ "Reading revision 0 cabal file from" <+> pretty hackagePath <> colon <+> pretty cabalPath) $ Map.keys cabalPaths
  cabalFiles <- embed $ findCabalFilesInIndex hackagePath cabalPaths
  Map.fromList <$> traverse (parseCabal cabalFiles) (nub versions)
  where
    parseCabal cabalFiles version =
      case parseGenericPackageDescriptionMaybe =<< Map.lookup version cabalFiles of
        Just cabal -> return (version, cabal)
        Nothing -> throw $ VersionNotFound name version

findCabalFilesInIndex :: FilePath -> Map.Map FilePath Version -> IO (Map.Map Version BS.ByteString)
findCabalFilesInIndex hackagePath cabalPaths = do
  foundRef <- newIORef Map.empty
  Map.fromList
    <$> runConduitRes
      ( sourceFileBS hackagePath
          .| Tar.untarChunks
          .| Tar.withEntries (action foundRef)
          .| takeC (Map.size cabalPaths)
          .| sinkList
      )
  where
    action foundRef header
      | Tar.FTNormal <- Tar.headerFileType header,
        Just version <- Map.lookup (Tar.headerFilePath header) cabalPaths = do
        found <- liftIO $ readIORef foundRef
        unless (Map.member version found) $ do
          cabalFile <- mconcat <$> sinkList
          liftIO $ modifyIORef' foundRef (Map.insert version ())
          yield (version, cabalFile)
      | otherwise = return ()

getCabalsFromSource :: Members [Embed IO, WithMyErr, Reader CabalSource] r => PackageName -> Version -> Version -> Sem r (GenericPackageDescription, GenericPackageDescription)
getCabalsFromSource name a b =
  ask @CabalSource >>= \case
    Online manager -> (,) <$> getCabalFromHackage manager name a <*> getCabalFromHackage manager name b
    Offline hackagePath -> do
      cabals <- getCabalsFromIndex hackagePath name [a, b]
      (,) <$> lookupCabal cabals a <*> lookupCabal cabals b
  where
    lookupCabal cabals version =
      case Map.lookup version cabals of
        Just cabal -> return cabal
        Nothing -> throw $ VersionNotFound name version

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

diffCabal :: Members [KnownGHCVersion, ExtraEnv, FlagAssignmentsEnv, WithMyErr, Trace, DependencyRecord, Reader CabalSource, Embed IO] r => PackageName -> Version -> Version -> Sem r ()
diffCabal name a b = do
  (ga, gb) <- getCabalsFromSource name a b
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
      Right rawVersion ->
        case simpleParsec rawVersion of
          Just version -> return . Right $ (name, hRange, version, withinRange version hRange)
          Nothing -> throw $ VersionNoParse rawVersion
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
