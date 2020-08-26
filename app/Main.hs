{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import qualified Algebra.Graph.AdjacencyMap.Algorithm as G
import qualified Algebra.Graph.Labelled.AdjacencyMap as G
import ArchHs
import Conduit
import Control.DeepSeq (NFData)
import Control.Exception
import Control.Monad (filterM, when)
import qualified Data.Conduit.Tar as Tar
import qualified Data.Conduit.Zlib as Zlib
import Data.List (groupBy)
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import qualified Data.Set as S
import qualified Debug.Trace as D
import qualified Distribution.Hackage.DB as DB
import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration
import Distribution.Types.Dependency
import Distribution.Types.ExeDependency
import Distribution.Types.PackageName
import GHC.Generics (Generic)
import Lens.Micro
import Lens.Micro.TH (makeLenses)

data MyException
  = PkgNotFound PackageName
  | VersionError
  deriving stock (Show, Eq)
  deriving anyclass (Exception)

data DependencyType
  = Exe
  | Lib
  | TestAndBench
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

type CollectedDependencies = [PackageName]

data SolvedDependency = SolvedDependency {_provided :: Bool, _depName :: PackageName, _depType :: [DependencyType]}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

data SolvedPackage = ProvidedPackage {_pkgName :: PackageName} | SolvedPackage {_pkgName :: PackageName, _pkgDeps :: [SolvedDependency]}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

makeLenses ''SolvedDependency
makeLenses ''SolvedPackage

h = do
  hackage <- defaultHackageDB
  community <- fmap S.fromList $ runConduitRes  $ loadCommunity defaultCommunityPath .| sinkList
  let target = mkPackageName "summoner"
      deps = getDependencies hackage S.empty 0 False target
  putStrLn $ "Build target: " ++ show target
  putStrLn . prettyDeps $ G.reachable target $ G.skeleton deps
  let d = groupDeps $ deps
      v = S.toList $ S.fromList (d ^.. each . pkgName ++ d ^.. each . pkgDeps . each . depName)
  providedList <- filterM (defaultIsInCommunity) v >>= return . (++ ghcLibList)
  let q = fmap (\x -> if (x ^. pkgName) `elem` providedList then ProvidedPackage (x ^. pkgName) else x) d
      
      r = q <&> pkgDeps %~ each %~ (\y -> if y ^. depName `elem` providedList then y & provided .~ True else y)
  putStrLn . prettySolvedPkgs $ r

main :: IO ()
main = do
  defaultIsInCommunity (mkPackageName "QuickCheck") >>= print
  defaultIsInCommunity (mkPackageName "QuickCheck") >>= print


groupDeps :: G.AdjacencyMap (S.Set DependencyType) PackageName -> [SolvedPackage]
groupDeps =
  fmap (\(name, deps) -> SolvedPackage name $ fmap (uncurry . flip $ SolvedDependency False) deps)
    . fmap ((\(a, b, c) -> (head b, zip a c)) . unzip3)
    . groupBy (\x y -> uncurry (==) ((x, y) & both %~ (^. _2)))
    . fmap (_1 %~ S.toList)
    . S.toList
    . G.edgeSet

prettySolvedPkgs :: [SolvedPackage] -> String
prettySolvedPkgs =
  mconcat
    . fmap
      ( \case
          SolvedPackage {..} -> "⊢ " ++ show _pkgName ++ "\n" ++ mconcat (fmap (\SolvedDependency {..} -> "    ⊢ " ++ show _depName ++ " " ++ show _depType ++ (if _provided then " ✔" else "") ++ "\n") _pkgDeps)
          ProvidedPackage {..} -> "⊢ " ++ show _pkgName ++ " ✔" ++ "\n"
      )

prettyDeps :: [PackageName] -> String
prettyDeps list =
  mconcat $
    fmap (\(i, n) -> show i ++ ". " ++ show n ++ "\n") $
      zip [1 ..] $
        reverse list

ignoreList :: CollectedDependencies
ignoreList =
  fmap
    mkPackageName
    [ "unbuildable",
      "invalid-cabal-flag-settings",
      "par-classes",
      "fail",
      "integer-simple",
      "bytestring-builder",
      -- "nats",
      "integer",
      "unsupported-ghc-version",
      "base",
      "ghc-prim",
      "ghcjs-prim",
      "ghc-bignum",
      "hans"
    ]

ghcLibList :: CollectedDependencies
ghcLibList =
  fmap
    mkPackageName
    [ "array",
      "base",
      "binary",
      "bytestring",
      "cabal",
      "containers",
      "deepseq",
      "directory",
      "exceptions",
      "filepath",
      "ghc-boot",
      "ghc-boot-th",
      "ghc-compact",
      "ghc-heap",
      "ghci",
      "ghc-prim",
      "haskeline",
      "hpc",
      "integer-gmp",
      "libiserv",
      "mtl",
      "parsec",
      "pretty",
      "process",
      "stm",
      "template-haskell",
      "terminfo",
      "text",
      "time",
      "transformers",
      "unix",
      "xhtml"
    ]

const' _ b = b

getDependencies ::
  DB.HackageDB ->
  S.Set PackageName ->
  Int ->
  Bool ->
  PackageName ->
  G.AdjacencyMap (S.Set DependencyType) PackageName
getDependencies db resolved n buildExe name =
  D.traceShow
    ( "At "
        ++ show n
        ++ ": Get dep for pkg ["
        ++ show name
        ++ "], with direct deps: "
        ++ show (ignored $ libDeps <> testDeps <> exeDeps)
        ++ ""
    )
    $ if name `elem` ignoreList
      then G.empty
      else
        ( if buildExe
            then (current Exe) `G.overlay` G.overlays (next Exe)
            else G.empty
        )
          `G.overlay` (current Lib)
          `G.overlay` (current TestAndBench)
          `G.overlay` G.overlays (next Lib)
  where
    cabal = getLatestCabal db name
    libDeps = collectLibDeps cabal
    exeDeps = collectExeDeps cabal
    testDeps = collectBenchmarkAndTestDeps cabal
    cats Lib = libDeps
    cats Exe = exeDeps
    cats TestAndBench = testDeps
    current x =
      G.edges
        ( zip3 (repeat $ S.singleton x) (repeat name) $
            filterNot (`elem` ignoreList) $
              cats x
        )
    next x =
      fmap
        (getDependencies db (S.insert name (resolved)) (n + 1) buildExe)
        $ ignored (cats x)
    ignored =
      filter
        (\x -> not $ x `elem` ignoreList || x == name || x `elem` resolved)
    filterNot p = filter (not . p)

getLatestCabal :: DB.HackageDB -> PackageName -> PackageDescription
getLatestCabal db name = case Map.lookup name db of
  (Just m) -> case Map.lookupMax m of
    Just (_, vdata) -> flattenPackageDescription $ vdata & DB.cabalFile
    Nothing -> throw VersionError
  Nothing -> throw $ PkgNotFound name

collectLibDeps :: PackageDescription -> CollectedDependencies
collectLibDeps cabal = case library cabal of
  Just lib -> libDeps lib <> toolDeps lib
  Nothing -> []
  where
    info lib = libBuildInfo lib
    libDeps lib = fmap depPkgName $ targetBuildDepends $ info lib
    toolDeps lib = fmap unExe $ buildToolDepends $ info lib

collectExeDeps :: PackageDescription -> CollectedDependencies
collectExeDeps cabal = mconcat $ exeDeps <> toolDeps
  where
    info = fmap buildInfo $ executables cabal
    exeDeps = fmap (fmap depPkgName . targetBuildDepends) info
    toolDeps = fmap (fmap unExe . buildToolDepends) info

collectBenchmarkAndTestDeps :: PackageDescription -> CollectedDependencies
collectBenchmarkAndTestDeps cabal =
  mconcat . mconcat $
    fmap (\x -> exeDeps x ++ toolDeps x) [bInfo, tInfo]
  where
    bInfo = fmap benchmarkBuildInfo $ benchmarks cabal
    tInfo = fmap testBuildInfo $ testSuites cabal
    exeDeps = fmap $ fmap depPkgName . targetBuildDepends
    toolDeps = fmap $ fmap unExe . buildToolDepends

unExe :: ExeDependency -> PackageName
unExe (ExeDependency name _ _) = name

defaultHackagePath :: FilePath
defaultHackagePath = "/home/berberman/.stack/pantry/hackage/00-index.tar"

defaultCommunityPath :: FilePath
defaultCommunityPath = "/var/lib/pacman/sync/community.db"

defaultHackageDB :: IO DB.HackageDB
defaultHackageDB = loadHackageDB defaultHackagePath

defaultIsInCommunity :: PackageName -> IO Bool
defaultIsInCommunity name = runConduitRes $ loadCommunity defaultCommunityPath .| isInCommunity name

loadHackageDB :: FilePath -> IO DB.HackageDB
loadHackageDB path = do
  putStrLn $ "Hackage index: " ++ path
  DB.readTarball Nothing path

loadCommunity ::
  (MonadResource m, PrimMonad m, MonadThrow m) =>
  FilePath ->
  ConduitM i FilePath m ()
loadCommunity path =
  sourceFileBS path .| Zlib.ungzip .| Tar.untarChunks .| Tar.withEntries action
  where
    action h =
      when (Tar.headerFileType h == Tar.FTNormal) $
        yield $ Tar.headerFilePath h

isInCommunity :: (Monad m) => PackageName -> ConduitT FilePath o m Bool
isInCommunity name = anyC ((==) (unPackageName name) . choice . (splitOn "-"))
  where
    choice list = case length list of
      3 -> list !! 0
      s ->
        if list !! 0 == "haskell"
          then mconcat . fst . splitAt (s - 3) . tail $ list
          else mconcat . fst . splitAt (s - 2) $ list
