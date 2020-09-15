{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Distribution.ArchHs.Hackage (parseCabalFile)
import Distribution.PackageDescription.PrettyPrint (showGenericPackageDescription)
import Distribution.Types.CondTree
  ( CondTree,
    mapTreeConstrs,
    mapTreeData,
  )
import Distribution.Types.Dependency (Dependency (..))
import Distribution.Types.ExeDependency (ExeDependency (..))
import Distribution.Types.Lens
  ( BuildInfo,
    ConfVar,
    GenericPackageDescription,
    HasBuildInfo (buildInfo, buildToolDepends, targetBuildDepends),
    condBenchmarks,
    condExecutables,
    condLibrary,
    condTestSuites,
  )
import Distribution.Types.VersionRange (anyVersion)
import Lens.Micro
import Options.Applicative
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (hClose, hFlush, hPutStr, openTempFile)
import System.Process (readCreateProcessWithExitCode, shell)

data Options = Options
  { optPath :: FilePath
  }

cmdOptions :: Parser Options
cmdOptions = Options <$> strArgument (metavar "TARGET")

runArgsParser :: IO Options
runArgsParser =
  execParser $
    info
      (cmdOptions <**> helper)
      ( fullDesc
          <> progDesc "Try to reach the TARGET QAQ."
          <> header "arch-hs-uusi - a program removing all version constraints of a .cabal file, and creating a diff between it and the origin one."
      )

-----------------------------------------------------------------------------

main :: IO ()
main = do
  Options {..} <- runArgsParser
  uusiCabal optPath >>= putStrLn

genPatch :: FilePath -> FilePath -> IO String
genPatch a b = (^. _2) <$> readCreateProcessWithExitCode (shell $ "diff -u " <> a <> " " <> b) ""

uusiCabal :: FilePath -> IO String
uusiCabal origin = do
  cabal <- parseCabalFile origin
  temp <- getTemporaryDirectory
  (path, handle) <- openTempFile temp "arch-hs-uusi"
  let uusied = showGenericPackageDescription $ uusiGenericPackageDescription cabal
  hPutStr handle uusied
  hFlush handle
  hClose handle
  result <- genPatch origin path
  removeFile path
  return result

-----------------------------------------------------------------------------

type Uusi a = a -> a

uusiDependency :: Uusi Dependency
uusiDependency (Dependency name _ lib) = Dependency name anyVersion lib

uusiExeDependency :: Uusi ExeDependency
uusiExeDependency (ExeDependency name component _) = ExeDependency name component anyVersion

uusiBuildInfo :: Uusi BuildInfo
uusiBuildInfo i =
  i
    & (targetBuildDepends %~ fmap uusiDependency)
    & (buildToolDepends %~ fmap uusiExeDependency)

uusiCondTree :: (HasBuildInfo a) => Uusi (CondTree ConfVar [Dependency] a)
uusiCondTree cond =
  mapTreeData (\a -> a & buildInfo %~ uusiBuildInfo)
    . mapTreeConstrs (fmap uusiDependency)
    $ cond

uusiGenericPackageDescription :: Uusi GenericPackageDescription
uusiGenericPackageDescription cabal =
  cabal
    & (condExecutables %~ uusiTrees)
    & (condTestSuites %~ uusiTrees)
    & (condBenchmarks %~ uusiTrees)
    & (condLibrary . mapped %~ uusiCondTree)
  where
    uusiTrees trees = trees & mapped . _2 %~ uusiCondTree