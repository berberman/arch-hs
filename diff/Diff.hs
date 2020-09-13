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
import qualified Distribution.Types.PackageId as I
import Distribution.Types.PackageName
import Distribution.Utils.ShortText (fromShortText)
import Distribution.Version (Version, versionNumbers)
import Hackage
import Local (ghcLibList, ignoreList)
import Options.Applicative
import Polysemy
import Types

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
          <> help "Path to 00-index.tar"
          <> showDefault
          <> value "~/.cabal/packages/YOUR_HACKAGE_MIRROR/00-index.tar"
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
        dep "Depends: " ba bb,
        dep "MakeDepends: " ma mb
      ]

directDependencies ::
  Members [HackageEnv, FlagAssignmentEnv, WithMyErr] r =>
  GenericPackageDescription ->
  Sem r ([String], [String])
directDependencies cabal = do
  (libDeps, libToolsDeps) <- collectLibDeps cabal
  (exeDeps, exeToolsDeps) <- collectExeDeps cabal []
  (testDeps, testToolsDeps) <- collectTestDeps cabal []
  let flatten = fmap unPackageName . mconcat . fmap snd
      l = fmap unPackageName libDeps
      lt = fmap unPackageName libToolsDeps
      e = flatten exeDeps
      et = flatten exeToolsDeps
      t = flatten testDeps
      tt = flatten testToolsDeps
      name = unPackageName . I.pkgName . package . packageDescription $ cabal
      notInGHCLib = (`notElem` ghcLibList) . mkPackageName
      notInIgnore = (`notElem` ignoreList) . mkPackageName
      notMyself = (/= name)
      distinct =
        filter notInIgnore
          . filter notInGHCLib
          . filter notMyself
          . nub
      depends = distinct $ l ++ e
      makedepends = distinct $ lt ++ et ++ t ++ tt
  return (depends, makedepends)

diffTerm :: String -> (a -> String) -> a -> a -> String
diffTerm s f a b = let (ra, rb) = (f a, f b) in s <> (if ra == rb then ra else (C.formatWith [C.red] $ ra <> " ⇒ " <> rb))

desc :: PackageDescription -> PackageDescription -> String
desc = diffTerm "Synopsis: " $ fromShortText . synopsis

ver :: PackageDescription -> PackageDescription -> String
ver = diffTerm "Version: " $ intercalate "." . fmap show . versionNumbers . I.pkgVersion . package

dep :: String -> [String] -> [String] -> String
dep s a b =
  s <> case b \\ a of
    [] -> joinToString a
    xs -> C.formatWith [C.red] $ joinToString a <> " ⇒ " <> joinToString b <> "Diff: " <> joinToString xs
  where
    joinToString [] = "[]"
    joinToString xs = intercalate ", " xs