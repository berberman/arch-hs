{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Main (main) where

import qualified Algebra.Graph.AdjacencyMap.Algorithm as G
import qualified Algebra.Graph.Labelled.AdjacencyMap as G
import qualified Colourista as C
import Community
import Conduit
import Control.Monad (filterM, when)
import Core
import Data.List (groupBy, isInfixOf)
import qualified Data.Map.Strict as Map
import qualified Data.Set as S
import qualified Data.Text as T
import Distribution.Hackage.DB (HackageDB)
import Distribution.PackageDescription (Flag, FlagAssignment, flagDefault, flagDescription, flagManual, flagName, insertFlagAssignment, mkFlagAssignment, mkFlagName, unFlagAssignment, unFlagName)
import Distribution.Types.PackageName
import Hackage
import Lens.Micro
import Local
import Options.Applicative
import PkgBuild
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Types

data Options = Options
  { optHackagePath :: FilePath,
    optOutputDir :: FilePath,
    optFlags :: [(String, String, Bool)],
    optTarget :: String
  }
  deriving stock (Show)

options :: Parser Options
options =
  Options
    <$> strOption
      ( long "hackage"
          <> metavar "PATH"
          <> short 'h'
          <> help "Path to 00-index.tar"
          <> showDefault
          <> value "~/.cabal/packages/YOUR_HACKAGE_MIRROR/00-index.tar"
      )
    <*> strOption
      ( long "output"
          <> metavar "PATH"
          <> short 'o'
          <> help "Output path to generated PKGBUILD files (empty means dry run)"
          <> showDefault
          <> value ""
      )
    <*> option
      auto
      ( long "flags"
          <> metavar "[(\"package_name\",\"flag_name\",True|False),...]"
          <> short 'f'
          <> help "Flag assignments for packages - e.g. [(\"inline-c\",\"gsl-example\",True)], notice that quotation marks can't be ignored"
          <> showDefault
          <> value []
      )
    <*> strArgument (metavar "TARGET")

h :: Members '[Embed IO, CommunityEnv, HackageEnv, FlagAssignmentEnv, WithMyErr] r => String -> FilePath -> Sem r ()
h name path = do
  let target = mkPackageName name
  deps <- getDependencies S.empty 0 target
  exist <- isInCommunity target
  when exist $ throw $ TargetExist target

  let grouped = groupDeps deps
      allNames = fmap head . groupBy (==) $ (grouped ^.. each . pkgName ++ grouped ^.. each . pkgDeps . each . depName)
  providedList <- (++ ghcLibList) <$> filterM isInCommunity allNames
  let fillProvidedPkgs = mapC (\x -> if (x ^. pkgName) `elem` providedList then ProvidedPackage (x ^. pkgName) ByCommunity else x)
      fillProvidedDeps = mapC (pkgDeps %~ each %~ (\y -> if y ^. depName `elem` providedList then y & depProvider .~ (Just ByCommunity) else y))
      cooked = runConduitPure $ yieldMany grouped .| fillProvidedPkgs .| fillProvidedDeps .| sinkList
      toBePacked = cooked ^.. each . filtered (\case ProvidedPackage {..} -> False; _ -> True)

  liftIO $ C.infoMessage "Solved target:"
  liftIO $ putStrLn . prettySolvedPkgs $ cooked

  let flattened = filter (`elem` (toBePacked ^.. each . pkgName)) $ G.reachable target $ G.skeleton deps
  liftIO $ C.infoMessage "Recommended package order (from topological sort):"
  liftIO $ putStrLn . prettyDeps $ flattened
  flags <- filter (\(_, l) -> length l /= 0) <$> mapM (\n -> (n,) <$> getPackageFlag n) flattened

  liftIO $
    when (length flags /= 0) $ do
      C.infoMessage "Detected flags from targets (their values will keep default unless you specify):"
      putStrLn . prettyFlags $ flags

  let dry = path == ""
  liftIO $ when dry $ C.warningMessage "You didn't pass -o, PKGBUILD files will not be generated."
  when (not dry) $
    mapM_
      ( \solved -> do
          txt <- applyTemplate <$> cabalToPkgBuild solved
          let pName = solved ^. pkgName & unPackageName
              dir = path </> pName
              fileName = dir </> "PKGBUILD"
          liftIO $ createDirectoryIfMissing True dir
          liftIO $ writeFile fileName txt
          liftIO $ C.infoMessage $ "Write file: " <> T.pack fileName
      )
      toBePacked

runH ::
  HackageDB ->
  CommunityDB ->
  Map.Map PackageName FlagAssignment ->
  Sem '[CommunityEnv, HackageEnv, FlagAssignmentEnv, WithMyErr, Embed IO, Final IO] a ->
  IO (Either MyException a)
runH hackage community flags =
  runFinal
    . embedToFinal
    . errorToIOFinal
    . runReader flags
    . runReader hackage
    . runReader community

main :: IO ()
main = do
  Options {..} <-
    execParser $
      info
        (options <**> helper)
        ( fullDesc
            <> progDesc "Try to reach the TARGET QAQ."
            <> header "arch-hs - a program converting packges from hackage to arch PKGBUILD."
        )
  let isDefault = isInfixOf "YOUR_HACKAGE_MIRROR" $ optHackagePath
  when isDefault $ C.skipMessage "You didn't pass -h, use hackage index file from default places."

  let isFlagEmpty = optFlags == []
      cookedFlags = cookFlag optFlags
  when isFlagEmpty $ C.skipMessage "You didn't pass -f, different flag values make difference in dependency solving."
  when (not isFlagEmpty) $ do
    C.infoMessage "You assigned flags:"
    putStrLn . prettyFlagAssignments $ cookedFlags

  hackage <- if isDefault then defaultHackageDB else loadHackageDB optHackagePath
  C.infoMessage "Loading hackage..."
  community <- defaultCommunity
  C.infoMessage "Loading community.db..."
  C.infoMessage "Start running..."
  runH hackage community cookedFlags (h optTarget optOutputDir) >>= \case
    Left x -> C.errorMessage $ "Error: " <> (T.pack . show $ x)
    _ -> C.successMessage "Success!"

cookFlag :: [(String, String, Bool)] -> Map.Map PackageName FlagAssignment
cookFlag [] = Map.empty
cookFlag list = z
  where
    x = groupBy (\(a, _, _) (b, _, _) -> a == b) list
    y = fmap (\l -> (mkPackageName . fst' . head $l, foldr (\(_, f, v) acc -> insertFlagAssignment (mkFlagName f) v acc) (mkFlagAssignment []) l)) x
    z = Map.fromList y
    fst' (a, _, _) = a

groupDeps :: G.AdjacencyMap (S.Set DependencyType) PackageName -> [SolvedPackage]
groupDeps =
  fmap (\(name, deps) -> SolvedPackage name $ fmap (uncurry . flip $ SolvedDependency Nothing) deps)
    . fmap ((\(a, b, c) -> (head b, zip a c)) . unzip3)
    . groupBy (\x y -> uncurry (==) ((x, y) & both %~ (^. _2)))
    . fmap (_1 %~ S.toList)
    . S.toList
    . G.edgeSet

prettyFlags :: [(PackageName, [Flag])] -> String
prettyFlags = mconcat . fmap (\(name, flags) -> (C.formatWith [C.magenta] $ unPackageName name ++ "\n") ++ mconcat (fmap (C.formatWith [C.indent 4] . prettyFlag) flags))

prettyFlag :: Flag -> String
prettyFlag f = "⚐ " ++ C.formatWith [C.yellow] name ++ ":\n" ++ mconcat (fmap (C.formatWith [C.indent 6] . (++ "\n")) $ ["description: " ++ desc, "default: " ++ def, "isManual: " ++ manual])
  where
    name = unFlagName . flagName $ f
    desc = flagDescription f
    def = show $ flagDefault f
    manual = show $ flagManual f

prettyFlagAssignments :: Map.Map PackageName FlagAssignment -> String
prettyFlagAssignments m = mconcat $ fmap (fmap (\(n, a) -> C.formatWith [C.magenta] (unPackageName n) ++ "\n" ++ C.formatWith [C.indent 4] (prettyFlagAssignment a))) Map.toList m

prettyFlagAssignment :: FlagAssignment -> String
prettyFlagAssignment m = mconcat $fmap (\(n, v) -> "⚐ " ++ C.formatWith [C.yellow] (unFlagName n) ++ " : " ++ C.formatWith [C.cyan] (show v) ++ "\n") $ unFlagAssignment m

prettySolvedPkgs :: [SolvedPackage] -> String
prettySolvedPkgs =
  mconcat
    . fmap
      ( \case
          SolvedPackage {..} ->
            C.formatWith [C.bold, C.yellow] ("⊢ " ++ unPackageName _pkgName ++ "\n")
              ++ mconcat
                ( fmap
                    ( \SolvedDependency {..} -> case _depProvider of
                        (Just x) -> (C.formatWith [C.green] $ "    ⊢ " ++ unPackageName _depName ++ " " ++ show _depType ++ " ✔ ") ++ (C.formatWith [C.cyan] $ "[" ++ show x ++ "]\n")
                        _ -> C.formatWith [C.bold, C.yellow] $ "    ⊢ " ++ unPackageName _depName ++ " " ++ show _depType ++ "\n"
                    )
                    _pkgDeps
                )
          ProvidedPackage {..} -> (C.formatWith [C.green] $ "⊢ " ++ unPackageName _pkgName ++ " ✔ ") ++ (C.formatWith [C.cyan] $ "[" ++ show _pkgProvider ++ "]\n")
      )

prettyDeps :: [PackageName] -> String
prettyDeps list =
  mconcat $
    fmap (\(i, n) -> show @Int i ++ ". " ++ unPackageName n ++ "\n") $
      zip [1 ..] $
        reverse list
