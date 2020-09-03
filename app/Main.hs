{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Main (main) where

import qualified Algebra.Graph.AdjacencyMap.Algorithm as G
import qualified Algebra.Graph.Labelled.AdjacencyMap as G
import Aur
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
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import Types

data Options = Options
  { optHackagePath :: FilePath,
    optOutputDir :: FilePath,
    optFlags :: [(String, String, Bool)],
    optAur :: Bool,
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
          <> value ""
      )
    <*> option
      myFlagReader
      ( long "flags"
          <> metavar "package_name:flag_name:true|false,..."
          <> short 'f'
          <> help "Flag assignments for packages - e.g. inline-c:gsl-example:true (separated by ',')"
          <> value []
      )
    <*> switch
      ( long "aur"
          <> short 'a'
          <> help "Enable AUR searching."
      )
    <*> strArgument (metavar "TARGET")

myFlagReader :: ReadM [(String, String, Bool)]
myFlagReader =
  eitherReader
    ( \s -> case M.parse myFlagParser "" s of
        Right x -> Right x
        Left err -> Left $ M.errorBundlePretty err
    )

myFlagParser :: M.Parsec Void String [(String, String, Bool)]
myFlagParser =
  ( do
      pkg <- M.manyTill M.anySingle $ M.single ':'
      flg <- M.manyTill M.anySingle $ M.single ':'
      b <- bool
      return (pkg, flg, b)
  )
    `M.sepBy` ","
  where
    bool = do
      s <- M.string "true" <|> M.string "false"
      case s of
        "true" -> return True
        "false" -> return False
        _ -> fail $ "unknown bool: " ++ s

-----------------------------------------------------------------------------

h :: Members '[Embed IO, CommunityEnv, HackageEnv, FlagAssignmentEnv, Aur, WithMyErr] r => String -> FilePath -> Bool -> Sem r ()
h name path aurSupport = do
  let target = mkPackageName name
  deps <- getDependencies S.empty 0 target
  inCommunity <- isInCommunity target
  when inCommunity $ throw $ TargetExist target ByCommunity

  if aurSupport
    then do
      inAur <- isInAur target
      when inAur $ throw $ TargetExist target ByAur
    else return ()

  let grouped = groupDeps deps
      namesFromSolved x = x ^.. each . pkgName ++ x ^.. each . pkgDeps . each . depName
      allNames = distinct $ namesFromSolved grouped
      distinct = fmap head . groupBy (==)
  communityProvideList <- (++ ghcLibList) <$> filterM isInCommunity allNames
  let fillProvidedPkgs provideList provider = mapC (\x -> if (x ^. pkgName) `elem` provideList then ProvidedPackage (x ^. pkgName) provider else x)
      fillProvidedDeps provideList provider = mapC (pkgDeps %~ each %~ (\y -> if y ^. depName `elem` provideList then y & depProvider .~ (Just provider) else y))
      cooked =
        runConduitPure $
          yieldMany grouped
            .| fillProvidedPkgs communityProvideList ByCommunity
            .| fillProvidedDeps communityProvideList ByCommunity
            .| sinkList
      toBePacked = cooked ^.. each . filtered (\case ProvidedPackage {..} -> False; _ -> True)
  (cookedAgain, toBePackedAgain) <- do
    embed $ C.infoMessage "Start searching AUR..."
    aurProvideList <- if aurSupport then filterM (\n -> do embed $ C.infoMessage ("Searching " <> (T.pack $ unPackageName n)); isInAur n) $ toBePacked ^.. each . pkgName else return []
    let cookedAgain =
          if aurSupport
            then
              runConduitPure $
                yieldMany cooked
                  .| fillProvidedPkgs aurProvideList ByAur
                  .| fillProvidedDeps aurProvideList ByAur
                  .| sinkList
            else cooked
        toBePackedAgain =
          if aurSupport
            then cookedAgain ^.. each . filtered (\case ProvidedPackage {..} -> False; _ -> True)
            else toBePacked
    return (cookedAgain, toBePackedAgain)

  embed $ C.infoMessage "Solved target:"
  embed $ putStrLn . prettySolvedPkgs $ cookedAgain

  let flattened = filter (`elem` (toBePackedAgain ^.. each . pkgName)) $ G.reachable target $ G.skeleton deps
  embed $ C.infoMessage "Recommended package order (from topological sort):"
  embed $ putStrLn . prettyDeps $ flattened
  flags <- filter (\(_, l) -> length l /= 0) <$> mapM (\n -> (n,) <$> getPackageFlag n) flattened

  embed $
    when (length flags /= 0) $ do
      C.infoMessage "Detected flags from targets (their values will keep default unless you specify):"
      putStrLn . prettyFlags $ flags

  let dry = path == ""
  embed $ when dry $ C.warningMessage "You didn't pass -o, PKGBUILD files will not be generated."
  when (not dry) $
    mapM_
      ( \solved -> do
          txt <- applyTemplate <$> cabalToPkgBuild solved
          let pName = solved ^. pkgName & unPackageName
              dir = path </> pName
              fileName = dir </> "PKGBUILD"
          embed $ createDirectoryIfMissing True dir
          embed $ writeFile fileName txt
          embed $ C.infoMessage $ "Write file: " <> T.pack fileName
      )
      toBePackedAgain

runH ::
  HackageDB ->
  CommunityDB ->
  Map.Map PackageName FlagAssignment ->
  Sem '[CommunityEnv, HackageEnv, FlagAssignmentEnv, Aur, WithMyErr, Embed IO, Final IO] a ->
  IO (Either MyException a)
runH hackage community flags =
  runFinal
    . embedToFinal
    . errorToIOFinal
    . aurToIO
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
            <> header "arch-hs - a program generating PKGBUILD for hackage packages."
        )
  let isDefault = isInfixOf "YOUR_HACKAGE_MIRROR" $ optHackagePath
  when isDefault $ C.skipMessage "You didn't pass -h, use hackage index file from default places."

  let isFlagEmpty = optFlags == []
      cookedFlags = cookFlag optFlags
  when isFlagEmpty $ C.skipMessage "You didn't pass -f, different flag values may make difference in dependency solving."
  when (not isFlagEmpty) $ do
    C.infoMessage "You assigned flags:"
    putStrLn . prettyFlagAssignments $ cookedFlags
  when optAur $ C.infoMessage "You passed -a, searching AUR may takes a long time."

  hackage <- if isDefault then defaultHackageDB else loadHackageDB optHackagePath
  C.infoMessage "Loading hackage..."
  community <- defaultCommunity
  C.infoMessage "Loading community.db..."
  C.infoMessage "Start running..."
  runH hackage community cookedFlags (h optTarget optOutputDir optAur) >>= \case
    Left x -> C.errorMessage $ "Error: " <> (T.pack . show $ x)
    _ -> C.successMessage "Success!"

-----------------------------------------------------------------------------

cookFlag :: [(String, String, Bool)] -> Map.Map PackageName FlagAssignment
cookFlag [] = Map.empty
cookFlag list =
  Map.fromList
    . fmap (\l -> (mkPackageName . (^. _1) . head $ l, foldr (\(_, f, v) acc -> insertFlagAssignment (mkFlagName f) v acc) (mkFlagAssignment []) l))
    . groupBy (\a b -> a ^. _1 == b ^. _1)
    $ list

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
prettyFlagAssignment m = mconcat $ fmap (\(n, v) -> "⚐ " ++ C.formatWith [C.yellow] (unFlagName n) ++ " : " ++ C.formatWith [C.cyan] (show v) ++ "\n") $ unFlagAssignment m

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
