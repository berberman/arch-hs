{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Main (main) where

import qualified Algebra.Graph.AdjacencyMap.Algorithm as G
import qualified Algebra.Graph.Labelled.AdjacencyMap as G
import Args
import Aur
import qualified Colourista as C
import Community
import Conduit
import qualified Control.Exception as CE
import Control.Monad (filterM, when)
import Core
import Data.List (groupBy, intercalate, isInfixOf)
import qualified Data.Map.Strict as Map
import qualified Data.Set as S
import qualified Data.Text as T
import Distribution.Hackage.DB (HackageDB)
import Distribution.PackageDescription
import Distribution.Types.PackageName (PackageName, mkPackageName, unPackageName)
import Distribution.Types.UnqualComponentName (mkUnqualComponentName)
import Hackage
import Lens.Micro
import Local
import PkgBuild
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeFileName, (</>))
import Types

app :: Members '[Embed IO, CommunityEnv, HackageEnv, FlagAssignmentEnv, Aur, WithMyErr] r => String -> FilePath -> Bool -> [String] -> Sem r ()
app name path aurSupport skip = do
  let target = mkPackageName name
  deps <- getDependencies S.empty (fmap mkUnqualComponentName skip) target
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
      toBePacked = cooked ^.. each . filtered (\case ProvidedPackage _ _ -> False; _ -> True)
  (cookedAgain, toBePackedAgain) <- do
    embed . when aurSupport $ C.infoMessage "Start searching AUR..."
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
            then cookedAgain ^.. each . filtered (\case ProvidedPackage _ _ -> False; _ -> True)
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

runApp ::
  HackageDB ->
  CommunityDB ->
  Map.Map PackageName FlagAssignment ->
  Sem '[CommunityEnv, HackageEnv, FlagAssignmentEnv, Aur, WithMyErr, Embed IO, Final IO] a ->
  IO (Either MyException a)
runApp hackage community flags =
  runFinal
    . embedToFinal
    . errorToIOFinal
    . aurToIO
    . runReader flags
    . runReader hackage
    . runReader community

main :: IO ()
main = CE.catch @CE.IOException
  ( do
      Options {..} <- runArgsParser

      let useDefaultHackage = isInfixOf "YOUR_HACKAGE_MIRROR" $ optHackagePath
          useDefaultCommunity = "/var/lib/pacman/sync/community.db" == optCommunityPath

      when useDefaultHackage $ C.skipMessage "You didn't pass -h, use hackage index file from default places."
      when useDefaultCommunity $ C.skipMessage "You didn't pass -c, use community db file from default places."

      let isFlagEmpty = optFlags == []
          cookedFlags = cookFlag optFlags
          isSkipEmpty = optSkip == []

      when isFlagEmpty $ C.skipMessage "You didn't pass -f, different flag values may make difference in dependency solving."
      when (not isFlagEmpty) $ do
        C.infoMessage "You assigned flags:"
        putStrLn . prettyFlagAssignments $ cookedFlags

      when (not isSkipEmpty) $ do
        C.infoMessage "You chose to skip:"
        putStrLn $ prettySkip optSkip

      when optAur $ C.infoMessage "You passed -a, searching AUR may takes a long time."

      hackage <- loadHackageDB =<< if useDefaultHackage then lookupHackagePath else return optHackagePath
      C.infoMessage "Loading hackage..."

      let isExtraEmpty = optExtraCabalPath == []

      when (not isExtraEmpty) $
        C.infoMessage $ "You added " <> (T.pack . intercalate ", " $ map takeFileName optExtraCabalPath) <> " as extra cabal file(s), starting parsing right now."

      parsedExtra <- mapM parseCabalFile optExtraCabalPath

      let newHackage = foldr (\x acc -> x `insertDB` acc) hackage parsedExtra

      community <- loadProcessedCommunity $ if useDefaultCommunity then defaultCommunityPath else optCommunityPath
      C.infoMessage "Loading community.db..."

      C.infoMessage "Start running..."

      runApp newHackage community cookedFlags (app optTarget optOutputDir optAur optSkip) >>= \case
        Left x -> C.errorMessage $ "Error: " <> (T.pack . show $ x)
        _ -> C.successMessage "Success!"
  )
  $ \e -> C.errorMessage $ "IOException: " <> (T.pack . show $ e)

-----------------------------------------------------------------------------

prettySkip :: [String] -> String
prettySkip = C.formatWith [C.magenta] . intercalate ", "

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
