{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_HADDOCK hide #-}

module Distribution.ArchHs.PP
  ( prettySkip,
    prettyFlagAssignments,
    prettyFlags,
    prettySolvedPkgs,
    prettyDeps,
  )
where

import qualified Colourista                      as C
import           Data.List                       (intercalate)
import qualified Data.Map.Strict                 as Map
import qualified Data.Text                       as T
import           Distribution.ArchHs.Types
import           Distribution.PackageDescription (Flag (..), FlagAssignment,
                                                  unFlagAssignment, unFlagName)
import           Distribution.Types.PackageName  (PackageName, unPackageName)

prettySkip :: [String] -> String
prettySkip = C.formatWith [C.magenta] . intercalate ", "

prettyFlagAssignments :: Map.Map PackageName FlagAssignment -> String
prettyFlagAssignments m = mconcat $ fmap (fmap (\(n, a) -> C.formatWith [C.magenta] (unPackageName n) <> "\n" <> C.formatWith [C.indent 4] (prettyFlagAssignment a))) Map.toList m

prettyFlagAssignment :: FlagAssignment -> String
prettyFlagAssignment m = mconcat $ fmap (\(n, v) -> "⚐ " <> C.formatWith [C.yellow] (unFlagName n) <> " : " <> C.formatWith [C.cyan] (show v) <> "\n") $ unFlagAssignment m

prettyDeps :: [PackageName] -> String
prettyDeps =
  mconcat
    . fmap (\(i, n) -> show @Int i <> ". " <> unPackageName n <> "\n")
    . zip [1 ..]

prettyFlags :: [(PackageName, [Flag])] -> String
prettyFlags = mconcat . fmap (\(name, flags) -> (C.formatWith [C.magenta] $ unPackageName name <> "\n") <> mconcat (fmap (C.formatWith [C.indent 4] . prettyFlag) flags))

prettyFlag :: Flag -> String
prettyFlag f = "⚐ " <> C.formatWith [C.yellow] name <> ":\n" <> mconcat (fmap (C.formatWith [C.indent 6]) $ ["description:\n" <> desc, "default: " <> def <> "\n", "isManual: " <> manual <> "\n"])
  where
    name = unFlagName . flagName $ f
    desc = unlines . fmap (C.formatWith [C.indent 8]) . lines $ flagDescription f
    def = show $ flagDefault f
    manual = show $ flagManual f

prettySolvedPkgs :: [SolvedPackage] -> String
prettySolvedPkgs = con . mconcat . fmap prettySolvedPkg

prettySolvedPkg :: SolvedPackage -> [(String, String)]
prettySolvedPkg SolvedPackage {..} =
  (C.formatWith [C.bold, C.yellow] (unPackageName _pkgName), C.formatWith [C.red] "    ✘") :
  ( fmap
      ( \(i :: Int, SolvedDependency {..}) ->
          let prefix = if i == length _pkgDeps then " └─" else " ├─"
           in case _depProvider of
                (Just x) -> ((C.formatWith [C.green] $ (T.unpack prefix) <> unPackageName _depName <> " " <> show _depType), ((C.formatWith [C.green] "✔ ") <> (C.formatWith [C.cyan] $ "[" <> show x <> "]")))
                _ -> (C.formatWith [C.bold, C.yellow] $ (T.unpack prefix) <> unPackageName _depName <> " " <> show _depType, C.formatWith [C.red] "    ✘")
      )
      (zip [1 ..] _pkgDeps)
  )
prettySolvedPkg ProvidedPackage {..} = [((C.formatWith [C.green] $ unPackageName _pkgName), ((C.formatWith [C.green] "✔ ") <> (C.formatWith [C.cyan] $ "[" <> show _pkgProvider <> "]")))]

con :: [(String, String)] -> String
con l = mconcat complemented
  where
    maxL = maximum $ fmap (length . fst) l
    complemented = fmap (\(x, y) -> (x <> (replicate (maxL - length x) ' ') <> y <> "\n")) l
