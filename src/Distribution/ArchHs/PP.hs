{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Copyright: (c) 2020 berberman
-- SPDX-License-Identifier: MIT
-- Maintainer: berberman <1793913507@qq.com>
-- Stability: experimental
-- Portability: portable
-- This module provides simple pretty-printing functions work with the cli program.
module Distribution.ArchHs.PP
  ( prettySkip,
    prettyFlagAssignments,
    prettyFlags,
    prettySolvedPkgs,
    prettyDeps,
    ppSysDependencies,
    ppDiffColored,
    align2col,
  )
where

import qualified Colourista as C
import Data.Algorithm.Diff
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Distribution.ArchHs.Internal.Prelude
import Distribution.ArchHs.Types

prettySkip :: [String] -> String
prettySkip = C.formatWith [C.magenta] . intercalate ", "

prettyFlagAssignments :: Map.Map PackageName FlagAssignment -> String
prettyFlagAssignments m =
  mconcat $
    fmap (fmap (\(n, a) -> C.formatWith [C.magenta] (unPackageName n) <> "\n" <> prettyFlagAssignment a)) Map.toList m

prettyFlagAssignment :: FlagAssignment -> String
prettyFlagAssignment m =
  mconcat $
    (\(n, v) -> "    ⚐ " <> C.formatWith [C.yellow] (unFlagName n) <> " : " <> C.formatWith [C.cyan] (show v) <> "\n") <$> unFlagAssignment m

prettyDeps :: [PackageName] -> String
prettyDeps =
  mconcat
    . fmap (\(i, n) -> show (i :: Int) <> ". " <> unPackageName n <> "\n")
    . zip [1 ..]

prettyFlags :: [(PackageName, [Flag])] -> String
prettyFlags = mconcat . fmap (\(name, flags) -> C.formatWith [C.magenta] (unPackageName name <> "\n") <> mconcat (C.formatWith [C.indent 4] . prettyFlag <$> flags))

prettyFlag :: Flag -> String
prettyFlag f = "⚐ " <> C.formatWith [C.yellow] name <> ":\n" <> mconcat (C.formatWith [C.indent 6] <$> ["description:\n" <> desc, "default: " <> def <> "\n", "isManual: " <> manual <> "\n"])
  where
    name = unFlagName . flagName $ f
    desc = unlines . fmap (C.formatWith [C.indent 8]) . lines $ flagDescription f
    def = show $ flagDefault f
    manual = show $ flagManual f

prettySolvedPkgs :: [SolvedPackage] -> String
prettySolvedPkgs = align2col . mconcat . fmap prettySolvedPkg

prettySolvedPkg :: SolvedPackage -> [(String, String)]
prettySolvedPkg SolvedPackage {..} =
  (C.formatWith [C.bold, C.yellow] (unPackageName _pkgName), C.formatWith [C.red] "    ✘") :
  fmap
    ( \(i :: Int, SolvedDependency {..}) ->
        let prefix = if i == length _pkgDeps then " └─" else " ├─"
         in case _depProvider of
              (Just x) -> (C.formatWith [C.green] $ T.unpack prefix <> unPackageName _depName <> " " <> show _depType, C.formatWith [C.green] "✔ " <> C.formatWith [C.cyan] (show x))
              _ -> (C.formatWith [C.bold, C.yellow] $ T.unpack prefix <> unPackageName _depName <> " " <> show _depType, C.formatWith [C.red] "    ✘")
    )
    (zip [1 ..] _pkgDeps)
prettySolvedPkg ProvidedPackage {..} = [(C.formatWith [C.green] (unPackageName _pkgName), C.formatWith [C.green] "✔ " <> C.formatWith [C.cyan] (show _pkgProvider))]

align2col :: [(String, String)] -> String
align2col l = mconcat complemented
  where
    maxL = maximum $ fmap (length . fst) l
    complemented = (\(x, y) -> x <> replicate (maxL - length x) ' ' <> y <> "\n") <$> l

ppSysDependencies :: Map.Map PackageName [SystemDependency] -> String
ppSysDependencies m = align2col $ uncurry ppSysDependency <$> Map.toList m

ppSysDependency :: PackageName -> [SystemDependency] -> (String, String)
ppSysDependency name deps = (C.formatWith [C.bold, C.yellow] (unPackageName name) <> ": ", intercalate ", " (fmap (\(SystemDependency x) -> x) deps))

ppDiffColored :: Diff [String] -> [String]
ppDiffColored (First x) = C.formatWith [C.red] <$> x
ppDiffColored (Second x) = C.formatWith [C.green] <$> x
ppDiffColored (Both x _) = x