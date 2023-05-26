{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

-- | Copyright: (c) 2020-2021 berberman
-- SPDX-License-Identifier: MIT
-- Maintainer: berberman <berberman@yandex.com>
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
    dui,
    cuo,
    yellowStarInParens,
    ppExtra,
    ppAur,
    ppDBKind,
    annYellow,
    annGreen,
    annMagneta,
    annRed,
    annBold,
    annCyan,
    annBlue,
    render,
    viaPretty,
    splitLine,
    ppFromTo,
    printInfo,
    printWarn,
    printError,
    printSuccess,
    module Prettyprinter,
    module Prettyprinter.Render.Terminal,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Algorithm.Diff
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Distribution.ArchHs.FilesDB (DBKind)
import Distribution.ArchHs.Internal.Prelude
import Distribution.ArchHs.Types
import qualified Distribution.Pretty as DPretty
import Prettyprinter
import Prettyprinter.Render.Terminal

annYellow :: Doc AnsiStyle -> Doc AnsiStyle
annYellow = annotate (color Yellow)

annCyan :: Doc AnsiStyle -> Doc AnsiStyle
annCyan = annotate (color Cyan)

annMagneta :: Doc AnsiStyle -> Doc AnsiStyle
annMagneta = annotate (color Magenta)

annRed :: Doc AnsiStyle -> Doc AnsiStyle
annRed = annotate (color Red)

annGreen :: Doc AnsiStyle -> Doc AnsiStyle
annGreen = annotate (color Green)

annBold :: Doc AnsiStyle -> Doc AnsiStyle
annBold = annotate bold

annBlue :: Doc AnsiStyle -> Doc AnsiStyle
annBlue = annotate (color Blue)

cuo :: Doc AnsiStyle
cuo = annRed "✘"

dui :: Doc AnsiStyle
dui = annGreen "✔"

yellowStarInParens :: Doc AnsiStyle
yellowStarInParens = annYellow $ parens (pretty '*')

prettySkip :: [String] -> Doc AnsiStyle
prettySkip = hsep . punctuate comma . fmap (annotate (color Magenta) . pretty)

prettyFlagAssignments :: Map.Map PackageName FlagAssignment -> Doc AnsiStyle
prettyFlagAssignments m =
  vsep $
    fmap (fmap (\(n, a) -> annMagneta (viaPretty n) <> line <> indent 2 (prettyFlagAssignment a))) Map.toList m

prettyFlagAssignment :: FlagAssignment -> Doc AnsiStyle
prettyFlagAssignment = vsep . fmap (\(n, v) -> "⚐" <+> annotate (color Yellow) (viaPretty n) <> colon <+> annotate (color Cyan) (pretty v)) . unFlagAssignment

prettyDeps :: [(PackageName, Bool)] -> Doc AnsiStyle
prettyDeps =
  vsep
    . fmap
      ( \(i :: Int, (pkg, star)) ->
          pretty i
            <> dot <+> viaPretty pkg
            <> ( if star
                   then space <> yellowStarInParens
                   else mempty
               )
      )
    . zip [1 ..]

prettyFlags :: [(PackageName, [PkgFlag])] -> Doc AnsiStyle
prettyFlags = vsep . fmap (\(name, flags) -> annMagneta (viaPretty name) <> line <> indent 2 (vsep (prettyFlag <$> flags)))

prettyFlag :: PkgFlag -> Doc AnsiStyle
prettyFlag f =
  "⚐" <+> annYellow name <> colon <> line
    <> indent
      4
      ( vsep
          [ "description" <> colon <> line <> indent 2 desc,
            "default" <> colon <+> def,
            "isManual" <> colon <+> manual
          ]
      )
  where
    name = viaPretty . flagName $ f
    desc = pretty $ flagDescription f
    def = viaShow $ flagDefault f
    manual = viaShow $ flagManual f

prettySolvedPkgs :: [SolvedPackage] -> T.Text
prettySolvedPkgs = align2col . mconcat . fmap prettySolvedPkg

prettySolvedPkg :: SolvedPackage -> [(Doc AnsiStyle, Doc AnsiStyle)]
prettySolvedPkg SolvedPackage {..} =
  (annYellow . annBold . viaPretty $ _pkgName, indent 16 cuo) :
  fmap
    ( \(i :: Int, SolvedDependency {..}) ->
        let prefix = if i == length _pkgDeps then " └─" else " ├─"
         in case _depProvider of
              (Just x) -> (annGreen $ prefix <> viaPretty _depName <+> parens (hsep $ punctuate comma (viaShow <$> _depType)), dui <+> annCyan (viaShow x))
              _ -> (annYellow . annBold $ prefix <> viaPretty _depName <+> parens (hsep $ punctuate comma (viaShow <$> _depType)), indent 16 cuo)
    )
    (zip [1 ..] _pkgDeps)
prettySolvedPkg ProvidedPackage {..} = [(annGreen $ viaPretty _pkgName, dui <+> annCyan (viaShow _pkgProvider))]

render :: Doc AnsiStyle -> T.Text
render = renderStrict . layoutPretty defaultLayoutOptions

viaPretty :: DPretty.Pretty a => a -> Doc AnsiStyle
viaPretty = pretty . prettyShow

align2col :: [(Doc AnsiStyle, Doc AnsiStyle)] -> T.Text
align2col (fmap (both %~ render) -> l) = T.concat complemented
  where
    maxL = maximum $ fmap (T.length . fst) l
    complemented = (\(x, y) -> x <> T.replicate (maxL - T.length x) " " <> y <> "\n") <$> l

ppSysDependencies :: Map.Map PackageName [SystemDependency] -> T.Text
ppSysDependencies m = align2col $ uncurry ppSysDependency <$> Map.toList m

ppSysDependency :: PackageName -> [SystemDependency] -> (Doc AnsiStyle, Doc AnsiStyle)
ppSysDependency name deps = ((annBold . annYellow $ viaPretty name) <> colon, hsep $ punctuate comma (fmap (\(SystemDependency x) -> pretty x) deps))

ppDiffColored :: Diff [String] -> [Doc AnsiStyle]
ppDiffColored (First x) = annRed . pretty <$> x
ppDiffColored (Second x) = annGreen . pretty <$> x
ppDiffColored (Both x _) = pretty <$> x

splitLine :: Doc AnsiStyle
splitLine = line <> pretty (replicate 38 '-') <> line

ppFromTo :: Int -> Doc AnsiStyle -> Doc AnsiStyle -> Doc AnsiStyle
ppFromTo i a b = a <> hcat (replicate i space) <> "⇒" <> hcat (replicate i space) <> b

printInfo :: (MonadIO m) => Doc AnsiStyle -> m ()
printInfo msg = liftIO . putDoc . annBlue $ "ⓘ" <+> msg <> line

ppExtra :: Doc AnsiStyle
ppExtra = annCyan $ viaShow ByExtra

ppDBKind :: DBKind -> Doc AnsiStyle
ppDBKind x = annCyan . brackets $ viaShow x

ppAur :: Doc AnsiStyle
ppAur = annCyan $ viaShow ByAur

printWarn :: (MonadIO m) => Doc AnsiStyle -> m ()
printWarn msg = liftIO . putDoc . annYellow $ "⚠" <+> msg <> line

printError :: (MonadIO m) => Doc AnsiStyle -> m ()
printError msg = liftIO . putDoc . annRed $ "🛑" <+> msg <> line

printSuccess :: (MonadIO m) => Doc AnsiStyle -> m ()
printSuccess msg = liftIO . putDoc . annGreen $ dui <+> msg <> line
