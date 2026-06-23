{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Check (check) where

import Control.Monad (forM)
import qualified Data.Map.Strict as Map
import Distribution.ArchHs.DepCheck
import Distribution.ArchHs.Exception
import Distribution.ArchHs.Hackage
import Distribution.ArchHs.Internal.Prelude
import Distribution.ArchHs.Name (isGHCLibs)
import Distribution.ArchHs.PP
import Distribution.ArchHs.RDepCheck
import Distribution.ArchHs.Types
import Distribution.Package (packageName)
import Utils

data NewerVersion = NewerVersion Version (Maybe CheckResult)

data CheckResult = CheckResult
  { depFailures :: [DependencyFailure],
    rdepFailures :: [ReverseDependencyFailure]
  }

data ReverseDependencyFailure = ReverseDependencyFailure ArchLinuxName DepSrc VersionRange

check ::
  Members
    [ HackageEnv,
      ExtraEnv,
      KnownGHCVersion,
      FlagAssignmentsEnv,
      Trace,
      DependencyRecord,
      WithMyErr,
      Embed IO
    ]
    r =>
  Bool ->
  Bool ->
  Bool ->
  Sem r ()
check includeGHC runDepCheck verbose = do
  linked <- linkedHaskellPackageDescs
  checked <-
    traverse
      ( \(archName, desc, cabal) -> do
          let hackageName = packageName cabal
              rawArchVersion = _version desc
          case simpleParsec rawArchVersion of
            Just archVersion
              | includeGHC || not (isGHCLibs hackageName) -> do
                  hackageVersions <- getNewerVersions hackageName archVersion
                  if null hackageVersions
                    then pure ([], [])
                    else do
                      (newerVersions, skipped) <- checkNewerVersions runDepCheck hackageName hackageVersions
                      pure ([prettyNewerVersions verbose archName (_rawVersion desc) hackageName archVersion newerVersions], skipped)
            _ -> pure ([], [])
      )
      linked
  let result = concatMap fst checked
      skipped = uniqueSkippedReverseDeps $ concatMap snd checked
  mapM_ (printWarn . prettySkippedReverseDep) skipped
  if null result
    then printSuccess "Finished checking"
    else do
      printWarn "Finished checking with inconsistenc(ies):"
      embed $ putDoc $ vcat result <> line

checkNewerVersions ::
  Members
    [ ExtraEnv,
      HackageEnv,
      KnownGHCVersion,
      FlagAssignmentsEnv,
      Trace,
      DependencyRecord,
      WithMyErr,
      Embed IO
    ]
    r =>
  Bool ->
  PackageName ->
  [Version] ->
  Sem r ([NewerVersion], [SkippedReverseDep])
checkNewerVersions False _ hackageVersions =
  pure ((\hackageVersion -> NewerVersion hackageVersion Nothing) <$> hackageVersions, [])
checkNewerVersions True hackageName hackageVersions = do
  (reverseDeps, skipped) <- reverseDependencyRangesWithSkips hackageName
  newerVersions <-
    forM hackageVersions $ \hackageVersion -> do
      cabal <- getCabal hackageName hackageVersion
      depFailureDetails <- dependencyFailures cabal
      pure $
        NewerVersion
          hackageVersion
          ( Just
              CheckResult
                { depFailures = depFailureDetails,
                  rdepFailures = rdepFailureDetails hackageVersion reverseDeps
                }
          )
  pure (newerVersions, skipped)

uniqueSkippedReverseDeps :: [SkippedReverseDep] -> [SkippedReverseDep]
uniqueSkippedReverseDeps =
  Map.elems
    . Map.fromList
    . fmap
      ( \skipped ->
          ( (skippedReverseDepName skipped, show $ skippedReverseDepError skipped),
            skipped
          )
      )

rdepFailureDetails :: Version -> [ReverseDep] -> [ReverseDependencyFailure]
rdepFailureDetails version reverseDeps =
  [ ReverseDependencyFailure name src range
    | ReverseDep name ranges <- reverseDeps,
      (src, range) <- versionFailures (Just version) ranges
    ]

prettyNewerVersions :: Bool -> ArchLinuxName -> ArchLinuxVersion -> PackageName -> Version -> [NewerVersion] -> Doc AnsiStyle
prettyNewerVersions verbose archName rawArchVersion hackageName archVersion hackageVersions =
  base <> verboseDetails
  where
    base =
      annMagneta (pretty (unArchLinuxName archName))
        <+> "in"
        <+> ppExtra
        <+> "has version"
        <+> prettyArchVersion rawArchVersion archVersion
        <> comma
          <+> "but linked"
          <+> annMagneta (pretty (unPackageName hackageName))
          <+> "in"
          <+> annCyan "Hackage"
          <+> (if length hackageVersions == 1 then "has newer version" else "has newer versions")
          <+> hsep (punctuate comma $ prettyNewerVersion <$> hackageVersions)

    verboseDetails =
      case concatMap prettyVerboseNewerVersion hackageVersions of
        details | verbose && not (null details) -> line <> indent 2 (vsep details)
        _ -> mempty

prettyArchVersion :: ArchLinuxVersion -> Version -> Doc AnsiStyle
prettyArchVersion rawVersion archVersion =
  annRed (viaPretty archVersion) <> maybe mempty (annBlue . pretty) (pkgrelSuffix rawVersion)

pkgrelSuffix :: ArchLinuxVersion -> Maybe String
pkgrelSuffix rawVersion =
  case splitOn "-" withoutEpoch of
    _ : pkgrelParts@(_ : _) -> Just $ "-" <> intercalate "-" pkgrelParts
    _ -> Nothing
  where
    withoutEpoch =
      case splitOn ":" rawVersion of
        [_epoch, versionRelease] -> versionRelease
        _ -> rawVersion

prettyNewerVersion :: NewerVersion -> Doc AnsiStyle
prettyNewerVersion (NewerVersion version Nothing) = annGreen $ viaPretty version
prettyNewerVersion (NewerVersion version (Just CheckResult {depFailures = [], rdepFailures = []})) =
  annGreen $ viaPretty version <+> parens "ok"
prettyNewerVersion (NewerVersion version (Just failures)) =
  annRed $ viaPretty version <+> parens ("blocked:" <+> prettyCheckFailures failures)

prettyCheckFailures :: CheckResult -> Doc AnsiStyle
prettyCheckFailures CheckResult {..} =
  hsep . punctuate comma $
    ["dep=" <> pretty (length depFailures) | not (null depFailures)]
      <> ["rdep=" <> pretty (length rdepFailures) | not (null rdepFailures)]

prettyVerboseNewerVersion :: NewerVersion -> [Doc AnsiStyle]
prettyVerboseNewerVersion (NewerVersion _ Nothing) = []
prettyVerboseNewerVersion (NewerVersion _ (Just CheckResult {depFailures = [], rdepFailures = []})) = []
prettyVerboseNewerVersion (NewerVersion version (Just CheckResult {..})) =
  (viaPretty version <> colon)
    : fmap (indent 2 . prettyDependencyFailure) depFailures
      <> fmap (indent 2 . prettyReverseDependencyFailure) rdepFailures

prettyDependencyFailure :: DependencyFailure -> Doc AnsiStyle
prettyDependencyFailure = \case
  MissingDependency name range ->
    annRed "dep:"
      <+> viaPretty name
      <+> "requires"
      <+> viaPretty range
      <> comma
      <+> ppExtra
      <+> "missing"
  DependencyOutOfRange name range version ->
    annRed "dep:"
      <+> viaPretty name
      <+> "requires"
      <+> viaPretty range
      <> comma
      <+> ppExtra
      <+> "has"
      <+> viaPretty version

prettyReverseDependencyFailure :: ReverseDependencyFailure -> Doc AnsiStyle
prettyReverseDependencyFailure (ReverseDependencyFailure name src range) =
  annRed "rdep:"
    <+> pretty (unArchLinuxName name)
    <+> pretty src
    <+> "requires"
    <+> viaPretty range
