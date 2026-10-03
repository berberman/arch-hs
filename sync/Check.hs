{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Check (check, checkNewerVersions, prettyNewerVersions) where

import Control.Monad (forM)
import Data.List (partition)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Distribution.ArchHs.DepCheck
import Distribution.ArchHs.Exception
import Distribution.ArchHs.Hackage
import Distribution.ArchHs.Internal.Prelude
import Distribution.ArchHs.Name (isGHCLibs)
import Distribution.ArchHs.PP
import Distribution.ArchHs.RDepCheck
import Distribution.ArchHs.Types
import Utils

data NewerVersion
  = NewerVersion Version (Maybe CheckResult)
  | UncheckedVersion Version MyException

data CheckResult = CheckResult
  { depFailures :: [DependencyFailure],
    rdepFailures :: [ReverseDependencyFailure],
    existingRdepFailures :: [ReverseDependencyFailure]
  }

data ReverseDependencyFailure = ReverseDependencyFailure ArchLinuxName DepSrc VersionRange

check ::
  Members
    [ HackageEnv,
      RawHackageEnv,
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
  extra <- ask @ExtraDB
  let providedByGHC = Set.fromList
        [ name
          | provider <- ["ghc", "ghc-libs"],
            Just desc <- [Map.lookup (ArchLinuxName provider) extra],
            PkgDependent name _ <- _provides desc
        ]
  linked <- linkedHaskellPackageDescs
  checked <-
    traverse
      ( \(archName, desc, hackageName) -> do
          let rawArchVersion = _version desc
          case simpleParsec rawArchVersion of
            Just archVersion
              | includeGHC || (not (isGHCLibs hackageName) && Set.notMember archName providedByGHC) -> do
                  hackageVersions <- getNewerVersions hackageName archVersion
                  if null hackageVersions
                    then pure ([], [])
                    else do
                      (newerVersions, skipped) <- checkNewerVersions runDepCheck hackageName archVersion hackageVersions
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
      RawHackageEnv,
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
  Version ->
  [Version] ->
  Sem r ([NewerVersion], [SkippedReverseDep])
checkNewerVersions False _ _ hackageVersions =
  pure ((\hackageVersion -> NewerVersion hackageVersion Nothing) <$> hackageVersions, [])
checkNewerVersions True hackageName archVersion hackageVersions = do
  (reverseDeps, skipped) <- reverseDependencyRangesWithSkips hackageName
  newerVersions <-
    forM hackageVersions $ \hackageVersion -> do
      -- Candidates are already filtered by preferred versions. Parse the raw
      -- cabal here so failures use MyException instead of hackage-db's throws.
      eCabal <- try @MyException $ getCabalIncludingDeprecated hackageName hackageVersion
      case eCabal of
        Left err -> pure $ UncheckedVersion hackageVersion err
        Right cabal -> do
          depFailureDetails <- dependencyFailures cabal
          let (newRdepFailures, oldRdepFailures) =
                partition
                  (\(ReverseDependencyFailure _ _ range) -> withinRange archVersion range)
                  (rdepFailureDetails hackageVersion reverseDeps)
          pure $
            NewerVersion
              hackageVersion
              ( Just
                  CheckResult
                    { depFailures = depFailureDetails,
                      rdepFailures = newRdepFailures,
                      existingRdepFailures = oldRdepFailures
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
prettyNewerVersion (UncheckedVersion version _) =
  annRed $ viaPretty version <+> parens "unchecked: cabal parse failed"
prettyNewerVersion (NewerVersion version Nothing) = annGreen $ viaPretty version
prettyNewerVersion (NewerVersion version (Just CheckResult {depFailures = [], rdepFailures = [], existingRdepFailures = []})) =
  annGreen $ viaPretty version <+> parens "ok"
prettyNewerVersion (NewerVersion version (Just failures@CheckResult {depFailures = [], rdepFailures = []})) =
  annYellow $ viaPretty version <+> parens ("existing:" <+> prettyCheckFailures failures)
prettyNewerVersion (NewerVersion version (Just failures)) =
  annRed $ viaPretty version <+> parens ("blocked:" <+> prettyCheckFailures failures)

prettyCheckFailures :: CheckResult -> Doc AnsiStyle
prettyCheckFailures CheckResult {..} =
  hsep . punctuate comma $
    ["dep=" <> pretty (length depFailures) | not (null depFailures)]
      <> ["rdep=" <> pretty (length rdepFailures) | not (null rdepFailures)]
      <> ["rdep-old=" <> pretty (length existingRdepFailures) | not (null existingRdepFailures)]

prettyVerboseNewerVersion :: NewerVersion -> [Doc AnsiStyle]
prettyVerboseNewerVersion (UncheckedVersion version err) =
  [viaPretty version <> colon, indent 2 $ viaShow err]
prettyVerboseNewerVersion (NewerVersion _ Nothing) = []
prettyVerboseNewerVersion (NewerVersion _ (Just CheckResult {depFailures = [], rdepFailures = [], existingRdepFailures = []})) = []
prettyVerboseNewerVersion (NewerVersion version (Just CheckResult {..})) =
  (viaPretty version <> colon)
    : fmap (indent 2 . prettyDependencyFailure) depFailures
      <> fmap (indent 2 . prettyReverseDependencyFailure (annRed "rdep:")) rdepFailures
      <> fmap (indent 2 . prettyReverseDependencyFailure (annYellow "rdep-old:")) existingRdepFailures

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

prettyReverseDependencyFailure :: Doc AnsiStyle -> ReverseDependencyFailure -> Doc AnsiStyle
prettyReverseDependencyFailure label (ReverseDependencyFailure name src range) =
  label
    <+> pretty (unArchLinuxName name)
    <+> pretty src
    <+> "requires"
    <+> viaPretty range
