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

data NewerVersion = NewerVersion Version (Maybe CheckFailures)

data CheckFailures = CheckFailures
  { depFailures :: Int,
    rdepFailures :: Int
  }

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
  Sem r ()
check includeGHC runDepCheck = do
  linked <- linkedHaskellPackages
  checked <-
    traverse
      ( \(archName, rawArchVersion, cabal) -> do
          let hackageName = packageName cabal
          case simpleParsec rawArchVersion of
            Just archVersion
              | includeGHC || not (isGHCLibs hackageName) -> do
                  hackageVersions <- getNewerVersions hackageName archVersion
                  if null hackageVersions
                    then pure ([], [])
                    else do
                      (newerVersions, skipped) <- checkNewerVersions runDepCheck hackageName hackageVersions
                      pure ([prettyNewerVersions archName hackageName archVersion newerVersions], skipped)
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
      depFailureCount <- length <$> dependencyFailures cabal
      pure $
        NewerVersion
          hackageVersion
          ( Just
              CheckFailures
                { depFailures = depFailureCount,
                  rdepFailures = rdepFailureCount hackageVersion reverseDeps
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

rdepFailureCount :: Version -> [ReverseDep] -> Int
rdepFailureCount version reverseDeps =
  sum
    [ length $ versionFailures (Just version) ranges
      | ReverseDep _ ranges <- reverseDeps
    ]

prettyNewerVersions :: ArchLinuxName -> PackageName -> Version -> [NewerVersion] -> Doc AnsiStyle
prettyNewerVersions archName hackageName archVersion hackageVersions =
  annMagneta (pretty (unArchLinuxName archName))
    <+> "in"
    <+> ppExtra
    <+> "has version"
    <+> annRed (viaPretty archVersion)
    <> comma
      <+> "but linked"
      <+> annMagneta (pretty (unPackageName hackageName))
      <+> "in"
      <+> annCyan "Hackage"
      <+> (if length hackageVersions == 1 then "has newer version" else "has newer versions")
      <+> hsep (punctuate comma $ prettyNewerVersion <$> hackageVersions)

prettyNewerVersion :: NewerVersion -> Doc AnsiStyle
prettyNewerVersion (NewerVersion version Nothing) = annGreen $ viaPretty version
prettyNewerVersion (NewerVersion version (Just CheckFailures {depFailures = 0, rdepFailures = 0})) =
  annGreen $ viaPretty version <+> parens "upgradable"
prettyNewerVersion (NewerVersion version (Just failures)) =
  annRed $ viaPretty version <+> parens ("not upgradable:" <+> prettyCheckFailures failures)

prettyCheckFailures :: CheckFailures -> Doc AnsiStyle
prettyCheckFailures CheckFailures {..} =
  hsep . punctuate comma $
    [pretty depFailures <+> "dependency range failure(s)" | depFailures > 0]
      <> [pretty rdepFailures <+> "reverse dependency range failure(s)" | rdepFailures > 0]
