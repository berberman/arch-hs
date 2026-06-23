{-# LANGUAGE OverloadedStrings #-}

module Check (check) where

import Distribution.ArchHs.Exception
import Distribution.ArchHs.Hackage
import Distribution.ArchHs.Internal.Prelude
import Distribution.ArchHs.Name (isGHCLibs)
import Distribution.ArchHs.PP
import Distribution.ArchHs.Types
import Distribution.Package (packageName)
import Utils

check :: Members [HackageEnv, ExtraEnv, WithMyErr, Embed IO] r => Bool -> Sem r ()
check includeGHC = do
  linked <- linkedHaskellPackages
  result <- fmap concat $
    traverse
      ( \(archName, rawArchVersion, cabal) -> do
          let hackageName = packageName cabal
          case simpleParsec rawArchVersion of
            Just archVersion
              | includeGHC || not (isGHCLibs hackageName) -> do
                  hackageVersions <- getNewerVersions hackageName archVersion
                  pure [prettyNewerVersions archName hackageName archVersion hackageVersions | not (null hackageVersions)]
            _ -> pure []
      )
      linked
  if null result
    then printSuccess "Finished checking"
    else do
      printWarn "Finished checking with inconsistenc(ies):"
      embed $ putDoc $ vcat result <> line

prettyNewerVersions :: ArchLinuxName -> PackageName -> Version -> [Version] -> Doc AnsiStyle
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
      <+> hsep (punctuate comma $ annGreen . viaPretty <$> hackageVersions)
