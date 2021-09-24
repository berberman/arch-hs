{-# LANGUAGE OverloadedStrings #-}

module Check (check) where

import Data.Maybe (maybeToList)
import Distribution.ArchHs.Exception
import Distribution.ArchHs.Internal.Prelude
import Distribution.ArchHs.Name (isGHCLibs)
import Distribution.ArchHs.PP
import Distribution.ArchHs.Types
import Distribution.Package (packageName, packageVersion)
import Utils

check :: Members [HackageEnv, CommunityEnv, WithMyErr, Embed IO] r => Bool -> Sem r ()
check includeGHC = do
  linked <- linkedHaskellPackages
  let result =
        [ annMagneta (pretty (unArchLinuxName archName))
            <+> "in"
            <+> ppCommunity
            <+> "has version"
            <+> (if isHackageNewer then annRed else annGreen)
              (viaPretty archVersion)
            <> comma
              <+> "but linked"
              <+> annMagneta (pretty (unPackageName hackageName))
              <+> "in"
              <+> annCyan "Hackage"
              <+> "has"
              <+> (if isHackageNewer then "a newer" else "an older")
              <+> "version"
              <+> (if isHackageNewer then annGreen else annRed)
                (viaPretty hackageVersion)
          | (archName, rawArchVersion, cabal) <- linked,
            let hackageName = packageName cabal
                hackageVersion = packageVersion cabal,
            includeGHC || not (isGHCLibs hackageName),
            archVersion <- maybeToList $ simpleParsec rawArchVersion,
            archVersion /= hackageVersion,
            let isHackageNewer = hackageVersion > archVersion
        ]
  if null result
    then printSuccess "Finished checking"
    else do
      printWarn "Finished checking with inconsistenc(ies):"
      embed $ putDoc $ vcat result <> line
