{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Check (check) where

import Control.Monad (forM)
import Distribution.ArchHs.Exception
import Distribution.ArchHs.Internal.Prelude
import Distribution.ArchHs.PP
import Distribution.ArchHs.RDepCheck
import Distribution.ArchHs.Types

check ::
  Members
    [ ExtraEnv,
      RawHackageEnv,
      KnownGHCVersion,
      FlagAssignmentsEnv,
      Trace,
      DependencyRecord,
      WithMyErr,
      Embed IO
    ]
    r =>
  Maybe Version ->
  PackageName ->
  Sem r Int
check mVersion target = do
  reverseDeps <- reverseDependencyRanges target
  failures <- forM reverseDeps $ \ReverseDep {..} -> do
    let failedRanges = versionFailures mVersion reverseDepRanges
    embed . putDoc $
      vsep
        ( annMagneta "Reverse dependency" <> colon
            <+> pretty (unArchLinuxName reverseDepName)
            : (rangeDocs reverseDepRanges <> versionErrors mVersion failedRanges)
        )
        <> line
    pure $ length failedRanges
  pure $ sum failures

rangeDocs :: [(DepSrc, VersionRange)] -> [Doc AnsiStyle]
rangeDocs result =
  [ indent 2 $ pretty s <> colon <+> viaPretty r
    | (s, r) <- result
  ]

versionErrors :: Maybe Version -> [(DepSrc, VersionRange)] -> [Doc AnsiStyle]
versionErrors Nothing _ = []
versionErrors (Just version) result =
  [ indent 2 $
      annRed "Error:"
        <+> viaPretty version
        <+> "is outside"
        <+> pretty src
        <+> "range"
        <+> parens (viaPretty range)
    | (src, range) <- result
  ]
