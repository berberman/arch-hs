-- | Copyright: (c) 2020-2021 berberman
-- SPDX-License-Identifier: MIT
-- Maintainer: berberman <berberman@yandex.com>
-- Stability: experimental
-- Portability: portable
-- This module maintains names of packages which are need special treatments during dependency resolving or PKGBUILD generating.
module Distribution.ArchHs.Local
  ( ignoreList,
    ghcLibList,
    ghcPlaceholderVersions,
  )
where

import qualified Data.Map.Strict as Map
import Distribution.ArchHs.Types
import Distribution.Types.PackageName
import Distribution.Types.Version (Version, mkVersion)

-- | Packages should be dropped in dependency resolving.
ignoreList :: PkgList
ignoreList =
  mkPackageName
    <$> [ "unbuildable",
          "invalid-cabal-flag-settings",
          "par-classes",
          "fail",
          "integer-simple",
          "bytestring-builder",
          "nats",
          "integer",
          "unsupported-ghc-version",
          "rts",
          "ghc-prim",
          "ghc-bignum",
          "hans",
          "Win32",
          "ghc-heap",
          "ghc-byteorder",
          "ghc-platform",
          "system-cxx-std-lib",
          -- a build-tools of "zip-archive", which is not haskell package
          "unzip"
        ]

-- | Packages provided by GHC, including runtime and system-library placeholders.
ghcLibList :: PkgList
ghcLibList =
  mkPackageName
    <$> [ "array",
          "base",
          "binary",
          "bytestring",
          "Cabal",
          "Cabal-syntax",
          "containers",
          "deepseq",
          "directory",
          "exceptions",
          "filepath",
          "ghc-bignum",
          "ghc-boot",
          "ghc-boot-th",
          "ghc-compact",
          "ghc-heap",
          "ghci",
          "ghc-prim",
          "haskeline",
          "hpc",
          "integer-gmp",
          "libiserv",
          "mtl",
          "parsec",
          "pretty",
          "process",
          "rts",
          "stm",
          "system-cxx-std-lib",
          "template-haskell",
          "terminfo",
          "text",
          "time",
          "transformers",
          "unix",
          "xhtml"
        ]

-- | Versions omitted from Arch's compiler provides, from Stackage's
-- @stack/global-hints.yaml@. Unknown compiler releases have no fallback.
ghcPlaceholderVersions :: Map.Map Version (Map.Map PackageName Version)
ghcPlaceholderVersions = Map.fromList
  [ ( compiler,
      Map.fromList $
        [(mkPackageName "rts", mkVersion runtime)]
          <> [(mkPackageName "system-cxx-std-lib", mkVersion [1, 0]) | compiler >= mkVersion [9, 4, 1]]
    )
    | (major, minor, patches, runtime) <-
        [ (7, 8, [3, 4], [1, 0]),
          (7, 10, [1 .. 3], [1, 0]),
          (8, 0, [1, 2], [1, 0]),
          (8, 2, [1, 2], [1, 0]),
          (8, 4, [1 .. 4], [1, 0]),
          (8, 6, [1 .. 5], [1, 0]),
          (8, 8, [1 .. 4], [1, 0]),
          (8, 10, [1 .. 4], [1, 0]),
          (8, 10, [5 .. 7], [1, 0, 1]),
          (9, 0, [1], [1, 0]),
          (9, 0, [2], [1, 0, 2]),
          (9, 2, [2 .. 8], [1, 0, 2]),
          (9, 4, [1 .. 8], [1, 0, 2]),
          (9, 6, [1 .. 7], [1, 0, 2]),
          (9, 8, [1 .. 4], [1, 0, 2]),
          (9, 10, [1 .. 3], [1, 0, 2]),
          (9, 12, [1 .. 3], [1, 0, 2]),
          (9, 12, [4], [1, 0, 3]),
          (9, 14, [1], [1, 0, 3])
        ],
      patch <- patches,
      let compiler = mkVersion [major, minor, patch]
  ]
