-- | Copyright: (c) 2020 berberman
-- SPDX-License-Identifier: MIT
-- Maintainer: berberman <1793913507@qq.com>
-- Stability: experimental
-- Portability: portable
-- This module maintains names of packages which are need special treatments during dependency resolving or PKGBUILD generating.
module Distribution.ArchHs.Local
  ( ignoreList,
    ghcLibList,
  )
where

import Distribution.ArchHs.Types
import Distribution.Types.PackageName (mkPackageName)

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
          "old-time",
          "old-locale",
          "integer",
          "unsupported-ghc-version",
          "base",
          "ghc",
          "ghc-prim",
          "ghc-bignum",
          "hans",
          "Win32",
          "ghc-heap"
        ]

-- | Packages already provided by <https://www.archlinux.org/packages/community/x86_64/ghc-libs/ ghc-libs>.
ghcLibList :: PkgList
ghcLibList =
  mkPackageName
    <$> [ "array",
          "base",
          "binary",
          "bytestring",
          "Cabal",
          "containers",
          "deepseq",
          "directory",
          "exceptions",
          "filepath",
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
          "stm",
          "template-haskell",
          "terminfo",
          "text",
          "time",
          "transformers",
          "unix",
          "xhtml"
        ]