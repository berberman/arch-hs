-- | Copyright: (c) 2020-2021 berberman
-- SPDX-License-Identifier: MIT
-- Maintainer: berberman <berberman@yandex.com>
-- Stability: experimental
-- Portability: portable
-- This module maintains names of packages which are need special treatments during dependency resolving or PKGBUILD generating.
module Distribution.ArchHs.Local
  ( ignoreList,
    ghcLibList,
  )
where

import Distribution.ArchHs.Types
import Distribution.Types.PackageName

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
          -- a build-tools of "zip-archive", which is not haskell package
          "unzip"
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
          "stm",
          "template-haskell",
          "terminfo",
          "text",
          "time",
          "transformers",
          "unix",
          "xhtml"
        ]
