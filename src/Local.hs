module Local
  ( ignoreList,
    ghcLibList,
  )
where

import Distribution.Types.PackageName
import Types

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
          "ghcjs-prim",
          "ghc-bignum",
          "hans",
          "Win32"
        ]

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