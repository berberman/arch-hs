{-# LANGUAGE DerivingStrategies #-}

module Check.Options
  ( CheckOptions (..),
    CheckSort (..),
    checkOptionsParser,
  )
where

import Distribution.ArchHs.Options

data CheckOptions = CheckOptions
  { optShowGHCLibs :: Bool,
    optDepCheck :: Bool,
    optVerbose :: Bool,
    optSort :: Maybe CheckSort,
    optReverse :: Bool
  }
  deriving stock (Eq, Show)

data CheckSort = SortName | SortUpdated | SortPkgrel
  deriving stock (Eq, Show)

checkOptionsParser :: Parser CheckOptions
checkOptionsParser =
  CheckOptions
    <$> switch (long "show-ghc-libs" <> help "Include GHC and its bundled libraries and tools")
    <*> switch (long "depcheck" <> help "Check whether newer Hackage versions are currently upgradable")
    <*> switch (long "verbose" <> help "Show failing dependency and reverse dependency ranges for --depcheck candidates")
    <*> optional
      ( option
          ( eitherReader $ \case
              "name" -> Right SortName
              "updated" -> Right SortUpdated
              "pkgrel" -> Right SortPkgrel
              _ -> Left "Expected one of: name, updated, pkgrel"
          )
          ( long "sort"
              <> metavar "name|updated|pkgrel"
              <> help "Sort by package name (A-Z), Arch build time (newest first), or pkgrel (lowest first)"
          )
      )
    <*> switch (long "reverse" <> help "Reverse the package sort order")
