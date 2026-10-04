{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}

module Distribution.ArchHs.Compat
  ( pattern PkgFlag,
    PkgFlag,
    licenseFile,
  )
where

import Data.Maybe (listToMaybe)
import Distribution.Types.ConfVar
import Distribution.Types.Flag
import Distribution.Types.PackageDescription (PackageDescription, licenseFiles)
import Distribution.Utils.Path (getSymbolicPath)

pattern PkgFlag :: FlagName -> ConfVar
{-# COMPLETE PkgFlag #-}

type PkgFlag = PackageFlag
pattern PkgFlag x = PackageFlag x

licenseFile :: PackageDescription -> Maybe FilePath
licenseFile = fmap getSymbolicPath . listToMaybe . licenseFiles
