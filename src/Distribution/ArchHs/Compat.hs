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
#if MIN_VERSION_Cabal(3,6,0)
import Distribution.Utils.Path (getSymbolicPath)
#endif

pattern PkgFlag :: FlagName -> ConfVar
{-# COMPLETE PkgFlag #-}

#if MIN_VERSION_Cabal(3,4,0)
type PkgFlag = PackageFlag
pattern PkgFlag x = PackageFlag x
#else
type PkgFlag = Flag
pattern PkgFlag x = Flag x
#endif

licenseFile :: PackageDescription -> Maybe FilePath
#if MIN_VERSION_Cabal(3,6,0)
licenseFile = fmap getSymbolicPath . listToMaybe . licenseFiles
#else
licenseFile = listToMaybe . licenseFiles
#endif
