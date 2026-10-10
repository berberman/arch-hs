{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}

module Distribution.ArchHs.Compat
  ( pattern PkgFlag,
    PkgFlag,
    pattern ZeroBSD,
    licenseFile,
  )
where

import Data.Maybe (listToMaybe)
import Distribution.SPDX.LicenseId (LicenseId)
import qualified Distribution.SPDX.LicenseId as SPDX
import Distribution.Types.ConfVar
import Distribution.Types.Flag
import Distribution.Types.PackageDescription (PackageDescription, licenseFiles)
import Distribution.Utils.Path (getSymbolicPath)

pattern PkgFlag :: FlagName -> ConfVar
{-# COMPLETE PkgFlag #-}

type PkgFlag = PackageFlag
pattern PkgFlag x = PackageFlag x

pattern ZeroBSD :: LicenseId
#if MIN_VERSION_Cabal_syntax(3,16,0)
pattern ZeroBSD = SPDX.N_0BSD
#else
pattern ZeroBSD = SPDX.NullBSD
#endif

licenseFile :: PackageDescription -> Maybe FilePath
licenseFile = fmap getSymbolicPath . listToMaybe . licenseFiles
