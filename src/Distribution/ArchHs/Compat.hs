{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}

module Distribution.ArchHs.Compat
  ( pattern PkgFlag,
    PkgFlag,
  )
where

import Distribution.Types.ConfVar
import Distribution.Types.Flag

pattern PkgFlag :: FlagName -> ConfVar
{-# COMPLETE PkgFlag #-}

#if MIN_VERSION_Cabal(3,4,0)
type PkgFlag = PackageFlag
pattern PkgFlag x = PackageFlag x
#else
type PkgFlag = Flag
pattern PkgFlag x = Flag x
#endif
