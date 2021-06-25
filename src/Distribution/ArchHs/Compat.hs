{-# LANGUAGE CPP #-}

module Distribution.ArchHs.Compat (
    PackageFlag
) where

#if MIN_VERSION_Cabal(3,4,0)
import Distribution.Types.Flag (PackageFlag)
#else
import Distribution.Types.Flag (Flag)
type PackageFlag = Flag
#endif
