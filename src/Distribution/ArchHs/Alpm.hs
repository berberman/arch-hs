{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}

module Distribution.ArchHs.Alpm where

import Distribution.ArchHs.Alpm.Types
import Foreign

foreign import ccall "alpm_list.h alpm_list_next"
  alpmListNext :: Ptr AlpmList -> IO (Ptr AlpmList)

foreign import ccall "alpm_list.h alpm_list_free"
  alpmListFree :: Ptr AlpmList -> IO ()
