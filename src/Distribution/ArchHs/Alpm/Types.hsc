{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}

module Distribution.ArchHs.Alpm.Types where

#include <alpm.h>
#include <alpm_list.h>

import Foreign
import Foreign.C.String
import Foreign.C.Types

data AlpmFile = AlpmFile
  { alpm_file_name :: !CString,
    alpm_file_off :: !CLong,
    alpm_file_mode :: !CUInt
  }

instance Storable AlpmFile where
  sizeOf _ = (#size alpm_file_t)
  alignment _ = (#alignment alpm_file_t)
  peek ptr = do
    alpm_file_name <- (#peek alpm_file_t, name) ptr
    alpm_file_off <- (#peek alpm_file_t, size) ptr
    alpm_file_mode <- (#peek alpm_file_t, mode) ptr
    return AlpmFile {..}
  poke ptr AlpmFile {..} = do
    (#poke alpm_file_t, name) ptr alpm_file_name
    (#poke alpm_file_t, size) ptr alpm_file_off
    (#poke alpm_file_t, mode) ptr alpm_file_mode

data AlpmFileList = AlpmFileList
  { alpm_filelist_count :: CSize,
    alpm_filelist_files :: Ptr AlpmFile
  }

instance Storable AlpmFileList where
  sizeOf _ = (#size alpm_filelist_t)
  alignment _ = (#alignment alpm_filelist_t)
  peek ptr = do
    alpm_filelist_count <- (#peek alpm_filelist_t, count) ptr
    alpm_filelist_files <- (#peek alpm_filelist_t, files) ptr
    return AlpmFileList {..}
  poke ptr AlpmFileList {..} = do
    (#poke alpm_filelist_t, count) ptr alpm_filelist_count
    (#poke alpm_filelist_t, files) ptr alpm_filelist_files

data AlpmList = AlpmList{
  alpm_list_data :: Ptr (),
  alpm_list_prev :: Ptr AlpmList,
  alpm_list_next :: Ptr AlpmList
}

instance Storable AlpmList where
  sizeOf _ = (#size alpm_list_t)
  alignment _ = (#alignment alpm_list_t)
  peek ptr = do
    alpm_list_data <- (#peek alpm_list_t, data) ptr
    alpm_list_prev <- (#peek alpm_list_t, prev) ptr
    alpm_list_next <- (#peek alpm_list_t, next) ptr
    return AlpmList {..}
  poke ptr AlpmList {..} = do
    (#poke alpm_list_t, data) ptr alpm_list_data
    (#poke alpm_list_t, prev) ptr alpm_list_prev
    (#poke alpm_list_t, next) ptr alpm_list_next
