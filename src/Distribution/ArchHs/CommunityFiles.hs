module Distribution.ArchHs.CommunityFiles where

import Distribution.ArchHs.Internal.Prelude
-- | Default path to @community.db@.
defaultCommunityFilesPath :: FilePath
defaultCommunityFilesPath = "/" </> "var" </> "lib" </> "pacman" </> "sync" </> "community.files"
