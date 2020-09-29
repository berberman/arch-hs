{-# LANGUAGE DerivingStrategies        #-}
{-# LANGUAGE ExistentialQuantification #-}

module Distribution.ArchHs.Exception
  ( WithMyErr,
    MyException (..),
  )
where

import           Distribution.ArchHs.Name
import           Distribution.ArchHs.Types
import Distribution.ArchHs.Internal.Prelude

-- | Error effect of 'MyException'
type WithMyErr = Error MyException

-- | Custom exception used in this project
data MyException
  = forall n. (HasMyName n) => PkgNotFound n
  | VersionError PackageName Version
  | TargetExist PackageName DependencyProvider
  | CyclicError [PackageName]

instance Show MyException where
  show (PkgNotFound name) = "Unable to find [" <> (unPackageName $ toHackageName name) <> "] (hackage name) / [" <> (unCommunityName $ toCommunityName name) <> "] (community name)"
  show (VersionError name version) = "Unable to find [" <> unPackageName name <> "-" <> prettyShow version <> "]"
  show (TargetExist name provider) = "Target [" <> unPackageName name <> "] has been provided by " <> show provider
  show (CyclicError c) = "Graph contains a cycle " <> (show $ fmap unPackageName c)
