{-# LANGUAGE DerivingStrategies        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}

module Distribution.ArchHs.Exception
  ( WithMyErr,
    MyException (..),
    printHandledIOException,
    printAppResult,
    interpretHttpException,
  )
where

import qualified Colourista                           as C
import qualified Control.Exception                    as CE
import qualified Data.Text                            as T
import           Distribution.ArchHs.Internal.Prelude
import           Distribution.ArchHs.Name
import           Distribution.ArchHs.Types
import           Network.HTTP.Req                     (HttpException)

-- | Error effect of 'MyException'
type WithMyErr = Error MyException

-- | Custom exception used in this project
data MyException
  = forall n. (HasMyName n) => PkgNotFound n
  | VersionError PackageName Version
  | TargetExist PackageName DependencyProvider
  | CyclicError [PackageName]
  | NetworkError HttpException

instance Show MyException where
  show (PkgNotFound name) = "Unable to find [" <> (unPackageName $ toHackageName name) <> "] (hackage name) / [" <> (unCommunityName $ toCommunityName name) <> "] (community name)"
  show (VersionError name version) = "Unable to find [" <> unPackageName name <> "-" <> prettyShow version <> "]"
  show (TargetExist name provider) = "Target [" <> unPackageName name <> "] has been provided by " <> show provider
  show (CyclicError c) = "Graph contains a cycle " <> (show $ fmap unPackageName c)
  show (NetworkError c) = "Failed to request " <> (show c)

printHandledIOException :: IO () -> IO ()
printHandledIOException = CE.handle @CE.IOException (\e -> C.errorMessage $ "IOException: " <> (T.pack . show $ e))

printAppResult :: IO (Either MyException ()) -> IO ()
printAppResult io =
  io >>= \case
    Left x -> C.errorMessage $ "Runtime Error: " <> (T.pack . show $ x)
    _ -> C.successMessage "Success!"

interpretHttpException :: Members [WithMyErr, Embed IO] r => IO a -> Sem r a
interpretHttpException io = do
  x <- embed $ CE.try io
  case x of
    Left err -> throw $ NetworkError err
    Right x' -> return x'
