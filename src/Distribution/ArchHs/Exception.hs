{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Copyright: (c) 2020 berberman
-- SPDX-License-Identifier: MIT
-- Maintainer: berberman <berberman@yandex.com>
-- Stability: experimental
-- Portability: portable
-- Exceptions used in this project.
module Distribution.ArchHs.Exception
  ( WithMyErr,
    MyException (..),
    printHandledIOException,
    printAppResult,
    tryMaybe,
    interceptHttpException,
  )
where

import qualified Control.Exception as CE
import Distribution.ArchHs.Internal.Prelude
import Distribution.ArchHs.Name
import Distribution.ArchHs.PP (colon, printError, printSuccess, viaShow, (<+>))
import Distribution.ArchHs.Types
import Network.HTTP.Client (HttpException)
import Servant.Client (ClientError (ConnectionError))

-- | Error effect of 'MyException'
type WithMyErr = Error MyException

-- | Custom exception used in this project
data MyException
  = forall n. (HasMyName n) => PkgNotFound n
  | VersionNotFound PackageName Version
  | TargetExist PackageName DependencyProvider
  | CyclicExist [PackageName]
  | NetworkException ClientError
  | TargetDisappearException PackageName

instance Show MyException where
  show (PkgNotFound name) = "Unable to find \"" <> unPackageName (toHackageName name) <> "\""
  show (VersionNotFound name version) = "Unable to find \"" <> unPackageName (toHackageName name) <> "\" " <> prettyShow version
  show (TargetExist name provider) = "Target \"" <> unPackageName name <> "\" has been provided by " <> show provider
  show (CyclicExist c) = "Graph contains a cycle \"" <> show (fmap unPackageName c) <> "\""
  show (NetworkException e) = show e
  show (TargetDisappearException name) = "Target \"" <> unPackageName name <> "\" is discarded during the dependency resolving"

-- | Catch 'CE.IOException' and print it.
printHandledIOException :: IO () -> IO ()
printHandledIOException = CE.handle @CE.IOException (\e -> printError $ "IOException" <> colon <+> viaShow e)

-- | Print the result of 'errorToIOFinal'.
printAppResult :: IO (Either MyException ()) -> IO ()
printAppResult io =
  io >>= \case
    Left x -> printError $ "Runtime Exception" <> colon <+> viaShow x
    _ -> printSuccess "Success!"

-- | Like 'try' but discard the concrete exception.
tryMaybe :: Member WithMyErr r => Sem r a -> Sem r (Maybe a)
tryMaybe m =
  try @MyException m >>= \case
    Left _ -> return Nothing
    Right x -> return $ Just x

-- | Catch the 'HttpException' thrown in 'IO' monad, then re-throw it with 'NetworkException'.
interceptHttpException :: Members [WithMyErr, Embed IO] r => IO a -> Sem r a
interceptHttpException io = do
  x <- embed $ CE.try io
  case x of
    Left (err :: HttpException) -> throw . NetworkException . ConnectionError . CE.SomeException $ err
    Right x' -> return x'
