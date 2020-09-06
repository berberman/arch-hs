{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Aur
  ( AurReply (..),
    AurSearch (..),
    AurInfo (..),
    Aur,
    searchByName,
    infoByName,
    isInAur,
    aurToIO,
  )
where

import Data.Aeson
import Data.Aeson.Ext
import Data.Text (Text, pack)
import Distribution.Types.PackageName (PackageName, unPackageName)
import GHC.Generics (Generic)
import Network.HTTP.Req
import Polysemy
import Utils

data AurReply a = AurReply
  { r_version :: Int,
    r_type :: String,
    r_resultcount :: Int,
    r_results :: [a]
  }
  deriving stock (Show, Generic)

data AurSearch = AurSearch
  { s_ID :: Int,
    s_Name :: String,
    s_PackageBaseID :: Int,
    s_PackageBase :: String,
    s_Version :: String,
    s_Description :: String,
    s_URL :: String,
    s_NumVotes :: Int,
    s_Popularity :: Double,
    s_OutOfDate :: Maybe Int,
    s_Maintainer :: Maybe String,
    s_FirstSubmitted :: Int, -- UTC
    s_LastModified :: Int, -- UTC
    s_URLPath :: String
  }
  deriving stock (Show, Generic)

data AurInfo = AurInfo
  { i_ID :: Int,
    i_Name :: String,
    i_PackageBaseID :: Int,
    i_PackageBase :: String,
    i_Version :: String,
    i_Description :: String,
    i_URL :: String,
    i_NumVotes :: Int,
    i_Popularity :: Double,
    i_OutOfDate :: Maybe Int,
    i_Maintainer :: Maybe String,
    i_FirstSubmitted :: Int, -- UTC
    i_LastModified :: Int, -- UTC
    i_URLPath :: String,
    i_Depends :: Maybe [String],
    i_MakeDepends :: Maybe [String],
    i_OptDepends :: Maybe [String],
    i_CheckDepends :: Maybe [String],
    i_Conflicts :: Maybe [String],
    i_Provides :: Maybe [String],
    i_Replaces :: Maybe [String],
    i_Groups :: Maybe [String],
    i_License :: Maybe [String],
    i_Keywords :: Maybe [String]
  }
  deriving stock (Show, Generic)

instance (FromJSON a) => FromJSON (AurReply a) where
  parseJSON (Object v) =
    AurReply
      <$> v .: "version"
        <*> v .: "type"
        <*> v .: "resultcount"
        <*> v .: "results"
  parseJSON  _  = fail "Unable to parse AUR reply."
instance (ToJSON a) => ToJSON (AurReply a)

$(generateJSONInstance ''AurSearch)
$(generateJSONInstance ''AurInfo)

data Aur m a where
  SearchByName :: String -> Aur m (Maybe AurSearch)
  InfoByName :: String -> Aur m (Maybe AurInfo)
  IsInAur :: PackageName -> Aur m Bool

makeSem ''Aur

reqToIO :: forall a r. Member (Embed IO) r => Req a -> Sem r a
reqToIO r = embed @IO $ runReq defaultHttpConfig r

baseURL :: Url 'Https
baseURL = https "aur.archlinux.org" /: "rpc"

aurToIO :: Member (Embed IO) r => Sem (Aur ': r) a -> Sem r a
aurToIO = interpret $ \case
  (SearchByName name) -> do
    let parms =
          "v" =: ("5" :: Text)
            <> "type" =: ("search" :: Text)
            <> "by" =: ("name" :: Text)
            <> "arg" =: (pack name)
        r = req GET baseURL NoReqBody jsonResponse parms
    response <- reqToIO r
    let body :: AurReply AurSearch = responseBody response
    return $ case r_resultcount body of
      1 -> Just . head $ r_results body
      _ -> Nothing
  (InfoByName name) -> do
    let parms =
          "v" =: ("5" :: Text)
            <> "type" =: ("info" :: Text)
            <> "by" =: ("name" :: Text)
            <> "arg[]" =: (pack name)
        r = req GET baseURL NoReqBody jsonResponse parms
    response <- reqToIO r
    let body :: AurReply AurInfo = responseBody response
    return $ case r_resultcount body of
      1 -> Just . head $ r_results body
      _ -> Nothing
  (IsInAur name) -> do
    result <- aurToIO . searchByName . fixName $ unPackageName name
    return $ case result of
      Just _ -> True
      _ -> False