{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedLists    #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE TemplateHaskell    #-}

module Distribution.ArchHs.Name
  ( MyName,
    unMyName,
    HasMyName (..),
    mToCommunityName,
    mToHackageName,
    toCommunityName,
    toHackageName,
    isHaskellPackage,
  )
where

import           Data.Char                               (toLower)
import           Data.String                             (IsString, fromString)
import           Distribution.ArchHs.Internal.NameLoader
import           Distribution.ArchHs.Internal.Prelude
import           Distribution.ArchHs.Types

data NameRep = CommunityRep | HackageRep

$(loadNamePreset)

communityToHackage :: MyName 'CommunityRep -> Maybe (MyName 'HackageRep)
hackageToCommunity :: MyName 'HackageRep -> Maybe (MyName 'CommunityRep)
falseList :: [MyName 'CommunityRep]
communityList :: [MyName 'CommunityRep]

newtype MyName a = MyName {unMyName :: String}
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving anyclass (NFData)

instance IsString (MyName a) where
  fromString = MyName

class HasMyName a where
  toHackageRep :: a -> MyName 'HackageRep
  toCommunityRep :: a -> MyName 'CommunityRep

instance HasMyName (MyName 'CommunityRep) where
  toHackageRep = toHackageRep . CommunityName . unMyName
  toCommunityRep = id

instance HasMyName (MyName 'HackageRep) where
  toHackageRep = id
  toCommunityRep = toCommunityRep . mkPackageName . unMyName

instance HasMyName PackageName where
  toHackageRep = MyName . unPackageName
  toCommunityRep = go . unPackageName
    where
      go s = case hackageToCommunity (MyName s) of
        Just x -> x
        _ ->
          MyName . fmap toLower $
            ( if "haskell-" `isPrefixOf` s
                then s
                else "haskell-" <> s
            )

instance HasMyName CommunityName where
  toHackageRep = go . unCommunityName
    where
      go s = case communityToHackage (MyName s) of
        Just x -> x
        _      -> MyName $ drop 8 s
  toCommunityRep = MyName . unCommunityName

mToCommunityName :: MyName 'CommunityRep -> CommunityName
mToCommunityName = CommunityName . unMyName

mToHackageName :: MyName 'HackageRep -> PackageName
mToHackageName = mkPackageName . unMyName

toCommunityName :: HasMyName n => n -> CommunityName
toCommunityName = mToCommunityName . toCommunityRep

toHackageName :: HasMyName n => n -> PackageName
toHackageName = mToHackageName . toHackageRep

isHaskellPackage :: CommunityName -> Bool
isHaskellPackage name = let rep = toCommunityRep name in (rep `elem` communityList || "haskell-" `isPrefixOf` (unMyName rep)) && rep `notElem` falseList
