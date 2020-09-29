{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE OverloadedLists  #-}
{-# LANGUAGE CPP  #-}

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
import           Data.Char                            (toLower)
import qualified Data.Map.Strict                      as Map
import           Data.Map.Strict                      (Map)
import           Data.String                          (IsString, fromString)
import           Distribution.ArchHs.Internal.Prelude
import           Distribution.ArchHs.Types

data NameRep = CommunityRep | HackageRep

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
      go s = case hackagePreset Map.!? (MyName s) of
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
      go s = case communityPreset Map.!? (MyName s) of
        Just x -> x
        _      -> MyName $ drop 8 s
  toCommunityRep = MyName . unCommunityName

communityPreset :: Map (MyName 'CommunityRep) (MyName 'HackageRep)
communityPreset = preset

hackagePreset :: Map.Map (MyName 'HackageRep) (MyName 'CommunityRep)
hackagePreset = Map.fromList . (fmap (\(x, y) -> (y, x))) . Map.toList $ preset

mToCommunityName :: MyName 'CommunityRep -> CommunityName
mToCommunityName = CommunityName . unMyName

mToHackageName :: MyName 'HackageRep -> PackageName
mToHackageName = mkPackageName . unMyName

toCommunityName :: HasMyName n => n -> CommunityName
toCommunityName = mToCommunityName . toCommunityRep

toHackageName :: HasMyName n => n -> PackageName
toHackageName = mToHackageName . toHackageRep

isHaskellPackage :: CommunityName -> Bool
isHaskellPackage name = let rep = toCommunityRep name in (rep `Map.member` communityPreset || "haskell-" `isPrefixOf` (unMyName rep)) &&  rep `notElem` falseList

#include "../../../NAME_PRESET.hs"
