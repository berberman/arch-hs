{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}

module Distribution.ArchHs.Internal.NameLoader (loadNamePreset) where

import           Data.Aeson
import qualified Data.ByteString     as BS
import           Data.Map.Strict     (Map, fromList, keys, toList)
import           Data.Tuple          (swap)
import           GHC.Generics        (Generic)
import           Language.Haskell.TH
import           Paths_arch_hs       (getDataFileName)

data NamePreset = NamePreset
  { falseList :: [String],
    preset    :: Map String String
  }
  deriving stock (Generic)

instance FromJSON NamePreset

presetFilePath :: IO FilePath
presetFilePath = getDataFileName "NAME_PRESET.json"

loadNamePreset :: DecsQ
loadNamePreset = do
  txt <- runIO $ presetFilePath >>= BS.readFile
  let NamePreset {..} = case decodeStrict txt of
        Just x -> x
        _      -> error "Failed to parse json"
  a <- genFunc "communityToHackage" preset
  b <- genFunc "hackageToCommunity" $ fromList . fmap swap . toList $ preset
  c <- genArray "falseList" falseList
  d <- genArray "communityList" $ keys preset
  return [a, b, c, d]

genFunc :: String -> Map String String -> DecQ
genFunc name src = do
  let temp = genCluse <$> toList src
  funD (mkName name) $ temp <> [nothingClause]
  where
    genCluse (from, to) =
      clause
        [litP $ stringL from]
        (normalB $ [|Just|] `appE` (litE . stringL $ to))
        []

    nothingClause = clause [wildP] (normalB [|Nothing|]) []

genArray :: String -> [String] -> DecQ
genArray name src = funD (mkName name) [clause [] (normalB [|src|]) []]
