{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Distribution.ArchHs.Internal.NamePresetLoader (loadNamePreset) where

import Data.Aeson
import qualified Data.ByteString as BS
import Data.Map.Strict (Map, fromList, keys, toList)
import Data.Tuple (swap)
import GHC.Generics (Generic)
import Language.Haskell.TH
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))

data NamePreset = NamePreset
  { falseList :: [String],
    preset :: Map String String
  }
  deriving stock (Generic)

instance FromJSON NamePreset

loadNamePreset :: DecsQ
loadNamePreset = do
  txt <- runIO $ getCurrentDirectory >>= \dot -> BS.readFile $ dot </> "data" </> "NAME_PRESET.json"
  let NamePreset {..} = case decodeStrict txt of
        Just x -> x
        _ -> error "Failed to parse json"
  a <- genFunc "communityToHackageP" preset
  b <- genFunc "hackageToCommunityP" $ fromList . fmap swap . toList $ preset
  c <- genArray "falseListP" falseList
  d <- genArray "communityListP" $ keys preset
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
