{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Distribution.ArchHs.Internal.NamePresetLoader (loadNamePreset) where

import Data.Aeson
import qualified Data.ByteString as BS
import Data.Map.Strict (Map, fromList, keys, toList)
import Data.Tuple (swap)
import Language.Haskell.TH
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))

loadNamePreset :: DecsQ
loadNamePreset = do
  txt <- runIO $ getCurrentDirectory >>= \dot -> BS.readFile $ dot </> "data" </> "NAME_PRESET.json"
  let preset = case decodeStrict @(Map String String) txt of
        Just x -> x
        _ -> error "Failed to parse json"
  a <- genFunc "extraToHackageP" preset
  b <- genFunc "hackageToExtraP" $ fromList . fmap swap . toList $ preset
  d <- genArray "extraListP" $ keys preset
  return [a, b, d]

genFunc :: String -> Map String String -> DecQ
genFunc name src = do
  let temp = genClause <$> toList src
  funD (mkName name) $ temp <> [nothingClause]
  where
    genClause (from, to) =
      clause
        [litP $ stringL from]
        (normalB $ [|Just|] `appE` (litE . stringL $ to))
        []

    nothingClause = clause [wildP] (normalB [|Nothing|]) []

genArray :: String -> [String] -> DecQ
genArray name src = funD (mkName name) [clause [] (normalB [|src|]) []]
