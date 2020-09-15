{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK hide #-}

module Data.Aeson.Ext
  ( generateJSONInstance,
    parseJSONDrop,
    toJSONDrop,
  )
where

import Data.Aeson
import Data.Aeson.Types
import GHC.Generics (Generic, Rep)
import Language.Haskell.TH

dropSize :: Type -> Q Int
dropSize (ConT n) = do
  info <- reify n
  case info of
    (TyConI (DataD _ _ [] _ cons _)) -> go cons
    (TyConI (NewtypeD _ _ _ _ con _)) -> go [con]
    _ -> fail "Unsupported."
  where
    -- Try only the first record constructor
    rec (x : _) = case x of
      (RecC _ f) -> f
      _ -> fail "Unsupported"
    rec [] = fail "Unsupported"
    go cons = do
      let fields = rec cons
      -- Find prefix from the first field
      let (name, _, _) = head fields
      let str = nameBase name
      return . (+ 1) $ length str - length (dropWhile (/= '_') str)
dropSize _ = fail "Unsupported"

generateToJSONInstance :: Name -> DecQ
generateToJSONInstance targetType =
  conT targetType >>= dropSize >>= \s -> instanceD (cxt []) (appT (conT ''ToJSON) (conT targetType)) [gen_f $ fromIntegral s]
  where
    gen_f s = funD (mkName "toJSON") [clause [] (normalB (varE 'toJSONDrop `appE` litE (integerL s))) []]

generateFromJSONInstance :: Name -> DecQ
generateFromJSONInstance targetType =
  conT targetType >>= dropSize >>= \s -> instanceD (cxt []) (appT (conT ''FromJSON) (conT targetType)) [gen_f $ fromIntegral s]
  where
    gen_f s = funD (mkName "parseJSON") [clause [] (normalB (varE 'parseJSONDrop `appE` litE (integerL s))) []]

generateJSONInstance :: Name -> Q [Dec]
generateJSONInstance name = do
  from <- generateFromJSONInstance name
  to <- generateToJSONInstance name
  return [from, to]

toJSONDrop ::
  forall a.
  (Generic a, GToJSON Zero (Rep a)) =>
  Int ->
  a ->
  Value
toJSONDrop prefix =
  genericToJSON defaultOptions {fieldLabelModifier = drop prefix, omitNothingFields = True, sumEncoding = UntaggedValue}

parseJSONDrop ::
  forall a.
  (Generic a, GFromJSON Zero (Rep a)) =>
  Int ->
  Value ->
  Parser a
parseJSONDrop prefix = genericParseJSON defaultOptions {fieldLabelModifier = drop prefix, sumEncoding = UntaggedValue}