{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK hide #-}

module OptionParse
  ( readFlag,
    optFlagReader,
    optFlagParser,
    optSkippedReader,
    optExtraCabalReader,
    optVersionReader,
    optPackageNameReader,
    module Options.Applicative,
  )
where

import           Data.List                      (groupBy, intercalate)
import           Data.List.Split                (splitOn)
import qualified Data.Map.Strict                as Map
import           Data.Void                      (Void)
import           Distribution.ArchHs.Types
import           Distribution.ArchHs.Utils
import           Distribution.Parsec            (simpleParsec)
import           Distribution.Types.Flag        (FlagAssignment,
                                                 insertFlagAssignment,
                                                 mkFlagAssignment, mkFlagName)
import           Distribution.Types.PackageName (PackageName, mkPackageName)
import           Distribution.Version           (Version)
import           Options.Applicative
import           System.FilePath                (takeExtension)
import qualified Text.Megaparsec                as M
import qualified Text.Megaparsec.Char           as M

readFlag :: [(String, String, Bool)] -> Map.Map PackageName FlagAssignment
readFlag [] = Map.empty
readFlag list =
  Map.fromList
    . fmap (\l -> (mkPackageName . (^. _1) . head $ l, foldr (\(_, f, v) acc -> insertFlagAssignment (mkFlagName f) v acc) (mkFlagAssignment []) l))
    . groupBy (\a b -> uncurry (==) (getTwo _1 a b))
    $ list

optFlagReader :: ReadM (Map.Map PackageName FlagAssignment)
optFlagReader =
  eitherReader
    ( \s -> case M.parse optFlagParser "" s of
        Right x  -> Right x
        Left err -> Left $ M.errorBundlePretty err
    )

optFlagParser :: M.Parsec Void String (Map.Map PackageName FlagAssignment)
optFlagParser =
  readFlag
    <$> ( do
            pkg <- M.manyTill M.anySingle $ M.single ':'
            flg <- M.manyTill M.anySingle $ M.single ':'
            b <- bool
            return (pkg, flg, b)
        )
    `M.sepBy` ","
  where
    bool = do
      s <- M.string "true" <|> M.string "false"
      case s of
        "true"  -> return True
        "false" -> return False
        _       -> fail $ "unknown bool: " <> s

optSkippedReader :: ReadM [String]
optSkippedReader = eitherReader $ Right . splitOn ","

optExtraCabalReader :: ReadM [FilePath]
optExtraCabalReader = eitherReader $ \x ->
  let splitted = splitOn "," x
      check = map (\e -> if takeExtension x == ".cabal" then (e, True) else (e, False)) splitted
      failed = map fst . filter (not . snd) $ check
      successful = map fst . filter snd $ check
   in if failed /= [] then Left ("Unexpected file name: " <> intercalate ", " failed) else Right successful

optVersionReader :: ReadM Version
optVersionReader =
  eitherReader
    ( \s -> case simpleParsec s of
        Just v -> Right v
        _      -> Left $ "Failed to parse version: " <> s
    )

optPackageNameReader :: ReadM PackageName
optPackageNameReader = eitherReader $ Right . mkPackageName
