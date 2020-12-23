{-# LANGUAGE OverloadedStrings #-}

-- | Copyright: (c) 2020 berberman
-- SPDX-License-Identifier: MIT
-- Maintainer: berberman <berberman@yandex.com>
-- Stability: experimental
-- Portability: portable
-- This module defines input patterns used in executables' cli.
-- "Options.Applicative" is re-exported.
module Distribution.ArchHs.OptionReader
  ( optFlagReader,
    optSkippedReader,
    optExtraCabalReader,
    optVersionReader,
    optPackageNameReader,
    module Options.Applicative,
  )
where

import qualified Data.Map.Strict as Map
import Data.Void (Void)
import Distribution.ArchHs.Internal.Prelude
import Options.Applicative
import System.FilePath (takeExtension)
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M

readFlag :: [(String, String, Bool)] -> Map.Map PackageName FlagAssignment
readFlag list =
  Map.map toAssignment $
    foldr (\(name, fname, fvalue) acc -> Map.insertWith (<>) (mkPackageName name) [(mkFlagName fname, fvalue)] acc) Map.empty list
  where
    toAssignment = foldr (\(fname, fvalue) acc -> insertFlagAssignment fname fvalue acc) (mkFlagAssignment [])

-- | Read a set of package name with flag assignments.
--
-- >>> f ""
-- >>> f "package_name:flag_name:true"
-- Right (fromList [(PackageName "package_name",fromList [(FlagName "flag_name",(1,True))])])
-- >>> f "package_name:flag_name_1:true,package_name:flag_name_2:false"
-- Right (fromList [(PackageName "package_name",fromList [(FlagName "flag_name_1",(1,True)),(FlagName "flag_name_2",(1,False))])])
-- >>> f "package_name_1:flag_name_1:false,package_name_2:flag_name_2:true"
-- Right (fromList [(PackageName "package_name_1",fromList [(FlagName "flag_name_1",(1,False))]),(PackageName "package_name_2",fromList [(FlagName "flag_name_2",(1,True))])])
-- >>> f "zzz"
-- Left "1:4:\n  |\n1 | zzz\n  |    ^\nunexpected end of input\nexpecting ':'\n"
optFlagReader :: ReadM (Map.Map PackageName FlagAssignment)
optFlagReader =
  eitherReader
    ( \s -> case M.parse optFlagParser "" s of
        Right x -> Right x
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
        "true" -> return True
        "false" -> return False
        _ -> fail $ "unknown bool: " <> s

-- | Read skipped components.
-- This never fails, i.e. the return value will be 'Right'.
-- >>> f ""
-- Right [""]
-- >>> f "component_1,component_2"
-- Right ["component_1","component_2"]
optSkippedReader :: ReadM [String]
optSkippedReader = eitherReader $ Right . splitOn ","

-- | Read extra cabal files.
--
-- >>> f ""
-- Left "Unexpected file name: "
-- >>> f "a.cabal"
-- Right ["a.cabal"]
-- >>> f "a.cabal,b.cabal"
-- Right ["a.cabal","b.cabal"]
-- >>> f "a.what,b.cabal"
-- Left "Unexpected file name: a.what"
optExtraCabalReader :: ReadM [FilePath]
optExtraCabalReader = eitherReader $ \x ->
  let split = splitOn "," x
      check = map (\e -> if takeExtension e == ".cabal" then (e, True) else (e, False)) split
      failed = map fst . filter (not . snd) $ check
      successful = map fst . filter snd $ check
   in if failed /= [] then Left ("Unexpected file name: " <> intercalate ", " failed) else Right successful

-- | Read a 'Version'
-- This function calls 'simpleParsec'.
optVersionReader :: ReadM Version
optVersionReader =
  eitherReader
    ( \s -> case simpleParsec s of
        Just v -> Right v
        _ -> Left $ "Failed to parse version: " <> s
    )

-- | Read a 'PackageName'
-- This function never fails, because it just wraps the input string with 'mkPackageName'.
optPackageNameReader :: ReadM PackageName
optPackageNameReader = eitherReader $ Right . mkPackageName
