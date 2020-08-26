-- {- |
-- Copyright: (c) 2020 berberman
-- SPDX-License-Identifier: MIT
-- Maintainer: berberman <1793913507@qq.com>

-- See README for more info
-- -}

-- module ArchHs
--        ( someFunc
--        ) where


-- someFunc :: IO ()
-- someFunc = putStrLn ("someFunc" :: String)


{-# LANGUAGE RecordWildCards #-}

module ArchHs where

import Conduit
import Data.List
import System.Directory
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Algebra.Graph.AdjacencyMap.Algorithm as G
import qualified Algebra.Graph.AdjacencyMap as G

type Parser = Parsec Void String

data PkgDesc = PkgDesc
  { name :: String,
    dep :: [String]
  }
  deriving (Show)

fieldList =
  [ "FILENAME",
    "NAME",
    "BASE",
    "VERSION",
    "DESC",
    "CSIZE",
    "ISIZE",
    "URL",
    "LICENSE",
    "ARCH",
    "BUILDDATE",
    "PACKAGER",
    "DEPENDS",
    "MAKEDEPENDS",
    "PROVIDES",
    "OPTDEPENDS",
    "REPLACES",
    "CONFLICTS"
  ]

sep :: Parser ()
sep = () <$ char '%'

fieldParser :: [Parser (String)]
fieldParser = map string fieldList

line :: Parser String
line = manyTill anySingle newline

parser = do
  sep
  field <- many (upperChar <|> digitChar)
  sep

  newline

  content <- manyTill line (lookAhead sep <|> () <$ eol <|> eof)
  return (field, content)

parseDesc :: String -> PkgDesc
parseDesc s =
  let (Right x) = parse (manyTill parser eof) "parse" s
      xs = filter (\(n, _) -> n == "NAME" || n == "PROVIDES") x
   in PkgDesc
        (head . snd . head $ xs)
        ( case length xs of
            2 -> snd $ xs !! 1
            _ -> []
        )

dirConduit :: ConduitT FilePath PkgDesc IO ()
dirConduit = awaitForever $ \path -> do
  c <- liftIO $ readFile path
  let PkgDesc{..} = parseDesc c

  if any ("haskell" `isInfixOf`) dep  || any ("ghc" `isInfixOf`) dep && "haskell" `isInfixOf` (name)
    then yield $ parseDesc c
    else return ()

run :: IO ()
run = do
  let s = "/home/berberman/Downloads/community/community.db/"
  dirs <- listDirectory s
  let pkgDescFiles = map (\it -> s ++ it ++ "/desc") dirs
  result <- runConduit $ yieldMany pkgDescFiles .| dirConduit .| sinkList
  let g = foldr (\PkgDesc{..} acc -> acc `G.overlay`  G.edges (zip (repeat name) dep) ) G.empty result
  print $ G.topSort g
  return ()

