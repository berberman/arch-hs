{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}

module Distribution.ArchHs.Name
  ( MyName,
    unMyName,
    HasMyName (..),
    mToCommunityName,
    mToHackageName,
    toCommunityName,
    toHackageName,
    falseList,
  )
where

import           Data.Char                      (toLower)
import           Data.List                      (isPrefixOf)
import qualified Data.Map.Strict                as Map
import           Data.String                    (IsString, fromString)
import           Distribution.ArchHs.Types
import           Distribution.Types.PackageName (PackageName, mkPackageName,
                                                 unPackageName)

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

communityPreset :: Map.Map (MyName 'CommunityRep) (MyName 'HackageRep)
communityPreset = Map.fromList $ (\(x, y) -> (MyName x, MyName y)) <$> preset

hackagePreset :: Map.Map (MyName 'HackageRep) (MyName 'CommunityRep)
hackagePreset = Map.fromList $ (\(x, y) -> (MyName y, MyName x)) <$> preset

mToCommunityName :: MyName 'CommunityRep -> CommunityName
mToCommunityName = CommunityName . unMyName

mToHackageName :: MyName 'HackageRep -> PackageName
mToHackageName = mkPackageName . unMyName

toCommunityName :: HasMyName n => n -> CommunityName
toCommunityName = mToCommunityName . toCommunityRep

toHackageName :: HasMyName n => n -> PackageName
toHackageName = mToHackageName . toHackageRep

falseList :: [MyName 'CommunityRep]
falseList =
  MyName
    <$> [ "haskell-network2.8",
          "haskell-sbv8.7"
        ]

preset :: [(String, String)]
preset =
  [ ("agda", "Agda"),
    ("alex", "alex"),
    ("arch-hs", "arch-hs"),
    ("c2hs", "c2hs"),
    ("cabal-install", "cabal-install"),
    ("cgrep", "cgrep"),
    ("cryptol", "cryptol"),
    ("darcs", "darcs"),
    ("dhall", "dhall"),
    ("dhall-bash", "dhall-bash"),
    ("dhall-json", "dhall-json"),
    ("dhall-lsp-server", "dhall-lsp-server"),
    ("dhall-yaml", "dhall-yaml"),
    ("git-annex", "git-annex"),
    ("git-repair", "git-repair"),
    ("happy", "happy"),
    ("haskell-chasingbottoms", "ChasingBottoms"),
    ("haskell-configfile", "ConfigFile"),
    ("haskell-cracknum", "crackNum"),
    ("haskell-dav", "DAV"),
    ("haskell-decimal", "Decimal"),
    ("haskell-diff", "Diff"),
    ("haskell-edisonapi", "EdisonAPI"),
    ("haskell-edisoncore", "EdisonCore"),
    ("haskell-findbin", "FindBin"),
    ("haskell-floatinghex", "FloatingHex"),
    ("haskell-glob", "Glob"),
    ("haskell-gtk", "gtk3"),
    ("haskell-graphscc", "GraphSCC"),
    ("haskell-hopenpgp", "hOpenPGP"),
    ("haskell-http", "HTTP"),
    ("haskell-hunit", "HUnit"),
    ("haskell-ifelse", "IfElse"),
    ("haskell-juicypixels", "JuicyPixels"),
    ("haskell-lexer", "haskell-lexer"),
    ("haskell-listlike", "ListLike"),
    ("haskell-missingh", "MissingH"),
    ("haskell-monadlib", "monadLib"),
    ("haskell-monadrandom", "MonadRandom"),
    ("haskell-only", "Only"),
    ("haskell-puremd5", "pureMD5"),
    ("haskell-quickcheck", "QuickCheck"),
    ("haskell-ranged-sets", "Ranged-sets"),
    ("haskell-safesemaphore", "SafeSemaphore"),
    ("haskell-sbv8.7", "haskell-sbv8.7"),
    ("haskell-sha", "SHA"),
    ("haskell-smtlib", "smtLib"),
    ("haskell-src-exts", "haskell-src-exts"),
    ("haskell-src-exts-util", "haskell-src-exts-util"),
    ("haskell-src-meta", "haskell-src-meta"),
    ("haskell-statevar", "StateVar"),
    ("haskell-stmonadtrans", "STMonadTrans"),
    ("haskell-unixutils", "Unixutils"),
    ("haskell-x11", "X11"),
    ("haskell-x11-xft", "X11-xft"),
    ("hasktags", "hasktags"),
    ("hledger", "hledger"),
    ("hledger-api", "hledger-api"),
    ("hledger-ui", "hledger-ui"),
    ("hledger-web", "hledger-web"),
    ("hlint", "hlint"),
    ("hoogle", "hoogle"),
    ("hopenpgp-tools", "hopenpgp-tools"),
    ("idris", "idris"),
    ("misfortune", "misfortune"),
    ("pandoc", "pandoc"),
    ("pandoc-citeproc", "pandoc-citeproc"),
    ("pandoc-crossref", "pandoc-crossref"),
    ("postgrest", "postgrest"),
    ("shellcheck", "ShellCheck"),
    ("stack", "stack"),
    ("stylish-haskell", "stylish-haskell"),
    ("tamarin-prover", "tamarin-prover"),
    ("taskell", "taskell"),
    ("tidalcycles", "tidal"),
    ("unlambda", "unlambda"),
    ("xmobar", "xmobar"),
    ("xmonad", "xmonad"),
    ("xmonad-contrib", "xmonad-contrib"),
    ("xmonad-utils", "xmonad-utils")
  ]
