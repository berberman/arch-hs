{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Monad (forM_)
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe, mapMaybe)
import Diff (inRange)
import Distribution.ArchHs.Exception
import Distribution.ArchHs.ExtraDB (defaultExtraDBPath, loadExtraDB)
import Distribution.ArchHs.Name (isGHCLibs, isHaskellPackage, toHackageName)
import Distribution.ArchHs.Types
import Distribution.Parsec (simpleParsec)
import Distribution.Types.PackageName (PackageName, unPackageName)
import Distribution.Types.Version (Version)
import Distribution.Types.VersionRange (VersionRange, anyVersion)
import Polysemy (run)
import Polysemy.Error (runError)
import Polysemy.Reader (runReader)
import Submit.CSV
import System.Directory (doesFileExist)
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "live pacman extra database" $ do
    it "loads current extra.db and finds parseable Haskell package metadata" $ do
      extra <- loadLiveExtraDB
      samples <- requireHaskellSamples extra

      forM_ samples $ \(archName, desc@PkgDesc {_version}) -> do
        _name desc `shouldBe` archName
        _version `shouldSatisfy` not . null
        unPackageName (toHackageName archName) `shouldSatisfy` not . null
        archName `shouldSatisfy` isHaskellPackage

    it "renders Hackage distro CSV rows from current extra.db and parses them back" $ do
      extra <- loadLiveExtraDB
      let rows = take 20 $ liveDistroRows extra
      rows `shouldSatisfy` not . null
      parseDistroCSV (renderDistroCSV rows) `shouldBe` rows

  describe "diff dependency range checks" $ do
    it "uses a live extra.db version when checking a Hackage dependency range" $ do
      extra <- loadLiveExtraDB
      (hackageName, expectedVersion) <- requireParseableHackagePackage extra

      case runInRange extra hackageName anyVersion of
        Right (Right (actualName, _, actualVersion, inBounds)) -> do
          actualName `shouldBe` hackageName
          actualVersion `shouldBe` expectedVersion
          inBounds `shouldBe` True
        result ->
          expectationFailure $ "expected successful range check, got: " <> show result

    it "reports a malformed live package version as VersionNoParse instead of crashing" $ do
      extra <- loadLiveExtraDB
      (archName, desc) <- requireHaskellPackage extra
      let hackageName = toHackageName archName
          badVersion = _version desc <> "_not_cabal"
          poisonedExtra = Map.insert archName (desc {_version = badVersion}) extra

      case runInRange poisonedExtra hackageName anyVersion of
        Left (VersionNoParse rawVersion) ->
          rawVersion `shouldBe` badVersion
        result ->
          expectationFailure $ "expected VersionNoParse, got: " <> show result

loadLiveExtraDB :: IO ExtraDB
loadLiveExtraDB = do
  exists <- doesFileExist defaultExtraDBPath
  if exists
    then loadExtraDB defaultExtraDBPath
    else skip $ "pacman database not found: " <> defaultExtraDBPath

requireHaskellSamples :: ExtraDB -> IO [(ArchLinuxName, PkgDesc)]
requireHaskellSamples extra = do
  let samples = take 20 . sortOn (unArchLinuxName . fst) $ haskellPackages extra
  if null samples
    then skip "current extra.db has no Haskell packages"
    else pure samples

requireHaskellPackage :: ExtraDB -> IO (ArchLinuxName, PkgDesc)
requireHaskellPackage extra =
  case listToMaybe $ haskellPackages extra of
    Just sample -> pure sample
    Nothing -> skip "current extra.db has no Haskell packages"

requireParseableHackagePackage :: ExtraDB -> IO (PackageName, Version)
requireParseableHackagePackage extra =
  case listToMaybe $ parseableHaskellPackages extra of
    Just sample -> pure sample
    Nothing -> skip "current extra.db has no Haskell packages with Cabal-style versions"

haskellPackages :: ExtraDB -> [(ArchLinuxName, PkgDesc)]
haskellPackages =
  filter (isHaskellPackage . fst) . Map.toList

parseableHaskellPackages :: ExtraDB -> [(PackageName, Version)]
parseableHaskellPackages =
  mapMaybe
    ( \(archName, PkgDesc {_version}) ->
        case simpleParsec _version of
          Just version -> Just (toHackageName archName, version)
          Nothing -> Nothing
    )
    . haskellPackages

liveDistroRows :: ExtraDB -> DistroCSV
liveDistroRows =
  sortOn
    (\(name, _, _) -> name)
    . fmap toRow
    . haskellPackages
  where
    toRow (archName, PkgDesc {_version}) =
      ( unPackageName hackageName,
        _version,
        "https://archlinux.org/packages/extra/x86_64/" <> packagePath hackageName archName
      )
      where
        hackageName = toHackageName archName

    packagePath hackageName archName
      | isGHCLibs hackageName = "ghc"
      | otherwise = unArchLinuxName archName

runInRange :: ExtraDB -> PackageName -> VersionRange -> Either MyException (Either (PackageName, VersionRange) (PackageName, VersionRange, Version, Bool))
runInRange extra name range =
  run
    . runError @MyException
    . runReader extra
    $ inRange (name, range)

skip :: String -> IO a
skip reason = pendingWith reason >> error "unreachable"
