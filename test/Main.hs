{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import qualified Check as Sync
import qualified Conduit as C
import Control.Exception (bracket, try)
import Control.Monad (forM_, void)
import qualified Data.ByteString.Char8 as B8
import qualified Data.Conduit.Tar as Tar
import Data.List (intercalate, isInfixOf, isPrefixOf, sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe, mapMaybe)
import Diff (inRange)
import Distribution.ArchHs.Exception
import Distribution.ArchHs.ExtraDB (defaultExtraDBPath, loadExtraDB)
import Distribution.ArchHs.Hackage (getCabalIncludingDeprecated, getNewerVersions, loadHackageDBsWithRevisions, loadRawHackageDB, loadRawHackageRevisions)
import Distribution.ArchHs.Name (isGHCLibs, isHaskellPackage, toArchLinuxName, toHackageName)
import Distribution.ArchHs.Options (ParserResult (..), defaultPrefs, execParserPure, info)
import Distribution.ArchHs.PP (AnsiStyle, Doc)
import Distribution.ArchHs.RDepCheck (DepSrc (..), ReverseDep (..))
import Distribution.ArchHs.Types
import qualified Distribution.Hackage.DB.Parsed as Hackage
import qualified Distribution.Hackage.DB.Unparsed as RawHackage
import Distribution.Package (packageName, packageVersion)
import Distribution.PackageDescription (GenericPackageDescription, packageDescription)
import Distribution.Parsec (simpleParsec)
import Distribution.Types.PackageName (PackageName, mkPackageName, unPackageName)
import Distribution.Types.Version (Version)
import Distribution.Types.VersionRange (VersionRange, anyVersion)
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import qualified PlanSpec
import Polysemy (run, runM)
import Polysemy.Error (runError)
import Polysemy.Reader (runReader)
import Polysemy.State (evalState)
import Polysemy.Trace (ignoreTrace)
import qualified RDepCheck
import qualified RDepCheck.Args as RDepArgs
import Submit.CSV
import System.Directory (doesFileExist, getTemporaryDirectory, removeFile)
import System.Exit (ExitCode (..))
import System.IO (hClose, hFlush, openBinaryTempFile, stdout)
import Test.Hspec
import Utils (linkedHaskellPackageDescs)

main :: IO ()
main = hspec $ do
  PlanSpec.spec
  describe "reverse dependency target arguments" $ do
    forM_
      [ (["aeson"], [("aeson", Nothing)]),
        (["aeson", "3.0"], [("aeson", Just "3.0")]),
        (["aeson", "text"], [("aeson", Nothing), ("text", Nothing)]),
        (["aeson", "3.0", "text", "2.1"], [("aeson", Just "3.0"), ("text", Just "2.1")]),
        (["aeson", "text", "2.1"], [("aeson", Nothing), ("text", Just "2.1")]),
        (["aeson", "3.0", "text"], [("aeson", Just "3.0"), ("text", Nothing)])
      ] $ \(args, expected) ->
        it ("parses " <> unwords args) $
          case execParserPure defaultPrefs (info RDepArgs.cmdOptions mempty) args of
            Success (Right opts) ->
              RDepArgs.optTargets opts `shouldBe` [(mkPackageName name, parseVersion <$> version) | (name, version) <- expected]
            _ -> expectationFailure "expected valid targets"
    forM_ [[], ["3.0"], ["aeson", "3.0", "2.1"], ["aeson", "3..0"]] $ \args ->
      it ("rejects " <> show args) $
        case execParserPure defaultPrefs (info RDepArgs.cmdOptions mempty) args of
          Success (Right _) -> expectationFailure "expected invalid targets"
          _ -> pure ()

  describe "combined reverse dependency results" $ do
    forM_
      [ ("accepts coordinated upgrades", Just "<2", Just ">=2", RDepCheck.FailureCounts 0 0),
        ("rejects incompatible candidates", Just "<2", Just "<2", RDepCheck.FailureCounts 1 0),
        ("checks newly added dependencies", Nothing, Just "<2", RDepCheck.FailureCounts 1 0),
        ("does not classify newly introduced failures as old", Just "<2", Just ">=3", RDepCheck.FailureCounts 1 0),
        ("retains existing failure classification", Just "<1", Just "<1", RDepCheck.FailureCounts 0 1),
        ("drops removed dependencies", Just "<2", Nothing, RDepCheck.FailureCounts 0 0)
      ] $ \(label, installedRange, candidateRange, expected) ->
        it label $ do
          let (_, _, baseExtra, target) = syncDepCheckDBs [] [("shared", [Run], "<2")]
              shared = mkPackageName "shared"
              extra = case installedRange of
                Just _ -> baseExtra
                Nothing -> Map.adjust (\desc -> desc {_depends = []}) (toArchLinuxName shared) baseExtra
              cabal name version range = B8.pack $ unlines $
                ["cabal-version: 1.24", "name: " <> name, "version: " <> version, "build-type: Simple"]
                  <> maybe [] (\r -> ["library", "  build-depends: Diff " <> r]) range
              entries =
                [ ("Diff/2.0/Diff.cabal", cabal "Diff" "2.0" Nothing),
                  ("shared/1.0/shared.cabal", cabal "shared" "1.0" installedRange),
                  ("shared/2.0/shared.cabal", cabal "shared" "2.0" candidateRange)
                ]
          withIndexEntries entries $ \path -> do
            (latest, original) <- loadRawHackageRevisions [(target, parseVersion "2.0"), (shared, parseVersion "1.0"), (shared, parseVersion "2.0")] path
            (result, _) <- captureStdout $ runRdepTargets extra latest original [(target, Just $ parseVersion "2.0"), (shared, Just $ parseVersion "2.0")]
            case result of
              Right counts -> counts `shouldBe` expected
              Left err -> expectationFailure $ show err
            -- Omitting a candidate keeps that dependent at its installed version.
            (installedResult, _) <- captureStdout $ runRdepTargets extra latest original [(target, Just $ parseVersion "2.0"), (shared, Nothing)]
            let installedFailures = case installedRange of
                  Nothing -> RDepCheck.FailureCounts 0 0
                  Just "<1" -> RDepCheck.FailureCounts 0 1
                  Just _ -> RDepCheck.FailureCounts 1 0
            case installedResult of
              Right counts -> counts `shouldBe` installedFailures
              Left err -> expectationFailure $ show err

    it "compares candidate revisions and counts only the latest candidate's failures" $ do
      let (_, _, extra, target) = syncDepCheckDBs [] [("shared", [Run], "<2")]
          shared = mkPackageName "shared"
          cabal name version range = B8.pack $ unlines $
            ["cabal-version: 1.24", "name: " <> name, "version: " <> version, "build-type: Simple"]
              <> maybe [] (\r -> ["library", "  build-depends: Diff " <> r]) range
          entries =
            [ ("Diff/2.0/Diff.cabal", cabal "Diff" "2.0" Nothing),
              ("shared/1.0/shared.cabal", cabal "shared" "1.0" $ Just "<2"),
              ("shared/2.0/shared.cabal", cabal "shared" "2.0" $ Just "<2"),
              ("shared/2.0/shared.cabal", cabal "shared" "2.0" $ Just ">=2")
            ]
      withIndexEntries entries $ \path -> do
        (latest, original) <- loadRawHackageRevisions [(target, parseVersion "2.0"), (shared, parseVersion "1.0"), (shared, parseVersion "2.0")] path
        (result, output) <- captureStdout $ runRdepTargets extra latest original [(target, Just $ parseVersion "2.0"), (shared, Just $ parseVersion "2.0")]
        case result of
          Right counts -> counts `shouldBe` RDepCheck.FailureCounts 0 0
          Left err -> expectationFailure $ show err
        output `shouldContain` "latest revision"
        output `shouldContain` "revision 0"
        output `shouldContain` ">=2"
        output `shouldContain` "<2"

    it "prints a shared dependent once and totals each target's failures" $ do
      let (_, _, baseExtra, target) = syncDepCheckDBs [] [("shared", [Run], "<2")]
          other = mkPackageName "other"
          otherArch = toArchLinuxName other
          shared = toArchLinuxName $ mkPackageName "shared"
          extra = Map.adjust (\desc -> desc {_depends = _depends desc <> [PkgDependent otherArch Nothing]}) shared $
            Map.insert otherArch ((baseExtra Map.! toArchLinuxName target) {_name = otherArch}) baseExtra
          cabal = B8.pack $ unlines ["cabal-version: 1.24", "name: shared", "version: 1.0", "build-type: Simple", "library", "  build-depends: Diff <2, other <1"]
          candidate name = B8.pack $ unlines ["cabal-version: 1.24", "name: " <> name, "version: 2.0", "build-type: Simple"]
      withIndexEntries [("shared/1.0/shared.cabal", cabal), ("Diff/2.0/Diff.cabal", candidate "Diff"), ("other/2.0/other.cabal", candidate "other")] $ \path -> do
        (latest, original) <- loadRawHackageRevisions [(mkPackageName "shared", parseVersion "1.0"), (target, parseVersion "2.0"), (other, parseVersion "2.0")] path
        (result, output) <- captureStdout $ runRdepTargets extra latest original [(target, Just $ parseVersion "2.0"), (other, Just $ parseVersion "2.0")]
        case result of
          Right counts -> counts `shouldBe` RDepCheck.FailureCounts 1 1
          Left err -> expectationFailure $ show err
        length (filter (isInfixOf "Reverse dependency:") $ lines output) `shouldBe` 1
        output `shouldContain` "Target: Diff 2.0"
        output `shouldContain` "Target: other 2.0"
        output `shouldContain` "rdep:"
        output `shouldContain` "rdep-old:"

  describe "live pacman extra database" $ do
    it "loads current extra.db and finds parseable Haskell package metadata" $ do
      extra <- loadLiveExtraDB
      samples <- requireHaskellSamples extra

      forM_ samples $ \(archName, desc@PkgDesc {_version}) -> do
        _name desc `shouldBe` archName
        _version `shouldSatisfy` not . null
        unPackageName (toHackageName archName) `shouldSatisfy` not . null
        archName `shouldSatisfy` isHaskellPackage

    it "keeps raw Arch EVR for a real Haskell package with pkgrel" $ do
      extra <- loadLiveExtraDB
      (_, PkgDesc {_version, _rawVersion}) <- requireHaskellPackage extra
      _rawVersion `shouldSatisfy` (_version `isPrefixOf`)
      drop (length _version) _rawVersion `shouldSatisfy` isPkgrelSuffix

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

  describe "Hackage preferred-version handling" $ do
    it "does not report masked versions as newer candidates" $ do
      let (preferred, _, name, version100, version101) = maskedHackageDBs
      assertGetNewerVersions preferred name version100 []
      assertGetNewerVersions preferred name version101 []

    it "can still load an exact masked cabal from the raw Hackage DB" $ do
      let (_, raw, name, _, version101) = maskedHackageDBs
      case runGetCabalIncludingDeprecated raw name version101 of
        Right cabal -> do
          let desc = packageDescription cabal
          packageName desc `shouldBe` name
          packageVersion desc `shouldBe` version101
        Left err ->
          expectationFailure $ "expected masked cabal lookup to succeed, got: " <> show err

  describe "sync with unsupported cabal formats" $ do
    it "lists newer versions without parsing their cabal files" $ do
      let (preferred, _, _, name) = unsupportedHackageDBs
      assertGetNewerVersions preferred name (parseVersion "1.0") [parseVersion "1.1", parseVersion "2.0.0"]

    it "links packages by index name without parsing the latest cabal file" $ do
      let (preferred, _, extra, name) = unsupportedHackageDBs
      linked <- runM . runReader preferred . runReader extra $ linkedHaskellPackageDescs
      [(archName, _version desc, hackageName) | (archName, desc, hackageName) <- linked]
        `shouldBe` [(toArchLinuxName name, "1.0", name)]

    it "finishes a version check with an unsupported latest cabal file" $ do
      let (preferred, _, extra, _) = unsupportedHackageDBs
      result <- runSyncCheck extra preferred Map.empty False False
      show result `shouldBe` "Right ()"

    it "continues dependency checking past an unsupported cabal file" $ do
      let (preferred, raw, extra, _) = unsupportedHackageDBs
      result <- runSyncCheck extra preferred raw False True
      show result `shouldBe` "Right ()"

  describe "sync GHC-provided packages" $ do
    forM_ [False, True] $ \depCheck -> do
      it ("excludes bundled tools and libraries by default, depcheck=" <> show depCheck) $ do
        let (hackage, raw, extra) = syncGHCDBs
        (result, output) <- captureStdout $ runSyncCheck extra hackage raw False depCheck
        show result `shouldBe` "Right ()"
        forM_ ["haskell-hsc2hs", "haskell-haddock", "haskell-bundled-library", "haskell-base"] $ \package ->
          output `shouldNotContain` package
        output `shouldContain` "haskell-standalone"

      it ("includes bundled tools and libraries with show-ghc-libs, depcheck=" <> show depCheck) $ do
        let (hackage, raw, extra) = syncGHCDBs
        (result, output) <- captureStdout $ runSyncCheck extra hackage raw True depCheck
        show result `shouldBe` "Right ()"
        forM_ ["haskell-hsc2hs", "haskell-haddock", "haskell-bundled-library", "haskell-base", "haskell-standalone"] $ \package ->
          output `shouldContain` package

      it ("still checks hsc2hs when it is not provided by GHC, depcheck=" <> show depCheck) $ do
        let (hackage, raw, extra) = syncGHCDBs
        (result, output) <- captureStdout $ runSyncCheck (Map.delete (ArchLinuxName "ghc") extra) hackage raw False depCheck
        show result `shouldBe` "Right ()"
        output `shouldContain` "haskell-hsc2hs"

  describe "sync reverse dependency failure classification" $ do
    it "counts newly broken and already unmet ranges separately" $ do
      output <- runSyncDepCheck True [] [("new", [Run], "<2"), ("old", [Run], "<1")]
      output `shouldContain` "2.0 (blocked: rdep=1, rdep-old=1)"
      output `shouldContain` "rdep: haskell-new Depends requires <2"
      output `shouldContain` "rdep-old: haskell-old Depends requires <1"
      output `shouldNotContain` "rdep: haskell-old"
      output `shouldNotContain` "rdep-old: haskell-new"

    it "marks candidates with only existing failures differently from new blockers" $ do
      output <- runSyncDepCheck False [] [("old", [Run], "<1")]
      output `shouldContain` "1.1 (existing: rdep-old=1)"
      output `shouldContain` "2.0 (existing: rdep-old=1)"
      output `shouldNotContain` "blocked:"
      output `shouldNotContain` "(ok)"
      output `shouldNotContain` "rdep-old:"

    it "stops counting an existing failure once the candidate satisfies its range" $ do
      output <- runSyncDepCheck False [] [("recovered", [Run], ">=2")]
      output `shouldContain` "1.1 (existing: rdep-old=1)"
      output `shouldContain` "2.0 (ok)"
      output `shouldContain` "3.0 (ok)"

    it "compares every candidate against the installed version" $ do
      output <- runSyncDepCheck False [] [("new", [Run], "<2")]
      output `shouldContain` "1.1 (ok)"
      output `shouldContain` "2.0 (blocked: rdep=1)"
      output `shouldContain` "3.0 (blocked: rdep=1)"
      output `shouldNotContain` "rdep-old="

    it "classifies each dependency source separately and counts failing ranges" $ do
      output <- runSyncDepCheck True [] [("both", [Run], "<2"), ("both", [Make, Check], "<1")]
      output `shouldContain` "2.0 (blocked: rdep=1, rdep-old=2)"
      output `shouldContain` "rdep: haskell-both Depends requires <2"
      output `shouldContain` "rdep-old: haskell-both MakeDepends requires <1"
      output `shouldContain` "rdep-old: haskell-both CheckDepends requires <1"

    it "keeps direct dependency failures blocking alongside existing reverse failures" $ do
      output <- runSyncDepCheck False ["missing >=1"] [("old", [Run], "<1")]
      output `shouldContain` "2.0 (blocked: dep=1, rdep-old=1)"

  describe "standalone reverse dependency failure classification" $ do
    it "marks and counts new and existing failures separately for each source" $ do
      let reverseDep =
            ReverseDep (toArchLinuxName $ mkPackageName "both")
              [(Run, parseRange "<2"), (Make, parseRange "<1"), (Check, parseRange "<1")]
          (doc, counts) = RDepCheck.checkReverseDep (Just (parseVersion "1.0", parseVersion "2.0")) reverseDep
      counts `shouldBe` RDepCheck.FailureCounts 1 2
      show doc `shouldContain` "rdep: 2.0 is outside Depends range (<2)"
      show doc `shouldContain` "rdep-old: 2.0 is outside MakeDepends range (<1)"
      show doc `shouldContain` "rdep-old: 2.0 is outside CheckDepends range (<1)"

    it "uses the current extra version and totals failures across reverse dependencies" $ do
      let (_, raw, extra, name) = syncDepCheckDBs [] [("new", [Run], "<2"), ("old", [Make, Check], "<1")]
      result <- runRdepCheck extra raw (Just $ parseVersion "2.0") name
      case result of
        Right counts -> counts `shouldBe` RDepCheck.FailureCounts 1 2
        Left err -> expectationFailure $ show err

    it "drops existing failures when the candidate satisfies the range" $ do
      let (_, raw, extra, name) = syncDepCheckDBs [] [("recovered", [Run], ">=2")]
      result <- runRdepCheck extra raw (Just $ parseVersion "2.0") name
      case result of
        Right counts -> counts `shouldBe` RDepCheck.FailureCounts 0 0
        Left err -> expectationFailure $ show err

    it "lists ranges without checking or parsing the current version when no candidate is given" $ do
      let (_, raw, extra, name) = syncDepCheckDBs [] [("old", [Run], "<1")]
          badExtra = Map.adjust (\desc -> desc {_version = "not-a-version"}) (toArchLinuxName name) extra
          reverseDep = ReverseDep (toArchLinuxName $ mkPackageName "old") [(Run, parseRange "<1")]
          (doc, counts) = RDepCheck.checkReverseDep Nothing reverseDep
      counts `shouldBe` RDepCheck.FailureCounts 0 0
      show doc `shouldContain` "Depends: <1"
      show doc `shouldNotContain` "rdep:"
      show doc `shouldNotContain` "rdep-old:"
      result <- runRdepCheck badExtra raw Nothing name
      case result of
        Right actual -> actual `shouldBe` RDepCheck.FailureCounts 0 0
        Left err -> expectationFailure $ show err

    it "reports an unparseable current version instead of guessing the failure classification" $ do
      let (_, raw, extra, name) = syncDepCheckDBs [] [("old", [Run], "<1")]
          badExtra = Map.adjust (\desc -> desc {_version = "not-a-version"}) (toArchLinuxName name) extra
      result <- runRdepCheck badExtra raw (Just $ parseVersion "2.0") name
      case result of
        Left (VersionNoParse version) -> version `shouldBe` "not-a-version"
        other -> expectationFailure $ "expected VersionNoParse, got: " <> show other

    it "exits unsuccessfully only for newly unmet ranges" $ do
      forM_
        [ (RDepCheck.FailureCounts 0 0, Right ()),
          (RDepCheck.FailureCounts 0 2, Right ()),
          (RDepCheck.FailureCounts 1 0, Left $ ExitFailure 1),
          (RDepCheck.FailureCounts 1 2, Left $ ExitFailure 1)
        ]
        $ \(counts, expected) -> do
          result <- try @ExitCode $ RDepCheck.printRdepcheckResult $ pure $ Right counts
          result `shouldBe` expected

  describe "Hackage index revision loading" $ do
    it "retains first cabal files while preserving latest metadata and preferred versions" $ do
      let name = mkPackageName "sample"
          version = parseVersion "1.0"
          original = B8.pack $ unlines ["cabal-version: 1.24", "name: sample", "version: 1.0", "library", "  build-depends: base <5"]
          latest = B8.pack $ unlines ["cabal-version: 1.24", "name: sample", "version: 1.0", "library", "  build-depends: base <6"]
          unchanged = B8.pack $ unlines ["cabal-version: 1.24", "name: sample", "version: 2.0"]
          entries =
            [ ("sample/1.0/package.json", B8.pack "{}"),
              ("sample/1.0/sample.cabal", original),
              ("sample/preferred-versions", B8.pack "sample >=2"),
              ("sample/2.0/sample.cabal", unchanged),
              ("sample/1.0/sample.cabal", latest),
              ("sample/1.0/package.json", B8.pack "{ }"),
              ("sample/preferred-versions", B8.pack "sample <2"),
              ("metadata-only/1.0/package.json", B8.pack "{}")
            ]
          getVersion raw release = RawHackage.versions (raw Map.! name) Map.! release
      withIndexEntries entries $ \path -> do
        expected <- loadRawHackageDB path
        (preferred, raw, revision0) <- loadHackageDBsWithRevisions path
        raw `shouldBe` expected
        Map.keys (preferred Map.! name) `shouldBe` [version]
        RawHackage.cabalFile (getVersion raw version) `shouldBe` latest
        RawHackage.cabalFile (getVersion revision0 version) `shouldBe` original
        RawHackage.metaFile (getVersion revision0 version) `shouldBe` B8.pack "{ }"
        RawHackage.preferredVersions (revision0 Map.! name) `shouldBe` B8.pack "sample <2"
        getVersion revision0 (parseVersion "2.0") `shouldBe` getVersion raw (parseVersion "2.0")
        revision0 Map.! mkPackageName "metadata-only" `shouldBe` raw Map.! mkPackageName "metadata-only"

    it "does not mistake an empty revision 0 for an unseen cabal file" $ do
      let name = mkPackageName "sample"
          version = parseVersion "1.0"
          latest = B8.pack $ unlines ["cabal-version: 1.24", "name: sample", "version: 1.0"]
      withIndexEntries [("sample/1.0/sample.cabal", B8.empty), ("sample/1.0/sample.cabal", latest)] $ \path -> do
        (_, raw, revision0) <- loadHackageDBsWithRevisions path
        RawHackage.cabalFile (RawHackage.versions (raw Map.! name) Map.! version) `shouldBe` latest
        RawHackage.cabalFile (RawHackage.versions (revision0 Map.! name) Map.! version) `shouldBe` B8.empty

  describe "reverse dependency revision comparisons" $ do
    it "loads the first and last index entries for exact versions, including deprecated releases" $ do
      let name = mkPackageName "revised"
          version = parseVersion "1.0"
          cabal range = B8.pack $ unlines ["cabal-version: 1.24", "name: revised", "version: 1.0", "build-type: Simple", "library", "  build-depends: Diff " <> range]
          entries =
            [ ("revised/1.0/revised.cabal", cabal "<2"),
              ("revised/preferred-versions", B8.pack "revised <1"),
              ("revised/1.0/revised.cabal", cabal "<3"),
              ("revised/2.0/revised.cabal", B8.pack "unrequested version"),
              ("revised/1.0/revised.cabal", cabal "<1")
            ]
          (_, _, extra, target) = syncDepCheckDBs [] [("revised", [Run], "<1")]
      withIndexEntries entries $ \path -> do
        (latest, original) <- loadRawHackageRevisions [(name, version)] path
        result <- runRdepCheckRevisions extra latest original (Just $ parseVersion "2.0") target
        case result of
          Right counts -> counts `shouldBe` RDepCheck.FailureCounts 0 1
          Left err -> expectationFailure $ show err
        originalResult <- runRdepCheck extra original (Just $ parseVersion "2.0") target
        case originalResult of
          Right counts -> counts `shouldBe` RDepCheck.FailureCounts 1 0
          Left err -> expectationFailure $ show err
        case runGetCabalIncludingDeprecated latest name (parseVersion "2.0") of
          Left (VersionNotFound _ _) -> pure ()
          other -> expectationFailure $ "expected unrequested version to be absent, got: " <> show other

    it "shows both ranges and their new/old classifications while returning latest counts" $ do
      let (doc, counts) = revisionComparison (Just $ parseVersion "2.0") "<1" "<2"
      counts `shouldBe` RDepCheck.FailureCounts 0 1
      show doc `shouldContain` "latest revision (rdep=0, rdep-old=1):"
      show doc `shouldContain` "revision 0 (rdep=1, rdep-old=0):"
      show doc `shouldContain` "rdep-old: 2.0 is outside Depends range (<1)"
      show doc `shouldContain` "rdep: 2.0 is outside Depends range (<2)"

    it "shows when a revision changes whether the candidate is accepted" $ do
      let (doc, counts) = revisionComparison (Just $ parseVersion "2.0") "<2" "<3"
      counts `shouldBe` RDepCheck.FailureCounts 1 0
      show doc `shouldContain` "latest revision (rdep=1, rdep-old=0):"
      show doc `shouldContain` "revision 0 (rdep=0, rdep-old=0):"
      show doc `shouldContain` "Depends: <3"

    it "does not duplicate equal or equivalent dependency ranges" $ do
      forM_ ["<2", ">=0 && <2"] $ \original -> do
        let (doc, counts) = revisionComparison (Just $ parseVersion "2.0") "<2" original
        counts `shouldBe` RDepCheck.FailureCounts 1 0
        show doc `shouldContain` "Depends: <2"
        show doc `shouldNotContain` "latest revision"
        show doc `shouldNotContain` "revision 0"

    it "shows changed ranges when listing without a candidate version" $ do
      let (doc, counts) = revisionComparison Nothing "<2" "<3"
      counts `shouldBe` RDepCheck.FailureCounts 0 0
      show doc `shouldContain` "latest revision:"
      show doc `shouldContain` "revision 0:"
      show doc `shouldContain` "Depends: <2"
      show doc `shouldContain` "Depends: <3"
      show doc `shouldNotContain` "rdep="

    it "shows the available result if either revision cannot be parsed" $ do
      let name = mkPackageName "revised"
          archName = toArchLinuxName name
          parsed = Right $ ReverseDep archName [(Run, parseRange "<2")]
          failed = Left $ CabalNoParse name $ parseVersion "1.0"
          versions = Just (parseVersion "1.0", parseVersion "2.0")
      forM_ [(parsed, failed, RDepCheck.FailureCounts 1 0), (failed, parsed, RDepCheck.FailureCounts 0 0)] $
        \(latest, original, expected) -> do
          let (doc, counts) = RDepCheck.checkReverseDepRevisions versions archName latest original
          counts `shouldBe` expected
          show doc `shouldContain` "latest revision"
          show doc `shouldContain` "revision 0"
          show doc `shouldContain` "unchecked: Unable to parse"
          show doc `shouldContain` "rdep: 2.0 is outside Depends range (<2)"

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

isPkgrelSuffix :: String -> Bool
isPkgrelSuffix ('-' : rest) = not (null rest)
isPkgrelSuffix _ = False

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

maskedHackageDBs :: (Hackage.HackageDB, RawHackage.HackageDB, PackageName, Version, Version)
maskedHackageDBs =
  (Hackage.parseDB raw, raw, name, version100, version101)
  where
    name = mkPackageName "masked"
    version100 = parseVersion "1.0.0"
    version101 = parseVersion "1.0.1"
    raw =
      Map.singleton
        name
        ( RawHackage.PackageData
            (B8.pack "masked <1.0.1 || >1.0.1")
            ( Map.fromList
                [ (version100, RawHackage.VersionData (cabalFile "1.0.0") (B8.pack "{}")),
                  (version101, RawHackage.VersionData (cabalFile "1.0.1") (B8.pack "{}"))
                ]
            )
        )

    cabalFile version =
      B8.pack $
        unlines
          [ "cabal-version: 1.12",
            "name: masked",
            "version: " <> version,
            "build-type: Simple"
          ]

parseVersion :: String -> Version
parseVersion raw =
  case simpleParsec raw of
    Just version -> version
    Nothing -> error $ "test fixture version does not parse: " <> raw

parseRange :: String -> VersionRange
parseRange raw =
  case simpleParsec raw of
    Just range -> range
    Nothing -> error $ "test fixture range does not parse: " <> raw

runGetNewerVersions :: Hackage.HackageDB -> PackageName -> Version -> Either MyException [Version]
runGetNewerVersions hackage name version =
  run
    . runError @MyException
    . runReader hackage
    $ getNewerVersions name version

assertGetNewerVersions :: Hackage.HackageDB -> PackageName -> Version -> [Version] -> Expectation
assertGetNewerVersions hackage name version expected =
  case runGetNewerVersions hackage name version of
    Right actual -> actual `shouldBe` expected
    Left err -> expectationFailure $ "expected newer versions, got: " <> show err

runGetCabalIncludingDeprecated :: RawHackage.HackageDB -> PackageName -> Version -> Either MyException GenericPackageDescription
runGetCabalIncludingDeprecated hackage name version =
  run
    . runError @MyException
    . runReader hackage
    $ getCabalIncludingDeprecated name version

unsupportedHackageDBs :: (Hackage.HackageDB, RawHackage.HackageDB, ExtraDB, PackageName)
unsupportedHackageDBs =
  (Hackage.parseDB raw, raw, Map.singleton archName desc, name)
  where
    name = mkPackageName "Diff"
    archName = toArchLinuxName name
    raw =
      Map.singleton name $
        RawHackage.PackageData
          (B8.pack "Diff <3")
          ( Map.fromList
              [ (parseVersion "1.1", versionData "1.12" "1.1"),
                (parseVersion "2.0.0", versionData "999.0" "2.0.0"),
                (parseVersion "3.0", versionData "999.0" "3.0")
              ]
          )

    -- Use a future format so the regression survives upgrades of Cabal itself.
    versionData format version =
      RawHackage.VersionData
        ( B8.pack $
            unlines
              [ "cabal-version: " <> format,
                "name: Diff",
                "version: " <> version,
                "build-type: Simple"
              ]
        )
        B8.empty

    desc =
      PkgDesc
        { _name = archName,
          _version = "1.0",
          _rawVersion = "1.0-1",
          _desc = "Diff algorithm in pure Haskell",
          _url = Nothing,
          _provides = [],
          _optDepends = [],
          _replaces = [],
          _conflicts = [],
          _depends = [],
          _makeDepends = [],
          _checkDepends = []
        }

syncGHCDBs :: (Hackage.HackageDB, RawHackage.HackageDB, ExtraDB)
syncGHCDBs = (Hackage.parseDB raw, raw, extra)
  where
    packages = ["hsc2hs", "haddock", "bundled-library", "base", "standalone"]
    raw = Map.fromList
      [ ( mkPackageName package,
          RawHackage.PackageData B8.empty $ Map.singleton (parseVersion "1.1") $
            RawHackage.VersionData
              (B8.pack $ unlines ["cabal-version: 1.12", "name: " <> package, "version: 1.1", "build-type: Simple"])
              B8.empty
        )
        | package <- packages
      ]
    extra = Map.fromList $
      [ (archName, desc archName []) | package <- packages, let archName = toArchLinuxName $ mkPackageName package ]
        <> [ ( archName,
               desc archName [PkgDependent (toArchLinuxName $ mkPackageName package) (Just "1.0") | package <- bundled]
             )
             | (provider, bundled) <- [("ghc", ["hsc2hs", "haddock"]), ("ghc-libs", ["bundled-library"])],
               let archName = ArchLinuxName provider
           ]
    desc package provides =
      PkgDesc
        { _name = package,
          _version = "1.0",
          _rawVersion = "1.0-1",
          _desc = "GHC-provided package fixture",
          _url = Nothing,
          _provides = provides,
          _optDepends = [],
          _replaces = [],
          _conflicts = [],
          _depends = [],
          _makeDepends = [],
          _checkDepends = []
        }

runSyncCheck :: ExtraDB -> Hackage.HackageDB -> RawHackage.HackageDB -> Bool -> Bool -> IO (Either MyException ())
runSyncCheck extra hackage raw includeGHC depCheck =
  runM
    . runError @MyException
    . evalState (Map.empty :: Map.Map PackageName [VersionRange])
    . ignoreTrace
    . runReader (Map.empty :: FlagAssignments)
    . runReader (parseVersion "9.6.6")
    . runReader raw
    . runReader hackage
    . runReader extra
    $ Sync.check includeGHC depCheck True

runRdepCheck :: ExtraDB -> RawHackage.HackageDB -> Maybe Version -> PackageName -> IO (Either MyException RDepCheck.FailureCounts)
runRdepCheck extra raw = runRdepCheckRevisions extra raw raw

runRdepCheckRevisions :: ExtraDB -> RawHackage.HackageDB -> RawHackage.HackageDB -> Maybe Version -> PackageName -> IO (Either MyException RDepCheck.FailureCounts)
runRdepCheckRevisions extra latest original version name =
  runRdepTargets extra latest original [(name, version)]

runRdepTargets :: ExtraDB -> RawHackage.HackageDB -> RawHackage.HackageDB -> [(PackageName, Maybe Version)] -> IO (Either MyException RDepCheck.FailureCounts)
runRdepTargets extra latest original targets =
  runM
    . runError @MyException
    . evalState (Map.empty :: Map.Map PackageName [VersionRange])
    . ignoreTrace
    . runReader (Map.empty :: FlagAssignments)
    . runReader (parseVersion "9.6.6")
    . runReader latest
    . runReader extra
    $ RDepCheck.checkTargets original targets

captureStdout :: IO a -> IO (a, String)
captureStdout action = do
  tmp <- getTemporaryDirectory
  bracket (openBinaryTempFile tmp "arch-hs-output") (\(path, handle) -> hClose handle >> removeFile path) $ \(path, handle) -> do
    result <- bracket (hDuplicate stdout) (\saved -> hFlush stdout >> hDuplicateTo saved stdout >> hClose saved) $ \_ -> do
      hDuplicateTo handle stdout
      action
    hClose handle
    output <- B8.readFile path
    pure (result, B8.unpack output)

revisionComparison :: Maybe Version -> String -> String -> (Doc AnsiStyle, RDepCheck.FailureCounts)
revisionComparison candidate latest original =
  RDepCheck.checkReverseDepRevisions
    ((\version -> (parseVersion "1.0", version)) <$> candidate)
    name
    (Right $ ReverseDep name [(Run, parseRange latest)])
    (Right $ ReverseDep name [(Run, parseRange original)])
  where
    name = toArchLinuxName $ mkPackageName "revised"

withIndexEntries :: [(FilePath, B8.ByteString)] -> (FilePath -> IO a) -> IO a
withIndexEntries entries action = do
  tmp <- getTemporaryDirectory
  bracket (openBinaryTempFile tmp "arch-hs-index.tar") (\(path, handle) -> hClose handle >> removeFile path) $ \(path, handle) -> do
    hClose handle
    C.runConduitRes $
      forM_ entries
        ( \(entryPath, bytes) -> do
            C.yield $ Left $
              Tar.FileInfo
                { Tar.filePath = B8.pack entryPath,
                  Tar.fileUserId = 0,
                  Tar.fileUserName = B8.empty,
                  Tar.fileGroupId = 0,
                  Tar.fileGroupName = B8.empty,
                  Tar.fileMode = 0o644,
                  Tar.fileSize = fromIntegral $ B8.length bytes,
                  Tar.fileType = Tar.FTNormal,
                  Tar.fileModTime = 0
                }
            C.yield $ Right bytes
        )
        C..| void Tar.tar
        C..| C.sinkFile path
    action path

runSyncDepCheck :: Bool -> [String] -> [(String, [DepSrc], String)] -> IO String
runSyncDepCheck verbose deps reverseDeps = do
  let (hackage, raw, extra, name) = syncDepCheckDBs deps reverseDeps
      currentVersion = parseVersion "1.0"
  result <-
    runM
      . runError @MyException
      . evalState (Map.empty :: Map.Map PackageName [VersionRange])
      . ignoreTrace
      . runReader (Map.empty :: FlagAssignments)
      . runReader (parseVersion "9.6.6")
      . runReader raw
      . runReader hackage
      . runReader extra
      $ do
        versions <- getNewerVersions name currentVersion
        (checked, skipped) <- Sync.checkNewerVersions True name currentVersion versions
        pure (show $ Sync.prettyNewerVersions verbose (toArchLinuxName name) "1.0-7" name currentVersion checked, length skipped)
  case result of
    Right (output, skipped) -> do
      skipped `shouldBe` 0
      pure output
    Left err -> do
      expectationFailure $ "expected dependency check to succeed, got: " <> show err
      pure ""

syncDepCheckDBs :: [String] -> [(String, [DepSrc], String)] -> (Hackage.HackageDB, RawHackage.HackageDB, ExtraDB, PackageName)
syncDepCheckDBs deps reverseDeps =
  (Hackage.parseDB raw, raw, extra, name)
  where
    name = mkPackageName "Diff"
    archName = toArchLinuxName name
    grouped = Map.fromListWith (<>) [(rdep, [(sources, range)]) | (rdep, sources, range) <- reverseDeps]
    raw =
      Map.fromList $
        (name, packageData [(version, candidate) | version <- ["1.1", "2.0", "3.0"]] "Diff")
          : [(mkPackageName rdep, packageData [("1.0", components ranges)] rdep) | (rdep, ranges) <- Map.toList grouped]

    packageData versions package =
      RawHackage.PackageData B8.empty . Map.fromList $
        [ ( parseVersion version,
            RawHackage.VersionData
              (B8.pack $ unlines $ ["cabal-version: 1.24", "name: " <> package, "version: " <> version, "build-type: Simple"] <> body)
              B8.empty
          )
          | (version, body) <- versions
        ]

    candidate =
      if null deps
        then []
        else ["library", "  build-depends: " <> intercalate ", " deps]

    components ranges =
      concat
        [ body
          | (sources, range) <- ranges,
            (include, body) <-
              [ (Run `elem` sources, ["library", "  build-depends: Diff " <> range]),
                (Make `elem` sources || Check `elem` sources, ["custom-setup", "  setup-depends: Diff " <> range])
              ],
            include
        ]

    extra =
      Map.fromList $
        (archName, desc archName)
          : [ ( rdepName,
                (desc rdepName)
                  { _depends = [PkgDependent archName Nothing | any (elem Run . fst) ranges],
                    _makeDepends = [PkgDependent archName Nothing | any (elem Make . fst) ranges],
                    _checkDepends = [PkgDependent archName Nothing | any (elem Check . fst) ranges]
                  }
              )
              | (rdep, ranges) <- Map.toList grouped,
                let rdepName = toArchLinuxName $ mkPackageName rdep
            ]

    desc package =
      PkgDesc
        { _name = package,
          _version = "1.0",
          _rawVersion = "1.0-7",
          _desc = "Dependency check fixture",
          _url = Nothing,
          _provides = [],
          _optDepends = [],
          _replaces = [],
          _conflicts = [],
          _depends = [],
          _makeDepends = [],
          _checkDepends = []
        }

skip :: String -> IO a
skip reason = pendingWith reason >> error "unreachable"
