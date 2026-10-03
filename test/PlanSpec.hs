{-# LANGUAGE TypeApplications #-}

module PlanSpec (spec) where

import Control.Monad (forM_)
import qualified Data.ByteString.Char8 as B8
import Data.Either (isLeft)
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import Distribution.ArchHs.Exception
import Distribution.ArchHs.Name (toArchLinuxName)
import Distribution.ArchHs.Options (ParserResult (..), defaultPrefs, execParserPure, info)
import Distribution.ArchHs.Types
import qualified Distribution.Hackage.DB.Parsed as Hackage
import qualified Distribution.Hackage.DB.Unparsed as RawHackage
import Distribution.Parsec (simpleParsec)
import Distribution.Types.Flag (mkFlagAssignment, mkFlagName)
import Distribution.Types.PackageName (PackageName, mkPackageName)
import Distribution.Types.Version (Version)
import Distribution.Types.VersionRange (VersionRange)
import qualified Plan
import qualified Plan.Args as Args
import qualified Plan.Toolchain as Toolchain
import Polysemy (runM)
import Polysemy.Error (runError)
import Polysemy.Reader (runReader)
import Polysemy.State (evalState)
import Polysemy.Trace (ignoreTrace)
import Test.Hspec

spec :: Spec
spec = describe "coordinated update planner" $ do
  it "parses solve mode with per-package minimum versions" $
    case execParserPure defaultPrefs (info Args.cmdOptions mempty) ["--solve", "alpha", "2.0", "bravo"] of
      Success (Right options) -> do
        Args.optSolve options `shouldBe` True
        Args.optTargets options `shouldBe` [(name "alpha", Just $ version "2.0"), (name "bravo", Nothing)]
      _ -> expectationFailure "expected planner arguments to parse"

  forM_ [[], ["2.0"], ["alpha", "2..0"]] $ \args ->
    it ("rejects malformed arguments " <> show args) $
      case execParserPure defaultPrefs (info Args.cmdOptions mempty) args of
        Success (Right _) -> expectationFailure "expected malformed arguments to fail"
        _ -> pure ()

  describe "rebuild command" $ do
    it "prints a copyable command after the commit message using Arch package bases" $ do
      result <- runPlan False [("zeta", Just "2.0"), ("Alpha", Just "2.0")]
        [("zeta", [], [("2.0", [])]), ("Alpha", [], [("2.0", [])])]
      let output = show $ Plan.prettyPlanResult result
      output `shouldContain` "Commit message:\nAlpha 2.0, zeta 2.0\n\ngenrebuild -H haskell-alpha haskell-zeta"
      last (lines output) `shouldBe` "genrebuild -H haskell-alpha haskell-zeta"

    it "respects package name presets without adding a haskell prefix" $ do
      result <- runPlan False [("stack", Just "2.0"), ("elm-compiler", Just "2.0")]
        [("stack", [], [("2.0", [])]), ("elm-compiler", [], [("2.0", [])])]
      last (lines $ show $ Plan.prettyPlanResult result) `shouldBe` "genrebuild -H elm-compiler stack"

    it "includes requested packages whose versions are unchanged" $ do
      result <- runPlan False [("alpha", Just "2.0"), ("bravo", Just "1.0")]
        [("alpha", [], [("2.0", [])]), ("bravo", [], [])]
      show (Plan.prettyPlanResult result) `shouldContain` "Commit message:\nalpha 2.0\n\ngenrebuild -H haskell-alpha haskell-bravo"

    it "omits the command when there is no commit message" $ do
      result <- runPlan False [("alpha", Just "1.0")] [("alpha", [], [])]
      let output = show $ Plan.prettyPlanResult result
      output `shouldNotContain` "Commit message:"
      output `shouldNotContain` "genrebuild"

  describe "revision comparisons" $ do
    it "shows original candidate failures without changing a successful latest plan" $ do
      let specs range = [("alpha", [], [("2.0", lib ["bravo " <> range])]), ("bravo", [], [("2.0", [])])]
      result <- runRevisionPlan False [("alpha", Just "2.0"), ("bravo", Just "2.0")] (specs "<3") (specs "<2")
      assertWorking result [("alpha", "2.0"), ("bravo", "2.0")]
      let output = show $ Plan.prettyPlanResult result
      output `shouldContain` "Revision comparison: alpha 2.0"
      output `shouldContain` "latest revision: <3 (ok)"
      output `shouldContain` "revision 0: <2"
      output `shouldContain` "dep: alpha requires bravo <2"
      output `shouldContain` "Commit message:\nalpha 2.0, bravo 2.0\n\ngenrebuild -H haskell-alpha haskell-bravo"
      last (lines output) `shouldBe` "genrebuild -H haskell-alpha haskell-bravo"

    it "keeps latest candidate failures blocking even when revision 0 accepts the set" $ do
      let specs range = [("alpha", [], [("2.0", lib ["bravo " <> range])]), ("bravo", [], [("2.0", [])])]
      result <- runRevisionPlan False [("alpha", Just "2.0"), ("bravo", Just "2.0")] (specs "<2") (specs "<3")
      assertBlocked result "dep: alpha requires bravo <2"
      show (Plan.prettyPlanResult result) `shouldContain` "revision 0: <3 (ok)"

    it "compares installed reverse dependents against the chosen target versions" $ do
      let specs range = [("alpha", [], [("2.0", [])]), ("consumer", ["alpha"], [("1.0", lib ["alpha " <> range])])]
      result <- runRevisionPlan False [("alpha", Just "2.0")] (specs "<3") (specs "<2")
      assertWorking result [("alpha", "2.0")]
      let output = show $ Plan.prettyPlanResult result
      output `shouldContain` "Revision comparison: consumer 1.0"
      output `shouldContain` "latest revision: <3 (ok)"
      output `shouldContain` "rdep: haskell-consumer Depends requires alpha <2"

    it "uses each revision's installed baseline to distinguish existing failures" $ do
      let specs baseline =
            [("alpha", ["bravo"], [("1.0", lib ["bravo " <> baseline]), ("2.0", lib ["bravo <2"])]), ("bravo", [], [("2.0", [])])]
      result <- runRevisionPlan False [("alpha", Just "2.0"), ("bravo", Just "2.0")] (specs "<1") (specs "<2")
      assertWorking result [("alpha", "2.0"), ("bravo", "2.0")]
      length (Plan.planWarnings result) `shouldBe` 1
      length (Plan.planRevisionNotes result) `shouldBe` 1
      let output = show $ Plan.prettyPlanResult result
      output `shouldContain` "dep-old: alpha requires bravo <2"
      output `shouldContain` "dep: alpha requires bravo <2"

    it "shows revision-added upper bounds already exceeded by existing dependencies as warnings" $ do
      let specs range =
            [("alpha", ["bravo"], [("1.0", lib ["bravo >=0"]), ("1.1", lib ["bravo " <> range]), ("2.0", [])]), ("bravo", [], [])]
      result <- runRevisionPlan True [("alpha", Nothing)] (specs "<1") (specs ">=0")
      assertWorking result [("alpha", "1.1")]
      length (Plan.planWarnings result) `shouldBe` 1
      length (Plan.planRevisionNotes result) `shouldBe` 1
      let output = show $ Plan.prettyPlanResult result
      output `shouldContain` "latest revision: <1"
      output `shouldContain` "dep-old: alpha requires bravo <1"
      output `shouldContain` "revision 0: >=0 (ok)"

    it "does not duplicate semantically equivalent ranges" $ do
      let specs range = [("alpha", [], [("2.0", lib ["bravo " <> range])]), ("bravo", [], [])]
      result <- runRevisionPlan False [("alpha", Just "2.0")] (specs "<3") (specs ">=0 && <3")
      assertWorking result [("alpha", "2.0")]
      length (Plan.planRevisionNotes result) `shouldBe` 0

    forM_
      [ ("passing", "<3", "<4", True),
        ("blocking", ">=3", ">=4", False),
        ("warning", "<1", "<0.5", True)
      ] $ \(outcome, latest, original, ready) ->
        it ("hides different candidate ranges with the same " <> outcome <> " outcome") $ do
          let specs range =
                [("alpha", ["bravo"], [("1.0", lib ["bravo >=0"]), ("2.0", lib ["bravo " <> range])]), ("bravo", [], [("2.0", [])])]
          result <- runRevisionPlan False [("alpha", Just "2.0"), ("bravo", Just "2.0")] (specs latest) (specs original)
          Plan.planIsReady result `shouldBe` ready
          length (Plan.planRevisionNotes result) `shouldBe` 0
          show (Plan.prettyPlanResult result) `shouldNotContain` "Revision comparison:"

    forM_ [("<3", "<4", True), ("<2", "<1.5", False)] $ \(latest, original, ready) ->
      it ("hides reverse-dependency ranges with unchanged outcomes: " <> latest <> " / " <> original) $ do
        let specs range = [("alpha", [], [("2.0", [])]), ("consumer", ["alpha"], [("1.0", lib ["alpha " <> range])])]
        result <- runRevisionPlan False [("alpha", Just "2.0")] (specs latest) (specs original)
        Plan.planIsReady result `shouldBe` ready
        length (Plan.planRevisionNotes result) `shouldBe` 0

    forM_ [False, True] $ \added ->
      it ("hides passing dependencies " <> if added then "added by a revision" else "removed by a revision") $ do
        let absent = [("alpha", [], [("2.0", [])]), ("bravo", [], [])]
            present = [("alpha", [], [("2.0", lib ["bravo >=1"])]), ("bravo", [], [])]
            (latest, original) = if added then (present, absent) else (absent, present)
        result <- runRevisionPlan False [("alpha", Just "2.0")] latest original
        assertWorking result [("alpha", "2.0")]
        length (Plan.planRevisionNotes result) `shouldBe` 0

    it "shows only outcome-changing dependencies in a mixed comparison" $ do
      let specs bravo charlie =
            [("alpha", [], [("2.0", lib ["bravo " <> bravo, "charlie " <> charlie])]), ("bravo", [], [("2.0", [])]), ("charlie", [], [("2.0", [])])]
      result <- runRevisionPlan False [("alpha", Just "2.0"), ("bravo", Just "2.0"), ("charlie", Just "2.0")]
        (specs "<3" "<3") (specs "<2" "<4")
      assertWorking result [("alpha", "2.0"), ("bravo", "2.0"), ("charlie", "2.0")]
      let output = show $ Plan.prettyPlanResult result
      output `shouldContain` "Revision comparison: alpha 2.0"
      output `shouldContain` "Depends: bravo"
      output `shouldNotContain` "Depends: charlie"

    it "hides comparisons when neither revision can be checked" $ do
      let latest = [("alpha", [], [("2.0", ["cabal-version: 99.0"])])]
          original = [("alpha", [], [])]
      result <- runRevisionPlan False [("alpha", Just "2.0")] latest original
      assertBlocked result "unchecked: alpha 2.0"
      length (Plan.planRevisionNotes result) `shouldBe` 0

    it "shows dependencies removed by a revision" $ do
      let latest = [("alpha", [], [("2.0", [])]), ("bravo", [], [])]
          original = [("alpha", [], [("2.0", lib ["bravo <1"])]), ("bravo", [], [])]
      result <- runRevisionPlan False [("alpha", Just "2.0")] latest original
      assertWorking result [("alpha", "2.0")]
      show (Plan.prettyPlanResult result) `shouldContain` "latest revision: not required"
      show (Plan.prettyPlanResult result) `shouldContain` "revision 0: <1"

    it "does not rerun version selection against revision 0" $ do
      let specs range = [("alpha", [], [("1.1", lib ["bravo " <> range]), ("1.2", [])]), ("bravo", [], [("2.0", [])])]
      result <- runRevisionPlan True [("alpha", Nothing)] (specs ">=1") (specs ">=2")
      assertWorking result [("alpha", "1.1")]
      Plan.plansTried result `shouldBe` 1
      show (Plan.prettyPlanResult result) `shouldContain` "revision 0: >=2"
      length (Plan.planRevisionNotes result) `shouldBe` 1

    it "shows whichever revision is readable without substituting it for latest" $ do
      let valid = [("alpha", [], [("2.0", [])])]
          invalid = [("alpha", [], [("2.0", ["cabal-version: 99.0"])])]
      originalBad <- runRevisionPlan False [("alpha", Just "2.0")] valid invalid
      assertWorking originalBad [("alpha", "2.0")]
      show (Plan.prettyPlanResult originalBad) `shouldContain` "revision 0:"
      show (Plan.prettyPlanResult originalBad) `shouldContain` "unchecked: Unable to parse"
      latestBad <- runRevisionPlan False [("alpha", Just "2.0")] invalid valid
      assertBlocked latestBad "unchecked: alpha 2.0"
      show (Plan.prettyPlanResult latestBad) `shouldContain` "No relevant dependencies"

  it "checks dependencies against the whole proposed set" $ do
    result <- runPlan False [("alpha", Just "2.0"), ("bravo", Just "2.0")]
      [ ("alpha", [], [("2.0", lib ["bravo >=2"])]),
        ("bravo", [], [("2.0", [])])
      ]
    assertWorking result [("alpha", "2.0"), ("bravo", "2.0")]

  it "reports changed dependencies on fixed repository packages" $ do
    result <- runPlan False [("alpha", Just "2.0")]
      [("alpha", [], [("2.0", lib ["bravo >=2"])]), ("bravo", [], [])]
    assertBlocked result "dep: alpha requires bravo >=2"

  it "reports existing direct dependency mismatches as non-blocking warnings" $ do
    result <- runPlan True [("alpha", Just "2.0")]
      [("alpha", ["bravo"], [("1.0", lib ["bravo <1"]), ("2.0", lib ["bravo <1"])]), ("bravo", [], [])]
    assertWorking result [("alpha", "2.0")]
    length (Plan.planWarnings result) `shouldBe` 1
    show (Plan.prettyPlanResult result) `shouldContain` "dep-old: alpha requires bravo <1"

  forM_ ["<1", "<=0.9", "==0.9", ">=0.8 && <1", "<0.5 || ==0.9"] $ \range ->
    it ("does not skip an incremental release for an already exceeded upper bound " <> range) $ do
      result <- runPlan True [("alpha", Nothing)]
        [ ("alpha", ["bravo"], [("1.0", lib ["bravo >=0"]), ("1.1", lib ["bravo " <> range]), ("2.0", lib ["bravo >=1"])]),
          ("bravo", [], [])
        ]
      assertWorking result [("alpha", "1.1")]
      length (Plan.planWarnings result) `shouldBe` 1
      show (Plan.prettyPlanResult result) `shouldContain` "dep-old: alpha requires bravo"

  it "keeps Stack 2.11.1 despite revised upper bounds already exceeded by repository dependencies" $ do
    let dependencies =
          [("casa-client", "0.0.4", ">=0", "<0.0.2"),
           ("hpack", "0.38.0", ">=0", "<0.35.3"),
           ("http-client-tls", "0.3.6.4", ">=0", "<0.3.6.2"),
           ("http-download", "0.2.1.0", ">=0", "<0.2.1.0"),
           ("optparse-applicative", "0.18.1.0", ">=0.17.0.0", "==0.17.0.0")]
        (extra, raw) = fixture $
          ("stack", [package | (package, _, _, _) <- dependencies],
            [("2.9.3.1", lib [package <> " " <> range | (package, _, range, _) <- dependencies]),
             ("2.11.1", lib [package <> " " <> range | (package, _, _, range) <- dependencies]),
             ("2.15.7", lib [package <> " >=" <> release | (package, release, _, _) <- dependencies])])
            : [(package, [], [(release, [])]) | (package, release, _, _) <- dependencies]
        installed = Map.fromList $ (toArchLinuxName $ name "stack", "2.9.3.1")
          : [(toArchLinuxName $ name package, release) | (package, release, _, _) <- dependencies]
        repository = Map.mapWithKey (\package desc -> desc {_version = installed Map.! package, _rawVersion = installed Map.! package <> "-1"}) extra
    result <- requireResult =<< runDB True [("stack", Nothing)] repository raw
    assertWorking result [("stack", "2.11.1")]
    length (Plan.planWarnings result) `shouldBe` length dependencies
    Plan.plansTried result `shouldBe` 1

  forM_ [">=2", "<1 || >=2", ">=2 && <1"] $ \range ->
    it ("still blocks newly unmet lower bounds and exclusions " <> range) $ do
      result <- runPlan False [("alpha", Just "2.0")]
        [("alpha", ["bravo"], [("1.0", lib ["bravo >=0"]), ("2.0", lib ["bravo " <> range])]), ("bravo", [], [])]
      assertBlocked result "dep: alpha requires bravo"
      length (Plan.planWarnings result) `shouldBe` 0

  it "still blocks already exceeded upper bounds on new dependencies" $ do
    result <- runPlan False [("alpha", Just "2.0")]
      [("alpha", [], [("2.0", lib ["bravo <1"])]), ("bravo", [], [])]
    assertBlocked result "dep: alpha requires bravo <1"
    length (Plan.planWarnings result) `shouldBe` 0

  it "still blocks upper bounds newly exceeded by a dependency update" $ do
    result <- runPlan False [("alpha", Just "2.0"), ("bravo", Just "2.0")]
      [("alpha", ["bravo"], [("1.0", lib ["bravo >=0"]), ("2.0", lib ["bravo <2"])]), ("bravo", [], [("2.0", [])])]
    assertBlocked result "dep: alpha requires bravo <2"
    length (Plan.planWarnings result) `shouldBe` 0

  it "does not infer existing dependencies when the installed metadata is unparseable" $ do
    result <- runPlan False [("alpha", Just "2.0")]
      [("alpha", ["bravo"], [("1.0", ["this is not valid cabal metadata"]), ("2.0", lib ["bravo <1"])]), ("bravo", [], [])]
    assertBlocked result "dep: alpha requires bravo <1"
    length (Plan.planWarnings result) `shouldBe` 0

  it "does not propagate already exceeded upper bounds of added packages" $ do
    result <- runPlan True [("alpha", Just "2.0")]
      [ ("alpha", [], [("2.0", lib ["bravo ==2.0"])]),
        ("bravo", ["charlie"], [("1.0", lib ["charlie >=0"]), ("2.0", lib ["charlie <1"])]),
        ("charlie", [], [])
      ]
    assertWorking result [("alpha", "2.0"), ("bravo", "2.0")]
    show (Plan.prettyPlanResult result) `shouldContain` "dep-old: bravo requires charlie <1"

  it "does not propagate existing transitive mismatches as hard requirements" $ do
    result <- runPlan True [("alpha", Just "2.0")]
      [ ("alpha", [], [("2.0", lib ["bravo ==2.0"])]),
        ("bravo", ["charlie"], [("1.0", lib ["charlie <1"]), ("2.0", lib ["charlie <1"])]),
        ("charlie", [], [])
      ]
    assertWorking result [("alpha", "2.0"), ("bravo", "2.0")]
    show (Plan.prettyPlanResult result) `shouldContain` "dep-old: bravo requires charlie <1"

  it "does not warn when a candidate repairs an existing direct mismatch" $ do
    result <- runPlan True [("alpha", Just "2.0")]
      [("alpha", ["bravo"], [("1.0", lib ["bravo <1"]), ("2.0", lib ["bravo >=1"])]), ("bravo", [], [])]
    assertWorking result [("alpha", "2.0")]
    length (Plan.planWarnings result) `shouldBe` 0

  it "does not hide new setup failures behind an existing library mismatch" $ do
    result <- runPlan False [("alpha", Just "2.0")]
      [ ("alpha", ["bravo"], [("1.0", lib ["bravo <1"]), ("2.0", lib ["bravo >=1"] <> ["custom-setup", "  setup-depends: bravo <1"])]),
        ("bravo", [], [])
      ]
    assertBlocked result "dep: alpha requires bravo <1"
    length (Plan.planWarnings result) `shouldBe` 0

  it "classifies existing direct failures against installed dependency versions" $ do
    result <- runPlan False [("alpha", Just "2.0"), ("bravo", Just "2.0")]
      [("alpha", ["bravo"], [("1.0", lib ["bravo <2"]), ("2.0", lib ["bravo <2"])]), ("bravo", [], [("2.0", [])])]
    assertBlocked result "dep: alpha requires bravo <2"
    length (Plan.planWarnings result) `shouldBe` 0

  it "detects impossible transitive bounds before enumerating independent updates" $ do
    let plugins = ["plugin" <> show i | i <- [1 :: Int .. 12]]
    result <- runPlan True [("alpha", Just "2.0")]
      ([ ("alpha", [], [(v, lib (["bravo ==" <> v] <> [package <> " ==" <> v | package <- plugins])) | v <- ["2.0", "3.0"]]),
         ("bravo", [], [(v, lib ["charlie <1"]) | v <- ["2.0", "3.0"]]),
         ("charlie", [], [("2.0", [])])
       ] <> [(package, [], [(v, []) | v <- ["2.0", "3.0"]]) | package <- plugins])
    assertBlocked result "no installed or newer preferred version of charlie satisfies"
    Plan.plansTried result `shouldBe` 1

  forM_ [["bravo"], ["bravo", "charlie", "delta"]] $ \required ->
    it ("stops on proven conflicts with " <> show (length required) <> " initial failures") $ do
      let plugins = ["plugin" <> show i | i <- [1 :: Int .. 12]]
      result <- runPlan True [("alpha", Just "2.0")]
        ([ ("alpha", [], [("2.0", lib [package <> " ==2.0" | package <- required])]),
           ("consumer", ["bravo"],
             [("1.0", lib ["bravo <2"]),
              ("2.0", lib (["bravo ==2.0", "base >=2"] <> [package <> " ==2.0" | package <- plugins]))]),
           ("base", [], [])
         ] <> [(package, [], [("2.0", [])]) | package <- plugins <> required])
      assertBlocked result "dep: alpha requires bravo ==2.0"
      Plan.planVersions result `shouldBe` Map.singleton (name "alpha") (version "2.0")
      length (Plan.planProblems result) `shouldBe` length required
      Plan.plansTried result `shouldBe` 1
      show (Plan.prettyPlanResult result) `shouldContain` "base is fixed at 1.0 by the installed GHC"

  it "does not infer a global conflict when a later target avoids the reverse update" $ do
    result <- runPlan True [("alpha", Just "2.0")]
      [ ("alpha", [], [("2.0", []), ("3.0", [])]),
        ("consumer", ["alpha"], [("1.0", lib ["alpha <2 || >=3"]), ("2.0", lib ["alpha >=2", "base >=2"])]),
        ("base", [], [])
      ]
    assertWorking result [("alpha", "3.0")]
    length (Plan.planSearchNotes result) `shouldBe` 0

  it "does not revalidate unrelated dependencies of packages kept installed" $ do
    result <- runPlan True [("alpha", Just "2.0")]
      [("alpha", [], [("2.0", lib ["bravo >=1"])]), ("bravo", [], [("1.0", lib ["missing >=2"])])]
    assertWorking result [("alpha", "2.0")]

  it "keeps later releases which resolve a transitive bound conflict" $ do
    result <- runPlan True [("alpha", Just "2.0")]
      [ ("alpha", [], [(v, lib ["bravo ==" <> v]) | v <- ["2.0", "3.0"]]),
        ("bravo", [], [("2.0", lib ["charlie <1"]), ("3.0", lib ["charlie >=1"])]),
        ("charlie", [], [])
      ]
    assertWorking result [("alpha", "3.0"), ("bravo", "3.0")]

  it "automatically updates a blocking dependency incrementally" $ do
    result <- runPlan True [("alpha", Just "2.0")]
      [ ("alpha", [], [("2.0", lib ["bravo >=2"])]),
        ("bravo", [], [("1.1", []), ("2.0", []), ("3.0", [])])
      ]
    assertWorking result [("alpha", "2.0"), ("bravo", "2.0")]
    Plan.planRequested result `shouldBe` Map.keysSet (Map.singleton (name "alpha") ())
    show (Plan.prettyPlanResult result) `shouldContain` "bravo 1.0 -> 2.0 (added by solver)"
    last (lines $ show $ Plan.prettyPlanResult result) `shouldBe` "genrebuild -H haskell-alpha haskell-bravo"

  it "automatically updates a blocking reverse dependency" $ do
    result <- runPlan True [("alpha", Just "2.0")]
      [ ("alpha", [], [("2.0", [])]),
        ("consumer", ["alpha"], [("1.0", lib ["alpha <2"]), ("1.1", lib ["alpha >=2"]), ("2.0", [])])
      ]
    assertWorking result [("alpha", "2.0"), ("consumer", "1.1")]

  it "resolves independent reverse dependencies despite an unsatisfiable blocker" $ do
    result <- runPlan True [("alpha", Just "2.0")]
      [ ("alpha", [], [("2.0", [])]),
        ("zblocker", ["alpha"], [("1.0", lib ["alpha <2"])]),
        ("consumer", ["alpha"], [("1.0", lib ["alpha ==1.0"]), ("2.0", lib ["alpha ==2.0"])])
      ]
    assertBlocked result "rdep: haskell-zblocker"
    Plan.planVersions result `shouldBe` Map.fromList [(name "alpha", version "2.0"), (name "consumer", version "2.0")]
    show (Plan.prettyPlanResult result) `shouldNotContain` "rdep: haskell-consumer"
    last (lines $ show $ Plan.prettyPlanResult result) `shouldBe` "genrebuild -H haskell-alpha haskell-consumer"

  it "resolves other blockers even when the first blocker has futile upgrades" $ do
    result <- runPlan True [("alpha", Just "2.0")]
      [ ("alpha", [], [("2.0", [])]),
        ("zblocker", ["alpha"], [("1.0", lib ["alpha <2"]), ("1.1", lib ["alpha <2", "base >=2"])]),
        ("base", [], []),
        ("consumer", ["alpha"], [("1.0", lib ["alpha ==1.0"]), ("2.0", lib ["alpha ==2.0"])])
      ]
    assertBlocked result "rdep: haskell-zblocker"
    Plan.planVersions result `shouldBe` Map.fromList [(name "alpha", version "2.0"), (name "consumer", version "2.0")]
    length (Plan.planProblems result) `shouldBe` 1

  it "expands recursively and checks reverse dependencies of added targets" $ do
    result <- runPlan True [("alpha", Just "2.0")]
      [ ("alpha", [], [("2.0", lib ["bravo >=2"])]),
        ("bravo", [], [("2.0", lib ["charlie >=2"])]),
        ("charlie", [], [("2.0", [])]),
        ("consumer", ["bravo"], [("1.0", lib ["bravo <2"]), ("1.1", lib ["bravo >=2"])]),
        ("downstream", ["consumer"], [("1.0", lib ["consumer <1.1"]), ("1.1", lib ["consumer >=1.1"])])
      ]
    assertWorking result [("alpha", "2.0"), ("bravo", "2.0"), ("charlie", "2.0"), ("consumer", "1.1"), ("downstream", "1.1")]

  it "does not force an added target onto a cheaper alternative branch" $ do
    result <- runPlan True [("alpha", Nothing)]
      [ ("alpha", [], [("1.1", lib ["bravo >=3"]), ("1.2", [])]),
        ("bravo", [], [("2.0", []), ("3.0", [])])
      ]
    assertWorking result [("alpha", "1.2")]
    Map.keys (Plan.planInstalled result) `shouldBe` [name "alpha"]

  it "adds missing Hackage packages and checks their dependencies" $ do
    let (extra, raw) = fixture
          [ ("alpha", [], [("2.0", lib ["bravo >=2"])]),
            ("bravo", [], [("2.0", lib ["charlie >=1"])]),
            ("charlie", [], [])
          ]
        installed = Map.delete (toArchLinuxName $ name "bravo") $ Map.delete (toArchLinuxName $ name "charlie") extra
    result <- requireResult =<< runDB True [("alpha", Just "2.0")] installed raw
    assertWorking result [("alpha", "2.0"), ("bravo", "2.0"), ("charlie", "1.0")]
    show (Plan.prettyPlanResult result) `shouldContain` "bravo not in repo -> 2.0 (added by solver)"

  it "respects preferred versions when adding a blocker" $ do
    let (extra, raw) = fixture
          [("alpha", [], [("2.0", lib ["bravo >=2"])]), ("bravo", [], [("2.0", []), ("3.0", [])])]
        masked = Map.adjust (\(RawHackage.PackageData _ releases) -> RawHackage.PackageData (B8.pack "bravo <2 || >=3") releases) (name "bravo") raw
    result <- requireResult =<< runDB True [("alpha", Just "2.0")] extra masked
    assertWorking result [("alpha", "2.0"), ("bravo", "3.0")]

  it "does not automatically update GHC bundled libraries" $ do
    result <- runPlan True [("alpha", Just "2.0")]
      [("alpha", [], [("2.0", lib ["base >=2"])]), ("base", [], [("2.0", [])])]
    assertBlocked result "dep: alpha requires base >=2"
    Map.keys (Plan.planVersions result) `shouldBe` [name "alpha"]

  it "can update an unparseable reverse dependent to a verifiable release" $ do
    result <- runPlan True [("alpha", Just "2.0")]
      [ ("alpha", [], [("2.0", lib ["consumer >=1.1"])]),
        ("consumer", ["alpha"], [("1.0", ["cabal-version: 99.0"]), ("1.1", lib ["alpha >=2"])])
      ]
    assertWorking result [("alpha", "2.0"), ("consumer", "1.1")]

  it "reports new dependencies missing from the repository" $ do
    result <- runPlan False [("alpha", Just "2.0")]
      [("alpha", [], [("2.0", lib ["missing >=2"])])]
    assertBlocked result "missing from the update set and repository"

  it "checks test and setup dependencies" $ do
    forM_
      [ ["custom-setup", "  setup-depends: bravo >=2"],
        ["test-suite checks", "  type: exitcode-stdio-1.0", "  main-is: Test.hs", "  build-depends: bravo >=2"]
      ] $ \body -> do
        result <- runPlan False [("alpha", Just "2.0")]
          [("alpha", [], [("2.0", body)]), ("bravo", [], [])]
        assertBlocked result "dep: alpha requires bravo >=2"

  it "checks versioned Haskell build tools" $ do
    result <- runPlan False [("alpha", Just "2.0")]
      [("alpha", [], [("2.0", ["library", "  build-tool-depends: bravo:tool >=2"])]), ("bravo", [], [])]
    assertBlocked result "dep: alpha requires bravo >=2"

  it "applies assigned flags when evaluating candidate dependencies" $ do
    let (extra, raw) = fixture
          [ ("alpha", [], [("2.0", ["flag relaxed", "  default: False", "library", "  if flag(relaxed)", "    build-depends: bravo >=1", "  else", "    build-depends: bravo >=2"])]),
            ("bravo", [], [])
          ]
        flags = Map.singleton (name "alpha") $ mkFlagAssignment [(mkFlagName "relaxed", True)]
    blocked <- requireResult =<< runDB False [("alpha", Just "2.0")] extra raw
    assertBlocked blocked "dep: alpha requires bravo >=2"
    working <- requireResult =<< runDBWithFlags flags False [("alpha", Just "2.0")] extra raw
    assertWorking working [("alpha", "2.0")]

  it "checks fixed reverse dependencies" $ do
    result <- runPlan False [("alpha", Just "2.0")]
      [("alpha", [], [("2.0", [])]), ("consumer", ["alpha"], [("1.0", lib ["alpha <2"])])]
    assertBlocked result "rdep: haskell-consumer Depends requires alpha <2"

  it "checks ranges in every reverse dependent component" $ do
    result <- runPlan False [("alpha", Just "2.0")]
      [ ("alpha", [], [("2.0", [])]),
        ("consumer", ["alpha"], [("1.0", lib ["alpha >=1"] <> ["executable tool", "  main-is: Main.hs", "  build-depends: alpha <2"])])
      ]
    assertBlocked result "rdep: haskell-consumer Depends requires alpha <2"

  it "reports existing reverse failures as non-blocking warnings" $ do
    result <- runPlan True [("alpha", Nothing)]
      [("alpha", [], [("1.1", []), ("2.0", [])]), ("consumer", ["alpha"], [("1.0", lib ["alpha <1"])])]
    assertWorking result [("alpha", "1.1")]
    length (Plan.planWarnings result) `shouldBe` 1
    show (Plan.prettyPlanResult result) `shouldContain` "rdep-old: haskell-consumer"

  it "updates newly broken reverse dependencies alongside existing warnings" $ do
    result <- runPlan True [("alpha", Just "2.0")]
      [ ("alpha", [], [("2.0", [])]),
        ("zblocker", ["alpha"], [("1.0", lib ["alpha <1"])]),
        ("consumer", ["alpha"], [("1.0", lib ["alpha ==1.0"]), ("2.0", lib ["alpha ==2.0"])])
      ]
    assertWorking result [("alpha", "2.0"), ("consumer", "2.0")]
    length (Plan.planWarnings result) `shouldBe` 1
    show (Plan.prettyPlanResult result) `shouldContain` "rdep-old: haskell-zblocker"

  it "drops existing reverse failures when the proposed version satisfies them" $ do
    result <- runPlan False [("alpha", Just "2.0")]
      [("alpha", [], [("2.0", [])]), ("consumer", ["alpha"], [("1.0", lib ["alpha >=2"])])]
    assertWorking result [("alpha", "2.0")]
    length (Plan.planWarnings result) `shouldBe` 0

  it "uses candidate metadata when a reverse dependent is also updated" $ do
    result <- runPlan False [("alpha", Just "2.0"), ("consumer", Just "2.0")]
      [ ("alpha", [], [("2.0", [])]),
        ("consumer", ["alpha"], [("1.0", lib ["alpha <2"]), ("2.0", lib ["alpha >=2"])])
      ]
    assertWorking result [("alpha", "2.0"), ("consumer", "2.0")]

  it "selects the next release rather than the highest by default" $ do
    result <- runPlan False [("alpha", Nothing)]
      [("alpha", [], [("1.1", []), ("2.0", []), ("3.0", [])])]
    assertWorking result [("alpha", "1.1")]

  it "does not search unless requested" $ do
    result <- runPlan False [("alpha", Nothing), ("bravo", Nothing)] incrementalFixture
    assertBlocked result "dep: alpha requires bravo >=3"
    Plan.plansTried result `shouldBe` 1

  it "minimizes total release steps across alternative combinations" $ do
    result <- runPlan True [("alpha", Nothing), ("bravo", Nothing)] incrementalFixture
    assertWorking result [("alpha", "1.2"), ("bravo", "1.1")]

  it "does not enumerate subsets of independent required updates" $ do
    let dependencies = ["dependency" <> show i | i <- [1 :: Int .. 12]]
    result <- runPlan True [("alpha", Just "2.0")]
      (("alpha", [], [("2.0", lib [package <> " >=2" | package <- dependencies])])
        : [(package, [], [("2.0", [])]) | package <- dependencies])
    assertWorking result (("alpha", "2.0") : [(package, "2.0") | package <- dependencies])
    Plan.plansTried result `shouldSatisfy` (<= 13)

  it "avoids enumerating optional subsets while retaining the earliest working release" $ do
    let dependencies = ["dependency" <> show i | i <- [1 :: Int .. 12]]
    result <- runPlan True [("alpha", Just "2.0")]
      ([ ("alpha", [], [("2.0", lib [package <> " >=2" | package <- dependencies]), ("3.0", [])]),
         ("consumer", ["alpha"], [("1.0", lib ["alpha <3"])])
       ] <> [(package, [], [("2.0", [])]) | package <- dependencies])
    assertWorking result (("alpha", "2.0") : [(package, "2.0") | package <- dependencies])
    Plan.plansTried result `shouldSatisfy` (<= 30)

  it "recognizes required updates across packages locked to the same release" $ do
    let dependencies = ["dependency" <> show i | i <- [1 :: Int .. 8]]
    result <- runPlan True [("alpha", Just "2.0")]
      (("alpha", [], [(v, lib [package <> " ==" <> v | package <- dependencies]) | v <- ["2.0", "3.0"]])
        : [(package, [], [(v, lib ["alpha ==" <> v]) | v <- ["2.0", "3.0"]]) | package <- dependencies])
    assertWorking result (("alpha", "2.0") : [(package, "2.0") | package <- dependencies])
    Plan.plansTried result `shouldSatisfy` (<= 12)

  it "does not use incompatible later server releases to underestimate a plugin update" $ do
    let plugins = ["plugin" <> show i | i <- [1 :: Int .. 8]]
    result <- runPlan True [("alpha", Just "2.0")]
      ([ ("alpha", [], [(v, lib ["bravo ==" <> v]) | v <- ["2.0", "3.0"]]),
         ("bravo", [], [(v, []) | v <- ["2.0", "3.0", "4.0"]]),
         ("server", ["alpha"], [("1.0", lib ["alpha ==1.0"]), ("4.0", lib ["bravo ==4.0"])]
           <> [(v, lib (["alpha ==" <> v, "bravo ==" <> v] <> [package <> " ==" <> v | package <- plugins])) | v <- ["2.0", "3.0"]])
       ] <> [(package, [], [(v, lib ["bravo ==" <> v]) | v <- ["2.0", "3.0"]]) | package <- plugins])
    assertWorking result ([("alpha", "2.0"), ("bravo", "2.0"), ("server", "2.0")] <> [(package, "2.0") | package <- plugins])
    Plan.plansTried result `shouldSatisfy` (<= 20)

  it "accounts for mandatory reverse updates before exploring server alternatives" $ do
    let plugins = ["plugin" <> show i | i <- [1 :: Int .. 12]]
    result <- runPlan True [("alpha", Just "2.0")]
      ([ ("alpha", [], [(v, lib ["bravo ==" <> v]) | v <- ["2.0", "3.0"]]),
         ("bravo", [], [(v, []) | v <- ["2.0", "3.0"]]),
         ("server", ["alpha"],
           [("1.0", lib ["alpha ==1.0"]),
            ("2.0", lib (["alpha ==2.0", "bravo ==2.0"] <> [package <> " ==2.0" | package <- plugins])),
            ("3.0", lib ["alpha ==3.0", "bravo ==3.0"])])
       ] <> [(package, ["bravo"], [(v, lib ["bravo ==" <> v]) | v <- ["1.0", "2.0", "3.0"]]) | package <- plugins])
    assertWorking result ([("alpha", "2.0"), ("bravo", "2.0"), ("server", "2.0")] <> [(package, "2.0") | package <- plugins])
    Plan.plansTried result `shouldSatisfy` (<= 16)

  it "counts a shared dependency repair only once when preferring small updates" $ do
    result <- runPlan True [("alpha", Just "2.0"), ("bravo", Just "2.0")]
      [ ("alpha", [], [("2.0", lib ["charlie >=2"]), ("2.1", [])]),
        ("bravo", [], [("2.0", lib ["charlie >=2"]), ("2.1", [])]),
        ("charlie", [], [("2.0", [])])
      ]
    assertWorking result [("alpha", "2.0"), ("bravo", "2.0"), ("charlie", "2.0")]

  it "agrees with exhaustive search on small non-monotonic version grids" $ do
    forM_ ["bravo <2", "bravo >=3", "bravo >=4"] $ \initialRange -> do
      let specs =
            [ ("alpha", [], [("1.1", lib [initialRange]), ("1.2", lib ["bravo <2"]), ("2.0", lib ["bravo >=2"])]),
              ("bravo", [], [("1.1", lib ["alpha <1.2"]), ("2.0", lib ["alpha >=2"]), ("3.0", [])])
            ]
          alphas = ["1.1", "1.2", "2.0"]
          bravos = ["1.1", "2.0", "3.0"]
          grid = [(i + j, a, b) | (i, a) <- zip [0 :: Int ..] alphas, (j, b) <- zip [0 ..] bravos]
      exhaustive <- mapM
        (\(cost, a, b) -> do
          checked <- runPlan False [("alpha", Just a), ("bravo", Just b)] specs
          pure (cost, null $ Plan.planProblems checked)) grid
      solved <- runPlan True [("alpha", Nothing), ("bravo", Nothing)] specs
      let scores = [cost | (cost, True) <- exhaustive]
          selectedCost = sum
            [ index
              | (package, releases) <- [("alpha", alphas), ("bravo", bravos)],
                (index, release) <- zip [0 :: Int ..] releases,
                Plan.planVersions solved Map.! name package == version release
            ]
      null (Plan.planProblems solved) `shouldBe` not (null scores)
      if null scores then pure () else selectedCost `shouldBe` minimum scores

  forM_ [3, 4] $ \count ->
    it ("agrees with exhaustive search on a " <> show count <> "-package constraint cycle") $ do
      let packages = take count ["alpha", "bravo", "charlie", "delta"]
          specs =
            [ (owner, [], [("2.0", lib [dependency <> " ==3.0"]), ("3.0", lib [dependency <> " ==2.0"])])
              | (owner, dependency) <- zip packages (drop 1 packages <> take 1 packages)
            ]
          combinations = sequence $ replicate count ["2.0", "3.0"]
      checked <- mapM
        (\versions -> do
          result <- runPlan False (zip packages $ Just <$> versions) specs
          pure (length $ filter (== "3.0") versions, Plan.planIsReady result))
        combinations
      solved <- runPlan True [(package, Just "2.0") | package <- packages] specs
      let costs = [cost | (cost, True) <- checked]
      Plan.planIsReady solved `shouldBe` not (null costs)
      if null costs
        then length (Plan.planProblems solved) `shouldSatisfy` (> 0)
        else length (filter (== version "3.0") $ Map.elems $ Plan.planVersions solved) `shouldBe` minimum costs

  it "searches beyond supplied minimum versions without jumping to the latest" $ do
    result <- runPlan True [("alpha", Just "2.0"), ("bravo", Just "2.0")]
      [ ("alpha", [], [("1.1", []), ("2.0", lib ["bravo >=3"])]),
        ("bravo", [], [("2.0", []), ("3.0", []), ("4.0", [])])
      ]
    assertWorking result [("alpha", "2.0"), ("bravo", "3.0")]

  it "can find a later release which removes a missing dependency" $ do
    result <- runPlan True [("alpha", Nothing)]
      [("alpha", [], [("1.1", lib ["missing >=2"]), ("1.2", []), ("2.0", [])])]
    assertWorking result [("alpha", "1.2")]

  it "terminates when no combination satisfies an external reverse dependency" $ do
    result <- runPlan True [("alpha", Nothing)]
      [("alpha", [], [("2.0", []), ("3.0", [])]), ("consumer", ["alpha"], [("1.0", lib ["alpha <2"])])]
    assertBlocked result "rdep: haskell-consumer"
    Plan.plansTried result `shouldBe` 1

  it "never treats unparseable candidate metadata as compatible" $ do
    result <- runPlan False [("alpha", Just "1.1")]
      [("alpha", [], [("1.1", ["cabal-version: 99.0"])])]
    assertBlocked result "unchecked: alpha 1.1"

  it "can advance beyond an unparseable candidate" $ do
    result <- runPlan True [("alpha", Nothing)]
      [("alpha", [], [("1.1", ["cabal-version: 99.0"]), ("1.2", [])])]
    assertWorking result [("alpha", "1.2")]

  it "reports unavailable repository metadata as a non-blocking unchecked warning" $ do
    result <- runPlan True [("alpha", Nothing)]
      [("alpha", [], [("1.1", []), ("1.2", [])]), ("consumer", ["alpha"], [("1.0", ["cabal-version: 99.0"])])]
    assertUnchecked result "unchecked rdep: haskell-consumer"

  it "does not multiply an unchecked reverse dependency across added targets" $ do
    result <- runPlan True [("alpha", Just "2.0")]
      [ ("alpha", [], [("2.0", lib ["bravo ==2.0", "charlie ==2.0"])]),
        ("bravo", [], [("2.0", [])]),
        ("charlie", [], [("2.0", [])]),
        ("consumer", ["alpha"], [("1.0", ["cabal-version: 99.0"])])
      ]
    assertUnchecked result "unchecked rdep: haskell-consumer for alpha"
    Plan.planVersions result `shouldBe` Map.fromList [(name package, version "2.0") | package <- ["alpha", "bravo", "charlie"]]
    length (Plan.planWarnings result) `shouldBe` 1
    Plan.plansTried result `shouldSatisfy` (<= 4)

  it "reports an unchecked owner once when it depends on several targets" $ do
    result <- runPlan True [("alpha", Just "2.0")]
      [ ("alpha", [], [("2.0", lib ["bravo ==2.0"])]),
        ("bravo", [], [("2.0", [])]),
        ("consumer", ["alpha", "bravo"], [("1.0", ["cabal-version: 99.0"])])
      ]
    assertUnchecked result "unchecked rdep: haskell-consumer for alpha, bravo"
    length (Plan.planWarnings result) `shouldBe` 1
    Plan.planVersions result `shouldBe` Map.fromList [(name package, version "2.0") | package <- ["alpha", "bravo"]]

  it "does not take larger updates solely to avoid unchecked repository metadata" $ do
    result <- runPlan True [("alpha", Just "2.0")]
      [ ("alpha", [], [("2.0", lib ["bravo ==2.0"]), ("2.1", lib ["bravo ==2.0"]), ("3.0", [])]),
        ("bravo", [], [("2.0", [])]),
        ("consumer", ["bravo"], [("1.0", ["cabal-version: 99.0"])])
      ]
    assertWorking result [("alpha", "2.0"), ("bravo", "2.0")]
    assertUnchecked result "unchecked rdep: haskell-consumer"

  it "keeps known incompatibilities blocking alongside unchecked repository metadata" $ do
    result <- runPlan False [("alpha", Just "2.0")]
      [ ("alpha", [], [("2.0", lib ["bravo >=2"])]),
        ("bravo", [], []),
        ("consumer", ["alpha"], [("1.0", ["cabal-version: 99.0"])])
      ]
    assertBlocked result "dep: alpha requires bravo >=2"
    show (Plan.prettyPlanResult result) `shouldContain` "unchecked rdep: haskell-consumer"

  it "respects preferred versions during automatic selection" $ do
    let (extra, raw) = fixture [("alpha", [], [("1.1", []), ("1.2", []), ("2.0", [])])]
        masked = Map.adjust (\(RawHackage.PackageData _ releases) -> RawHackage.PackageData (B8.pack "alpha >=1.2") releases) (name "alpha") raw
    result <- requireResult =<< runDB True [("alpha", Nothing)] extra masked
    assertWorking result [("alpha", "1.2")]
    explicit <- requireResult =<< runDB False [("alpha", Just "1.1")] extra masked
    assertWorking explicit [("alpha", "1.1")]

  forM_
    [ ([("alpha", Nothing), ("alpha", Just "2.0")], "only once"),
      ([("alpha", Just "0.5")], "Downgrades"),
      ([("alpha", Nothing)], "No newer"),
      ([("base", Nothing)], "toolchain")
    ] $ \(targets, message) ->
      it ("rejects invalid plan request: " <> message) $ do
        let (extra, raw) = fixture [("alpha", [], [])]
        result <- runDB True targets extra raw
        case result of
          Right (Left err) -> err `shouldContain` message
          _ -> expectationFailure "expected invalid plan request to be rejected"

  describe "GHC toolchains" $ do
    it "parses upstream snapshots and excludes Windows-only libraries" $ do
      let metadata = B8.pack $ unlines
            [ "ghc-9.6.7:", "  ghc: 9.6.7", "  base: '2.0'", "  ghc-prim: '1.0'",
              "  template-haskell: '1.0'", "  Win32: '2.0'", "ghcjs-0.2:", "  base: '1.0'"
            ]
      Toolchain.parseGHCReleases metadata `shouldBe` Right (Map.take 1 toolchainReleases)

    forM_
      [ "{}", "ghc-9.6.7: [", "ghc-invalid: {}", "ghc-9.6.7: {}",
        "ghc-9.6.7: {ghc: 9.8.1}", "ghc-9.6.7: {ghc: 9.6.7, base: invalid}"
      ] $ \metadata ->
        it ("rejects unusable upstream metadata: " <> metadata) $
          Toolchain.parseGHCReleases (B8.pack metadata) `shouldSatisfy` isLeft

    it "distinguishes stable releases from development snapshots" $ do
      filter Toolchain.stableGHCRelease (version <$> ["9.6.7", "9.7.1", "9.8.1", "9.4.0.20220721", "9.8.0"])
        `shouldBe` (version <$> ["9.6.7", "9.8.1"])

    it "selects the next compiler and keeps bundled libraries out of package updates" $ do
      result <- runToolchainPlan False [("ghc", Nothing)] []
      assertWorking result [("ghc", "9.6.7")]
      let output = show $ Plan.prettyPlanResult result
      output `shouldContain` "Bundled with GHC 9.6.7:"
      output `shouldContain` "base 1.0 -> 2.0"
      output `shouldNotContain` "ghc-prim 1.0 -> 1.0"
      output `shouldNotContain` "template-haskell 1.0 -> 1.0"
      output `shouldContain` "Commit message:\nghc 9.6.7\n\ngenrebuild -H --ignore ghc-static ghc"

    it "hides the bundled summary when all library versions are unchanged" $ do
      let releases = Map.adjust (Map.insert (name "base") (version "1.0")) (version "9.6.7") toolchainReleases
          (extra, raw) = toolchainFixture []
      result <- requireResult =<< runDBWithToolchains releases Nothing Map.empty False [("ghc", Nothing)] extra raw
      assertWorking result [("ghc", "9.6.7")]
      let output = show $ Plan.prettyPlanResult result
      output `shouldNotContain` "Bundled with GHC"
      output `shouldContain` "ghc 9.6.6 -> 9.6.7"

    it "shows newly bundled libraries absent from the repository" $ do
      let releases = Map.adjust (Map.insert (name "os-string") (version "2.0")) (version "9.6.7") toolchainReleases
          (extra, raw) = toolchainFixture []
      result <- requireResult =<< runDBWithToolchains releases Nothing Map.empty False [("ghc", Nothing)] extra raw
      assertWorking result [("ghc", "9.6.7")]
      show (Plan.prettyPlanResult result) `shouldContain` "os-string not in repo -> 2.0"

    it "advances GHC incrementally when the next bundle is incompatible" $ do
      result <- runToolchainPlan True [("ghc", Nothing)]
        [("consumer", [], [("1.0", lib ["base ==1.0 || >=3"])])]
      assertWorking result [("ghc", "9.8.1")]

    it "checks explicit GHC versions exactly and treats them as minimums with solve" $ do
      let specs = [("consumer", [], [("1.0", lib ["base ==1.0 || >=4"])])]
      exact <- runToolchainPlan False [("ghc", Just "9.8.1")] specs
      assertBlocked exact "rdep: haskell-consumer Depends requires base"
      Plan.planVersions exact `shouldBe` Map.singleton (name "ghc") (version "9.8.1")
      solved <- runToolchainPlan True [("ghc", Just "9.8.1")] specs
      assertWorking solved [("ghc", "9.8.2")]

    it "updates blocking packages without jumping to a later compiler" $ do
      result <- runToolchainPlan True [("ghc", Nothing)]
        [("consumer", [], [("1.0", lib ["base <2"]), ("1.1", lib ["base <3"]), ("2.0", lib ["base >=3"])])]
      assertWorking result [("ghc", "9.6.7"), ("consumer", "1.1")]

    it "minimizes release steps across compiler and package updates" $ do
      result <- runToolchainPlan True [("ghc", Nothing)]
        [ ("consumer", [], [("1.0", lib ["base ==1.0 || >=3"]), ("1.1", lib ["base >=2", "helper >=2"])]),
          ("helper", [], [("2.0", [])])
        ]
      assertWorking result [("ghc", "9.8.1")]

    it "reevaluates compiler conditionals even without a repository GHC dependency" $ do
      result <- runToolchainPlan True [("ghc", Nothing)]
        [ ("consumer", [], [("1.0", ["library", "  if impl(ghc >=9.6.7)", "    build-depends: helper >=2", "  else", "    build-depends: helper >=1"])]),
          ("helper", [], [("2.0", [])])
        ]
      assertWorking result [("ghc", "9.6.7"), ("helper", "2.0")]

    it "does not reuse dependency caches across compiler versions" $ do
      result <- runToolchainPlan True [("ghc", Nothing)]
        [("consumer", [], [("1.0", ["library", "  if impl(ghc >=9.6.7 && <9.8)", "    build-depends: missing >=1"])])]
      assertWorking result [("ghc", "9.8.1")]

    it "blocks upper bounds newly activated by the compiler" $ do
      result <- runToolchainPlan False [("ghc", Nothing)]
        [ ("consumer", [], [("1.0", ["library", "  if impl(ghc >=9.6.7)", "    build-depends: helper <1", "  else", "    build-depends: helper >=1"])]),
          ("helper", [], [])
        ]
      assertBlocked result "rdep: haskell-consumer Depends requires helper <1"
      length (Plan.planWarnings result) `shouldBe` 0

    it "retains failures already present under the installed compiler as warnings" $ do
      result <- runToolchainPlan False [("ghc", Nothing)]
        [("consumer", [], [("1.0", lib ["base <1"])])]
      assertWorking result [("ghc", "9.6.7")]
      show (Plan.prettyPlanResult result) `shouldContain` "rdep-old: haskell-consumer"

    it "retains incremental updates for exceeded bounds unchanged by the compiler" $ do
      result <- runToolchainPlan True [("ghc", Nothing), ("alpha", Nothing)]
        [ ("alpha", ["bravo"], [("1.0", lib ["bravo >=0"]), ("1.1", lib ["bravo <1"]), ("1.2", lib ["bravo >=0"])]),
          ("bravo", [], [])
        ]
      assertWorking result [("ghc", "9.6.7"), ("alpha", "1.1")]
      show (Plan.prettyPlanResult result) `shouldContain` "dep-old: alpha requires bravo <1"

    it "never updates a bundled library independently to repair a GHC plan" $ do
      let (extra, raw) = toolchainFixture [("consumer", [], [("1.0", lib ["base ==1.0 || >=3"])])]
      result <- requireResult =<< runDBWithToolchains (Map.take 1 toolchainReleases) Nothing Map.empty True [("ghc", Nothing)] extra raw
      assertBlocked result "rdep: haskell-consumer"
      Plan.planVersions result `shouldBe` Map.singleton (name "ghc") (version "9.6.7")

    it "does not retain libraries removed from the compiler bundle" $ do
      result <- runToolchainPlan False [("ghc", Nothing)]
        [("consumer", [], [("1.0", lib ["libiserv >=1"])]), ("libiserv", [], [])]
      assertBlocked result "dep: consumer requires libiserv >=1, missing"
      show (Plan.prettyPlanResult result) `shouldContain` "libiserv 1.0 -> not bundled"

    it "uses the old upstream bundle for baseline libraries missing from repository provides" $ do
      let baseline = Map.fromList [(name package, version release) | (package, release) <- [("ghc", "9.6.6"), ("base", "1.0"), ("rts", "1.0")]]
          releases = Map.insert (version "9.6.6") baseline $ Map.adjust (Map.insert (name "rts") (version "2.0")) (version "9.6.7") toolchainReleases
          (extra, raw) = toolchainFixture [("consumer", [], [("1.0", lib ["rts <2"])])]
      result <- requireResult =<< runDBWithToolchains releases Nothing Map.empty False [("ghc", Nothing)] extra raw
      assertBlocked result "rdep: haskell-consumer Depends requires rts <2"
      length (Plan.planWarnings result) `shouldBe` 0

    it "uses newly bundled library versions instead of standalone repository versions" $ do
      let releases = Map.adjust (Map.insert (name "os-string") (version "2.0")) (version "9.6.7") toolchainReleases
          (extra, raw) = toolchainFixture [("consumer", [], [("1.0", lib ["os-string <2"])]), ("os-string", [], [])]
      result <- requireResult =<< runDBWithToolchains releases Nothing Map.empty False [("ghc", Nothing)] extra raw
      assertBlocked result "rdep: haskell-consumer Depends requires os-string <2"

    it "reports compiler-provided tools as unchecked instead of removed libraries" $ do
      let (extra, raw) = toolchainFixture [("consumer", [], [("1.0", lib ["hsc2hs >=2"])]), ("hsc2hs", [], [])]
          provided = Map.adjust (\desc -> desc {_provides = [PkgDependent (toArchLinuxName $ name "hsc2hs") (Just "1.0")]}) (ArchLinuxName "ghc") extra
      result <- requireResult =<< runDBWithToolchains toolchainReleases Nothing Map.empty True [("ghc", Nothing)] provided raw
      assertUnchecked result "unchecked compiler tool: consumer requires hsc2hs >=2"
      Plan.planVersions result `shouldBe` Map.singleton (name "ghc") (version "9.6.7")
      show (Plan.prettyPlanResult result) `shouldNotContain` "hsc2hs 1.0 -> not bundled"
      requested <- runDBWithToolchains toolchainReleases Nothing Map.empty True [("ghc", Nothing), ("hsc2hs", Just "1.0")] provided raw
      case requested of
        Right (Left err) -> err `shouldContain` "compiler tools cannot be updated independently"
        _ -> expectationFailure "expected an independent compiler-tool request to be rejected"

    it "normalizes provided library aliases to upstream Hackage capitalization" $ do
      let releases = Map.adjust (Map.insert (name "Cabal-syntax") (version "1.0")) (version "9.6.7") toolchainReleases
          (extra, raw) = toolchainFixture [("Cabal-syntax", [], [])]
          provided = Map.adjust (\desc -> desc {_provides = [PkgDependent (ArchLinuxName "haskell-cabal-syntax") (Just "1.0")]}) (ArchLinuxName "ghc") extra
      result <- requireResult =<< runDBWithToolchains releases Nothing Map.empty False [("ghc", Nothing)] provided raw
      assertWorking result [("ghc", "9.6.7")]
      let output = show $ Plan.prettyPlanResult result
      output `shouldNotContain` "Cabal-syntax 1.0 -> 1.0"
      output `shouldNotContain` "cabal-syntax 1.0 -> not bundled"
      length (Plan.planWarnings result) `shouldBe` 0

    it "reports unavailable repository metadata as unchecked rather than passing silently" $ do
      result <- runToolchainPlan False [("ghc", Nothing)] [("consumer", [], [("1.0", ["cabal-version: 99.0"])])]
      assertUnchecked result "unchecked rdep: haskell-consumer"

    it "uses the target compiler and bundle in revision comparisons" $ do
      let specs bound = [("consumer", [], [("1.0", ["library", "  if impl(ghc >=9.6.7)", "    build-depends: base " <> bound])])]
          (extra, raw) = toolchainFixture $ specs "<3"
          (_, original) = toolchainFixture $ specs "<2"
      result <- requireResult =<< runDBWithToolchains toolchainReleases (Just original) Map.empty False [("ghc", Nothing)] extra raw
      assertWorking result [("ghc", "9.6.7")]
      let output = show $ Plan.prettyPlanResult result
      output `shouldContain` "Revision comparison: consumer 1.0"
      output `shouldContain` "latest revision: <3 (ok)"
      output `shouldContain` "rdep: haskell-consumer Depends requires base <2"

    forM_
      [ ([("ghc", Just "9.8.3")], "No upstream bundled-library metadata"),
        ([("ghc", Just "9.4.8")], "Downgrades"),
        ([("ghc", Nothing), ("base", Just "2.0")], "cannot be updated independently")
      ] $ \(targets, message) ->
        it ("rejects invalid GHC requests: " <> message) $ do
          let (extra, raw) = toolchainFixture []
          result <- runDBWithToolchains toolchainReleases Nothing Map.empty True targets extra raw
          case result of
            Right (Left err) -> err `shouldContain` message
            _ -> expectationFailure "expected an invalid GHC request"

toolchainReleases :: Toolchain.GHCReleases
toolchainReleases = Map.fromList
  [ (version compiler, Map.fromList [(name package, version release) | (package, release) <- [("ghc", compiler), ("base", base), ("ghc-prim", "1.0"), ("template-haskell", "1.0")]])
    | (compiler, base) <- [("9.6.7", "2.0"), ("9.8.1", "3.0"), ("9.8.2", "4.0")]
  ]

toolchainFixture :: Fixture -> (ExtraDB, RawHackage.HackageDB)
toolchainFixture specs =
  let bundled = ["ghc", "base", "ghc-prim", "template-haskell"]
      (extra, raw) = fixture ([(package, [], []) | package <- bundled] <> specs)
      compiler = (extra Map.! toArchLinuxName (name "ghc")) {_name = ArchLinuxName "ghc", _version = "9.6.6", _rawVersion = "9.6.6-1"}
   in ( Map.insert (ArchLinuxName "ghc") compiler $ Map.delete (toArchLinuxName $ name "ghc") extra,
        Map.filterWithKey (\package _ -> package `notElem` (name <$> bundled)) raw
      )

runToolchainPlan :: Bool -> [(String, Maybe String)] -> Fixture -> IO Plan.PlanResult
runToolchainPlan solve targets specs = do
  let (extra, raw) = toolchainFixture specs
  requireResult =<< runDBWithToolchains toolchainReleases Nothing Map.empty solve targets extra raw

incrementalFixture :: Fixture
incrementalFixture =
  [ ("alpha", [], [("1.1", lib ["bravo >=3"]), ("1.2", lib ["bravo <2"]), ("2.0", lib ["bravo >=3"])]),
    ("bravo", [], [("1.1", []), ("2.0", []), ("3.0", []), ("4.0", [])])
  ]

type Fixture = [(String, [String], [(String, [String])])]

lib :: [String] -> [String]
lib deps = ["library", "  build-depends: " <> intercalate ", " deps]

name :: String -> PackageName
name = mkPackageName

version :: String -> Version
version text = case simpleParsec text of
  Just parsed -> parsed
  Nothing -> error $ "invalid fixture version: " <> text

fixture :: Fixture -> (ExtraDB, RawHackage.HackageDB)
fixture specs =
  ( Map.fromList [(toArchLinuxName $ name package, desc package deps) | (package, deps, _) <- specs],
    Map.fromList [(name package, packageData package releases) | (package, _, releases) <- specs]
  )
  where
    packageData package releases = RawHackage.PackageData B8.empty $ Map.fromList
      [ (version release, RawHackage.VersionData (cabal package release body) B8.empty)
        | (release, body) <- ("1.0", []) : releases
      ]
    cabal package release body = B8.pack $ unlines $
      ["cabal-version: 1.24", "name: " <> package, "version: " <> release, "build-type: Simple"] <> body
    desc package deps = PkgDesc
      { _name = toArchLinuxName $ name package,
        _version = "1.0",
        _rawVersion = "1.0-1",
        _desc = "Planner fixture",
        _url = Nothing,
        _provides = [],
        _optDepends = [],
        _replaces = [],
        _conflicts = [],
        _depends = [PkgDependent (toArchLinuxName $ name dep) Nothing | dep <- deps],
        _makeDepends = [],
        _checkDepends = []
      }

runPlan :: Bool -> [(String, Maybe String)] -> Fixture -> IO Plan.PlanResult
runPlan solve targets specs = do
  let (extra, raw) = fixture specs
  requireResult =<< runDB solve targets extra raw

runRevisionPlan :: Bool -> [(String, Maybe String)] -> Fixture -> Fixture -> IO Plan.PlanResult
runRevisionPlan solve targets latest original = do
  let (extra, raw) = fixture latest
      (_, revision0) = fixture original
  requireResult =<< runDBWithRevisions (Just revision0) Map.empty solve targets extra raw

runDB :: Bool -> [(String, Maybe String)] -> ExtraDB -> RawHackage.HackageDB -> IO (Either MyException (Either String Plan.PlanResult))
runDB = runDBWithFlags Map.empty

runDBWithFlags :: FlagAssignments -> Bool -> [(String, Maybe String)] -> ExtraDB -> RawHackage.HackageDB -> IO (Either MyException (Either String Plan.PlanResult))
runDBWithFlags = runDBWithRevisions Nothing

runDBWithRevisions :: Maybe RawHackage.HackageDB -> FlagAssignments -> Bool -> [(String, Maybe String)] -> ExtraDB -> RawHackage.HackageDB -> IO (Either MyException (Either String Plan.PlanResult))
runDBWithRevisions = runDBWithToolchains Map.empty

runDBWithToolchains :: Toolchain.GHCReleases -> Maybe RawHackage.HackageDB -> FlagAssignments -> Bool -> [(String, Maybe String)] -> ExtraDB -> RawHackage.HackageDB -> IO (Either MyException (Either String Plan.PlanResult))
runDBWithToolchains releases original flags solve targets extra raw =
  runM
    . runError @MyException
    . evalState (Map.empty :: Map.Map PackageName [VersionRange])
    . ignoreTrace
    . runReader flags
    . runReader (version "9.6.6")
    . runReader raw
    . runReader (Hackage.parseDB raw)
    . runReader extra
    $ do
      planned <- Plan.planUpdates releases solve [(name package, version <$> candidate) | (package, candidate) <- targets]
      case original of
        Nothing -> pure planned
        Just revision0 -> traverse (Plan.comparePlanRevisions revision0) planned

requireResult :: Either MyException (Either String Plan.PlanResult) -> IO Plan.PlanResult
requireResult (Right (Right result)) = pure result
requireResult (Right (Left err)) = expectationFailure err >> fail err
requireResult (Left err) = expectationFailure (show err) >> fail (show err)

assertWorking :: Plan.PlanResult -> [(String, String)] -> Expectation
assertWorking result expected = do
  Plan.planIsReady result `shouldBe` True
  show (Plan.prettyPlanResult result) `shouldContain` "Update plan ready"
  Plan.planVersions result `shouldBe` Map.fromList [(name package, version release) | (package, release) <- expected]

assertBlocked :: Plan.PlanResult -> String -> Expectation
assertBlocked result message = do
  Plan.planIsReady result `shouldBe` False
  show (Plan.prettyPlanResult result) `shouldContain` "Blocked update set"
  show (Plan.prettyPlanResult result) `shouldContain` message
  show (Plan.prettyPlanResult result) `shouldContain` "Commit message:"

assertUnchecked :: Plan.PlanResult -> String -> Expectation
assertUnchecked result message = do
  Plan.planIsReady result `shouldBe` True
  show (Plan.prettyPlanResult result) `shouldContain` "Update plan ready (with unchecked packages)"
  show (Plan.prettyPlanResult result) `shouldContain` message
  show (Plan.prettyPlanResult result) `shouldContain` "Commit message:"
