{-# LANGUAGE TypeApplications #-}

module PlanSpec (spec) where

import Control.Monad (forM_)
import qualified Data.ByteString.Char8 as B8
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
      last (lines output) `shouldBe` "alpha 2.0, bravo 2.0"

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

    it "does not duplicate semantically equivalent ranges" $ do
      let specs range = [("alpha", [], [("2.0", lib ["bravo " <> range])]), ("bravo", [], [])]
      result <- runRevisionPlan False [("alpha", Just "2.0")] (specs "<3") (specs ">=0 && <3")
      assertWorking result [("alpha", "2.0")]
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
      ([("ghc", Nothing)], "toolchain")
    ] $ \(targets, message) ->
      it ("rejects invalid plan request: " <> message) $ do
        let (extra, raw) = fixture [("alpha", [], [])]
        result <- runDB True targets extra raw
        case result of
          Right (Left err) -> err `shouldContain` message
          _ -> expectationFailure "expected invalid plan request to be rejected"

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
runDBWithRevisions original flags solve targets extra raw =
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
      planned <- Plan.planUpdates solve [(name package, version <$> candidate) | (package, candidate) <- targets]
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
