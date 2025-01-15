{- HLINT ignore "Use camelCase" -}

module Test.Golden where

import           Control.Exception      (bracket_)
import           System.Directory
import           System.FilePath
import           Test.MockFS            as MockFS
import           Test.QuickCheck
import           Test.QuickCheck.Random (mkQCGen)
import           Test.Tasty
import           Test.Tasty.Golden

tests :: TestTree
tests = testGroup "Test.Golden" [
      golden_success_propLockstep_MockFS
    , golden_failure_default_propLockstep_MockFS
    , golden_failure_nonVerbose_propLockstep_MockFS
    , golden_failure_verbose_propLockstep_MockFS
    ]

goldenDir :: FilePath
goldenDir = "test" </> "golden"

tmpDir :: FilePath
tmpDir = "_tmp"

-- | Golden test for a successful lockstep test
golden_success_propLockstep_MockFS :: TestTree
golden_success_propLockstep_MockFS =
    goldenVsFile testName goldenPath outputPath $ do
      createDirectoryIfMissing False tmpDir
      r <- quickCheckWithResult args MockFS.propLockstep
      writeFile outputPath (output r)
  where
    testName = "golden_success_propLockstep_MockFS"
    goldenPath = goldenDir </> testName <.> "golden"
    outputPath = tmpDir </> testName <.> "golden"

    args = stdArgs {
        replay = Just (mkQCGen 17, 32)
      , chatty = False
      }

-- | Golden test for a failing lockstep test that produces a counterexample
-- using the default postcondition.
golden_failure_default_propLockstep_MockFS :: TestTree
golden_failure_default_propLockstep_MockFS =
    goldenVsFile testName goldenPath outputPath $ do
      createDirectoryIfMissing False tmpDir
      r <-
        bracket_
          MockFS.setInduceFault
          MockFS.setNoInduceFault
          (quickCheckWithResult args MockFS.propLockstep)
      writeFile outputPath (output r)
  where
    testName = "golden_failure_default_propLockstep_MockFS"
    goldenPath = goldenDir </> testName <.> "golden"
    outputPath = tmpDir </> testName <.> "golden"

    args = stdArgs {
        replay = Just (mkQCGen 17, 32)
      , chatty = False
      }

-- | Golden test for a failing lockstep test that produces a /non-verbose/ counterexample
golden_failure_nonVerbose_propLockstep_MockFS :: TestTree
golden_failure_nonVerbose_propLockstep_MockFS =
    goldenVsFile testName goldenPath outputPath $ do
      createDirectoryIfMissing False tmpDir
      r <-
        bracket_
          (MockFS.setInduceFault >> MockFS.setPostconditionNonVerbose)
          (MockFS.setNoInduceFault >> MockFS.setPostconditionDefault)
          (quickCheckWithResult args MockFS.propLockstep)
      writeFile outputPath (output r)
  where
    testName = "golden_failure_nonVerbose_propLockstep_MockFS"
    goldenPath = goldenDir </> testName <.> "golden"
    outputPath = tmpDir </> testName <.> "golden"

    args = stdArgs {
        replay = Just (mkQCGen 17, 32)
      , chatty = False
      }

-- | Golden test for a failing lockstep test that produces a /verbose/ counterexample
golden_failure_verbose_propLockstep_MockFS :: TestTree
golden_failure_verbose_propLockstep_MockFS =
    goldenVsFile testName goldenPath outputPath $ do
      createDirectoryIfMissing False tmpDir
      r <-
        bracket_
          (MockFS.setInduceFault >> MockFS.setPostconditionVerbose)
          (MockFS.setNoInduceFault >> MockFS.setPostconditionDefault)
          (quickCheckWithResult args MockFS.propLockstep)
      writeFile outputPath (output r)
  where
    testName = "golden_failure_verbose_propLockstep_MockFS"
    goldenPath = goldenDir </> testName <.> "golden"
    outputPath = tmpDir </> testName <.> "golden"

    args = stdArgs {
        replay = Just (mkQCGen 17, 32)
      , chatty = False
      }
