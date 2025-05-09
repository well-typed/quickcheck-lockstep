module Main (main) where

import           Test.Tasty
import qualified Test.Test.QuickCheck.StateModel.Lockstep.GVar

main :: IO ()
main = defaultMain $ testGroup "quickcheck-lockstep-internals" [
      Test.Test.QuickCheck.StateModel.Lockstep.GVar.tests
    ]
