module Main (main) where

import           Test.Tasty

import           Test.Golden
import           Test.IORef.Full
import           Test.IORef.Simple
import           Test.MockFS

main :: IO ()
main = defaultMain $ testGroup "quickcheck-lockstep" [
      Test.Golden.tests
    , Test.IORef.Simple.tests
    , Test.IORef.Full.tests
    , Test.MockFS.tests
    ]
