module Main (main) where

import Test.Tasty

import Test.IORef qualified
import Test.MockFS qualified

main :: IO ()
main = defaultMain $ testGroup "quickcheck-lockstep" [
      Test.IORef.tests
    , Test.MockFS.tests
    ]
