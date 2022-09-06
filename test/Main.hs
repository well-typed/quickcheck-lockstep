module Main (main) where

import Test.Tasty

import Test.MockFS qualified

main :: IO ()
main = defaultMain $ testGroup "quickcheck-lockstep" [
      Test.MockFS.tests
    ]
