module Main (main) where

import Test.Tasty

import Test.IORef.Full   qualified
import Test.IORef.Simple qualified
import Test.MockFS       qualified

main :: IO ()
main = defaultMain $ testGroup "quickcheck-lockstep" [
      Test.IORef.Simple.tests
    , Test.IORef.Full.tests
    , Test.MockFS.tests
    ]
