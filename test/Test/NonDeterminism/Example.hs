{-# LANGUAGE DerivingStrategies #-}

module Test.NonDeterminism.Example where

import           System.Directory
import           System.Random

data T = A | B
  deriving stock (Show, Eq, Read)

set :: FilePath -> T -> IO ()
set path x = writeFile path (show x)

get :: FilePath -> IO T
get path = do
  b <- doesFileExist path
  if b
  then read <$> readFile path
  else pure A

setRandom :: FilePath -> IO ()
setRandom path = do
  g <- getStdGen
  let (aorb, g') = uniform g
  setStdGen g'
  set path (if aorb then A else B)
