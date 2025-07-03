{-# LANGUAGE DerivingStrategies #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Test.QuickCheck.StateModel.Lockstep.GVar (tests) where

import           Data.Data (eqT, (:~:) (Refl))
import           Data.Functor.Identity
import           Test.QuickCheck.StateModel.Lockstep.EnvF hiding (shrinkVar)
import           Test.QuickCheck.StateModel.Lockstep.GVar
import           Test.QuickCheck.StateModel.Lockstep.Op
import           Test.QuickCheck.StateModel.Lockstep.Op.SumProd
import           Test.QuickCheck.StateModel.Variables hiding (shrinkVar)
import           Test.Tasty
import           Test.Tasty.QuickCheck

tests :: TestTree
tests = testGroup "Test.Test.QuickCheck.StateModel.Lockstep.GVar" [
      testProperty "prop_shrinkGVar_wellDefined" $
        forAllShrinkShow genEnvironment shrinkEnvironment showEnvironment $ \env ->
        forAllShrink genGVar shrinkGVar' $ \gvar ->
        prop_shrinkGVar_wellDefinedAndEvaluable env gvar
    ]

{-------------------------------------------------------------------------------
  Well-defined and evaluable shrinks
-------------------------------------------------------------------------------}

-- | Verify that 'GVar's are only shrunk to 'GVar's that are well-defined and
-- evaluable wth respect to the environment.
prop_shrinkGVar_wellDefinedAndEvaluable :: EnvF Identity -> GVar Op Int -> Property
prop_shrinkGVar_wellDefinedAndEvaluable env gvar =
    -- It's not important for the test that the input GVar is well-defined, but
    -- we label the test case anyway to see that the input GVar is /sometimes/
    -- well-defined.
    tabulate "Input GVar is well-defined" [show (definedInEnvF env gvar)] $
    tabulate "Number of shrinks" [show (length shrinks)] $
    conjoin [ prop gvar' | gvar' <- shrinks]
  where
    shrinks = shrinkGVar env gvar

    prop gvar' =
      counterexample ("Shrunk GVar is not in the environment: " ++ show gvar')
        (definedInEnvF env gvar')

{-------------------------------------------------------------------------------
  Well-defined and evaluable shrinks: generators, shrinkers, printers
-------------------------------------------------------------------------------}

instance InterpretOp Op Identity where
  intOp op = traverse (intOpId op)

-- NOTE: both the environment and variables contain/are existential types, which
-- can get a little bit complicated when writing generators, shrinkers and
-- printers. Thus for simplicity, we take a dynamic typing approach that throws
-- errors once we encounter a type that we did not expect. It does make
-- functions partial and the code a little bit ugly, but it works well enough
-- for testing well-defined and evaluable shrinks.

-- With just two types of 'Var's, we can test most interesting cases of
-- (non-)well-defined and (non-)successful evaluation.
type T1 = Either Int Char
type T2 = Either Char Int

-- | Generate an environment that maps 'T1' and 'T2' variables to 'T1' and 'T2' values
-- respectively.
genEnvironment :: Gen (EnvF Identity)
genEnvironment = do
    NonNegative (Small n) <- arbitrary
    genInsert empty n
  where
    genInsert :: EnvF Identity -> Int -> Gen (EnvF Identity)
    genInsert acc 0 = pure acc
    genInsert acc n = do
        b <- elements [True, False]
        if b then do
          var <- genVar
          x <- genValue1
          genInsert (insert var (Identity x) acc) (n-1)
        else do
          var <- genVar
          x <- genValue2
          genInsert (insert var (Identity x) acc) (n-1)

shrinkEnvironment :: EnvF Identity -> [EnvF Identity]
shrinkEnvironment (EnvF xs) = EnvF <$> liftShrink shrinkEntry xs

-- | Shrink each entry individually, but also shrink T1 entries towards T2 entries
shrinkEntry :: EnvEntry Identity -> [EnvEntry Identity]
shrinkEntry (EnvEntry (var :: Var a) (Identity x))
    -- A T1 entry
    | Just Refl <- eqT @a @T1
    = -- Shrink either the variable, or shrink the value, or ...
      [ EnvEntry var' (Identity x')
      | (var', x') <- liftShrink2 shrinkVar shrinkValue1 (var, x)
      ] ++
      -- ... shrink towards a T2 entry
      [ EnvEntry (unsafeCoerceVar var :: Var T2) (Identity x')
      | x' <- case x of
                Left y  -> [Right y]
                Right{} -> []
      ]
    -- A T2 entry
    | Just Refl <- eqT @a @T2
    =  -- Shrink either the variable, or shrink the value
      [ EnvEntry var' (Identity x')
      | (var', x') <- liftShrink2 shrinkVar shrinkValue2 (var, x)
      ]
    | otherwise
    = error "shrinkEntry"

showEnvironment :: EnvF Identity -> String
showEnvironment (EnvF xs) = "EnvF " ++ "[" ++ concatMap showEntry xs ++ "]"

showEntry :: EnvEntry Identity -> String
showEntry (EnvEntry (var :: Var a) x)
    | Just Refl <- eqT @a @T1
    = "EnvEntry " ++ show var ++ " (" ++ show x ++ ")"
    | Just Refl <- eqT @a @T2
    = "EnvEntry " ++ show var ++ " (" ++ show x ++ ")"
    | otherwise
    = error "showEntry"

-- | Generate a T1 or T2 variable
genGVar :: Gen (GVar Op Int)
genGVar = oneof [
      do var <- genVar
         pure (unsafeMkGVar (var :: Var T1) OpLeft)
    , do var <- genVar
         pure (unsafeMkGVar (var :: Var T2) OpRight)
    ]

shrinkGVar' :: GVar Op Int -> [GVar Op Int]
shrinkGVar' (GVar (var :: Var a) op)
  | Just Refl <- eqT @a @T1
  = [ unsafeMkGVar var' op
    | var' <- shrinkVar var
    ] ++ [
      unsafeMkGVar (unsafeCoerceVar var :: Var T2) OpRight
    ]
  | Just Refl <- eqT @a @T2
  = [ unsafeMkGVar var' op
    | var' <- shrinkVar var
    ]
  | otherwise
  = error "shrink'GVar"

genVar :: Gen (Var a)
genVar = mkVar <$> arbitrary

shrinkVar :: Var a -> [Var a]
shrinkVar var = [ mkVar x' | x' <- shrink x]
  where
    -- Hacky: the constructor for 'Var' is not exposed, but we can obtain the
    -- wrapped integer by reading it from the show output.
    --
    -- The show instance looks like:
    --
    -- > data Var = Var Int
    -- > show (Var x) = "var" ++ show x
    x = read (drop 3 (show var))

genValue1 :: Gen T1
genValue1 = arbitrary

shrinkValue1 :: T1 -> [T1]
shrinkValue1 = shrink

genValue2 :: Gen T2
genValue2 = arbitrary

shrinkValue2 :: T2 -> [T2]
shrinkValue2 = shrink
