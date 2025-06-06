{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE InstanceSigs       #-}
{-# LANGUAGE ViewPatterns       #-}

{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}

module Test.NonDeterminism where

import           Control.Monad.Reader
import           Data.Bifunctor
import           Data.Maybe
import           Data.Proxy
import           System.Directory
import           System.FilePath
import           System.IO.Temp
import           Test.NonDeterminism.Example
import           Test.QuickCheck.StateModel
import           Test.QuickCheck.StateModel.Lockstep
import qualified Test.QuickCheck.StateModel.Lockstep.Defaults as Lockstep
import qualified Test.QuickCheck.StateModel.Lockstep.Run      as Lockstep
import           Test.Tasty
import           Test.Tasty.QuickCheck

tests :: TestTree
tests = testGroup "Test.NonDeterminism" [
      testProperty "propLockstep" propLockstep
    ]

propLockstep :: Actions (Lockstep Model) -> Property
propLockstep =
    Lockstep.runActionsBracket
      (Proxy @Model)
      acq
      rel
      runReaderT
  where
    acq = do
        sysTmpDir <- getCanonicalTemporaryDirectory
        createTempDirectory sysTmpDir "propLockstep_NonDeterminism"

    rel = removeDirectoryRecursive

{-------------------------------------------------------------------------------
  Model
-------------------------------------------------------------------------------}

newtype Model = Model (Maybe T)
  deriving stock Show

mEmpty :: Model
mEmpty = Model Nothing

getT :: Model -> T
getT (Model m) = fromMaybe A m

mSet :: T -> Model -> ((), Model)
mSet x (Model _) = ((), Model (Just x))

mGet :: Model -> (T, Model)
mGet m = (getT m, m)

mSetRandom :: Model -> ((), Model)
mSetRandom (Model x) = ((), Model x)

{-------------------------------------------------------------------------------
  StateModel
-------------------------------------------------------------------------------}

instance StateModel (Lockstep Model) where
  data instance Action (Lockstep Model) a where
    Get :: Action (Lockstep Model) T
    Set :: T -> Action (Lockstep Model) ()
    SetRandom :: Action (Lockstep Model) ()

  initialState = Lockstep.initialState mEmpty
  nextState = Lockstep.nextState
  precondition = Lockstep.precondition
  arbitraryAction = Lockstep.arbitraryAction
  shrinkAction = Lockstep.shrinkAction

deriving stock instance Show (Action (Lockstep Model) a)
deriving stock instance Eq (Action (Lockstep Model) a)

instance RunModel (Lockstep Model) (ReaderT FilePath IO) where
  perform _ action _lookup = ReaderT $ \((</> "example") -> path) -> case action of
      Get       -> get path
      Set x     -> set path x
      SetRandom -> setRandom path
  postcondition = Lockstep.postcondition
  monitoring = Lockstep.monitoring (Proxy @(ReaderT FilePath IO))


{-------------------------------------------------------------------------------
  Lockstep
-------------------------------------------------------------------------------}

instance InLockstep Model where
  data instance ModelValue Model a where
    MT :: T -> ModelValue Model T
    MUnit :: () -> ModelValue Model ()

  data instance Observable Model a where
    OT :: T -> Observable Model T
    OUnit :: () -> Observable Model ()

  observeModel :: ModelValue Model a -> Observable Model a
  observeModel (MT x)    = OT x
  observeModel (MUnit x) = OUnit x

  usedVars _ = []

  modelNextState action _ctx m = case action of
      Get       -> first MT $ mGet m
      Set x     -> first MUnit $ mSet x m
      SetRandom -> first MUnit $ mSetRandom m

  arbitraryWithVars _ctx _m = oneof [
      pure $ Some Get
    , Some . Set <$> elements [A, B]
    , pure $ Some SetRandom
    ]

deriving stock instance Show (ModelValue Model a)
deriving stock instance Show (Observable Model a)
deriving stock instance Eq (Observable Model a)

instance RunLockstep Model (ReaderT FilePath IO) where
  observeReal _ action x = case action of
      Get       -> OT x
      Set{}     -> OUnit x
      SetRandom -> OUnit x
