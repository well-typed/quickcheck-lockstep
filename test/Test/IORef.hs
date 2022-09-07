{-# OPTIONS_GHC -Wno-orphans #-}

module Test.IORef (tests) where

import Data.Bifunctor
import Data.IORef
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Proxy
import Test.QuickCheck (Gen, Property)
import Test.QuickCheck qualified as QC
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)

import Test.QuickCheck.StateModel
import Test.QuickCheck.StateModel.Lockstep
import Test.QuickCheck.StateModel.Lockstep.Defaults qualified as Lockstep
import Test.QuickCheck.StateModel.Lockstep.Run qualified as Lockstep

{-------------------------------------------------------------------------------
  Model
-------------------------------------------------------------------------------}

type MockVar = Int
type Model   = Map MockVar Int

initModel :: Model
initModel = Map.empty

modelNew :: Model -> (MockVar, Model)
modelNew m = (mockRef, Map.insert mockRef 0 m)
  where
    mockRef :: MockVar
    mockRef = Map.size m

modelIncr :: MockVar -> Model -> ((), Model)
modelIncr v m = ((), Map.adjust (+ 1) v m)

modelRead :: MockVar -> Model -> (Int, Model)
modelRead v m = (m Map.! v, m)

{-------------------------------------------------------------------------------
  Instances
-------------------------------------------------------------------------------}

instance StateModel (Lockstep Model) where
  data Action (Lockstep Model) a where
    New  ::                               Action (Lockstep Model) (IORef Int)
    Incr :: ModelVar Model (IORef Int) -> Action (Lockstep Model) ()
    Read :: ModelVar Model (IORef Int) -> Action (Lockstep Model) Int

  initialState    = Lockstep.initialState initModel
  nextState       = Lockstep.nextState
  precondition    = Lockstep.precondition
  arbitraryAction = Lockstep.arbitraryAction
  shrinkAction    = Lockstep.shrinkAction

instance RunModel (Lockstep Model) IO where
  perform       = \_state -> runIO
  postcondition = Lockstep.postcondition
  monitoring    = Lockstep.monitoring (Proxy @IO)

instance InLockstep Model where
  data ModelValue Model a where
    MRef  :: MockVar -> ModelValue Model (IORef Int)
    MUnit :: ()      -> ModelValue Model ()
    MInt  :: Int     -> ModelValue Model Int

  data Observable Model a where
    ORef :: Observable Model (IORef Int)
    OId  :: (Show a, Eq a) => a -> Observable Model a

  observeModel (MRef  _) = ORef
  observeModel (MUnit x) = OId x
  observeModel (MInt  x) = OId x

  usedVars New      = []
  usedVars (Incr v) = [SomeGVar v]
  usedVars (Read v) = [SomeGVar v]

  modelNextState = runModel

  arbitraryWithVars findVars _mock = QC.oneof $ concat [
        withoutVars
      , case findVars (Proxy @(IORef Int)) of
          []   -> []
          vars -> withVars (QC.elements vars)
      ]
    where
      withoutVars :: [Gen (Any (LockstepAction Model))]
      withoutVars = [return $ Some New]

      withVars ::
           Gen (ModelVar Model (IORef Int))
        -> [Gen (Any (LockstepAction Model))]
      withVars genVar = [
            Some . Incr <$> genVar
          , Some . Read <$> genVar
          ]

instance RunLockstep Model IO where
  observeReal _ action result =
      case (action, result) of
        (New    , _) -> ORef
        (Incr{} , x) -> OId x
        (Read{} , x) -> OId x

deriving instance Show (Action (Lockstep Model) a)
deriving instance Show (Observable Model a)
deriving instance Show (ModelValue Model a)

deriving instance Eq (Action (Lockstep Model) a)
deriving instance Eq (Observable Model a)
deriving instance Eq (ModelValue Model a)

{-------------------------------------------------------------------------------
  Interpreters against the real system and against the model
-------------------------------------------------------------------------------}

runIO :: Action (Lockstep Model) a -> LookUp IO -> IO a
runIO action lookUp =
    case action of
      New    -> newIORef 0
      Incr v -> modifyIORef (lookUpRef v) (+ 1)
      Read v -> readIORef   (lookUpRef v)
  where
    lookUpRef :: ModelVar Model (IORef Int) -> IORef Int
    lookUpRef = lookUpGVar (Proxy @IO) lookUp

runModel ::
     Action (Lockstep Model) a
  -> ModelLookUp Model
  -> Model -> (ModelValue Model a, Model)
runModel action lookUp =
    case action of
      New    -> first MRef  . modelNew
      Incr v -> first MUnit . modelIncr (lookUpRef v)
      Read v -> first MInt  . modelRead (lookUpRef v)
  where
    lookUpRef :: ModelVar Model (IORef Int) -> MockVar
    lookUpRef var = case lookUp var of MRef r -> r

{-------------------------------------------------------------------------------
  Top-level tests
-------------------------------------------------------------------------------}

propIORef :: Actions (Lockstep Model) -> Property
propIORef = Lockstep.runActions (Proxy @Model)

tests :: TestTree
tests = testGroup "Test.IORef" [
      testProperty "runActions" propIORef
    ]