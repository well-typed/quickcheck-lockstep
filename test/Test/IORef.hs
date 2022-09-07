{-# OPTIONS_GHC -Wno-orphans #-}

module Test.IORef (tests) where

import Data.Bifunctor
import Data.IORef
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Proxy
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)

import Test.QuickCheck.StateModel
import Test.QuickCheck.StateModel.Lockstep
import Test.QuickCheck.StateModel.Lockstep.Defaults qualified as Lockstep
import Test.QuickCheck.StateModel.Lockstep.Run qualified as Lockstep

{-------------------------------------------------------------------------------
  Model "M"
-------------------------------------------------------------------------------}

type MRef = Int
type M    = Map MRef Int

initModel :: M
initModel = Map.empty

modelNew :: M -> (MRef, M)
modelNew m = (mockRef, Map.insert mockRef 0 m)
  where
    mockRef :: MRef
    mockRef = Map.size m

modelWrite :: MRef -> Int -> M -> ((), M)
modelWrite v x m = ((), Map.insert v x m)

modelRead :: MRef -> M -> (Int, M)
modelRead v m = (m Map.! v, m)

{-------------------------------------------------------------------------------
  Instances
-------------------------------------------------------------------------------}

instance StateModel (Lockstep M) where
  data Action (Lockstep M) a where
    -- | Create new IORef
    New :: Action (Lockstep M) (IORef Int)

    -- | Write value to IORef
    Write ::
         ModelVar M (IORef Int)
      -> Int
      -> Action (Lockstep M) ()

    -- | Read IORef
    Read ::
         ModelVar M (IORef Int)
      -> Action (Lockstep M) Int

  initialState    = Lockstep.initialState initModel
  nextState       = Lockstep.nextState
  precondition    = Lockstep.precondition
  arbitraryAction = Lockstep.arbitraryAction
  shrinkAction    = Lockstep.shrinkAction

instance RunModel (Lockstep M) IO where
  perform       = \_state -> runIO
  postcondition = Lockstep.postcondition
  monitoring    = Lockstep.monitoring (Proxy @IO)

instance InLockstep M where
  data ModelValue M a where
    MRef  :: MRef -> ModelValue M (IORef Int)
    MUnit :: ()   -> ModelValue M ()
    MInt  :: Int  -> ModelValue M Int

  data Observable M a where
    ORef :: Observable M (IORef Int)
    OId  :: (Show a, Eq a) => a -> Observable M a

  observeModel (MRef  _) = ORef
  observeModel (MUnit x) = OId x
  observeModel (MInt  x) = OId x

  usedVars New         = []
  usedVars (Write v _) = [SomeGVar v]
  usedVars (Read  v)   = [SomeGVar v]

  modelNextState = runModel

  arbitraryWithVars findVars _mock = oneof $ concat [
        withoutVars
      , case findVars (Proxy @(IORef Int)) of
          []   -> []
          vars -> withVars (elements vars)
      ]
    where
      withoutVars :: [Gen (Any (LockstepAction M))]
      withoutVars = [return $ Some New]

      withVars ::
           Gen (ModelVar M (IORef Int))
        -> [Gen (Any (LockstepAction M))]
      withVars genVar = [
            fmap Some $ Write <$> genVar <*> arbitrary
          , fmap Some $ Read  <$> genVar
          ]

instance RunLockstep M IO where
  observeReal _ action result =
      case (action, result) of
        (New     , _) -> ORef
        (Write{} , x) -> OId x
        (Read{}  , x) -> OId x

deriving instance Show (Action (Lockstep M) a)
deriving instance Show (Observable M a)
deriving instance Show (ModelValue M a)

deriving instance Eq (Action (Lockstep M) a)
deriving instance Eq (Observable M a)
deriving instance Eq (ModelValue M a)

{-------------------------------------------------------------------------------
  Interpreters against the real system and against the model
-------------------------------------------------------------------------------}

runIO :: Action (Lockstep M) a -> LookUp IO -> IO a
runIO action lookUp =
    case action of
      New       -> newIORef 0
      Write v x -> writeIORef (lookUpRef v) x
      Read  v   -> readIORef  (lookUpRef v)
  where
    lookUpRef :: ModelVar M (IORef Int) -> IORef Int
    lookUpRef = lookUpGVar (Proxy @IO) lookUp

runModel ::
     Action (Lockstep M) a
  -> ModelLookUp M
  -> M -> (ModelValue M a, M)
runModel action lookUp =
    case action of
      New       -> first MRef  . modelNew
      Write v x -> first MUnit . modelWrite (lookUpRef v) x
      Read  v   -> first MInt  . modelRead  (lookUpRef v)
  where
    lookUpRef :: ModelVar M (IORef Int) -> MRef
    lookUpRef var = case lookUp var of MRef r -> r

{-------------------------------------------------------------------------------
  Top-level tests
-------------------------------------------------------------------------------}

propIORef :: Actions (Lockstep M) -> Property
propIORef = Lockstep.runActions (Proxy @M)

tests :: TestTree
tests = testGroup "Test.IORef" [
      testProperty "runActions" propIORef
    ]