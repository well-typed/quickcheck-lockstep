{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.IORef.Simple (tests) where

import           Data.Bifunctor
import           Data.Constraint (Dict (..))
import           Data.IORef
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Proxy
import           Test.QuickCheck (Gen, Property, choose, elements, oneof)
import           Test.Tasty
import           Test.Tasty.QuickCheck (testProperty)

import           Test.QuickCheck.StateModel
import           Test.QuickCheck.StateModel.Lockstep
import qualified Test.QuickCheck.StateModel.Lockstep.Defaults as Lockstep
import qualified Test.QuickCheck.StateModel.Lockstep.Run as Lockstep

{-------------------------------------------------------------------------------
  Model "M"
-------------------------------------------------------------------------------}

type MRef = Int

newtype M = M {
      -- | Value of every var
      mValues :: Map MRef Int
    }
  deriving stock (Show)

initModel :: M
initModel = M { mValues = Map.empty }

modelNew :: M -> (MRef, M)
modelNew M{..} = (
      mockRef
    , M { mValues = Map.insert mockRef 0 mValues }
    )
  where
    mockRef :: MRef
    mockRef = Map.size mValues

modelWrite :: MRef -> Int -> M -> ((), M)
modelWrite v x M{..} = (
      ()
    , M { mValues = Map.insert v x mValues }
    )

modelRead :: MRef -> M -> (Int, M)
modelRead v m@M{..} = (mValues Map.! v, m)

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

instance RunModel (Lockstep M) RealMonad where
  perform       = \_state -> runIO
  postcondition = Lockstep.postcondition
  monitoring    = Lockstep.monitoring (Proxy @RealMonad)

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

  modelNextState action ctx = runModel action (lookupVar ctx)

  arbitraryWithVars ctx _mock = oneof $ concat [
        withoutVars
      , case findVars ctx (Proxy @(IORef Int)) of
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
            fmap Some $ Write <$> genVar <*> choose (0, 10)
          , fmap Some $ Read  <$> genVar
          ]

instance RunLockstep M RealMonad where
  observeReal _ action result =
      case (action, result) of
        (New     , _) -> ORef
        (Write{} , x) -> OId x
        (Read{}  , x) -> OId x

  showRealResponse _ = \case
    New{}   -> Nothing
    Write{} -> Just Dict
    Read{}  -> Just Dict

deriving stock instance Show (Action (Lockstep M) a)
deriving stock instance Show (Observable M a)
deriving stock instance Show (ModelValue M a)

deriving stock instance Eq (Action (Lockstep M) a)
deriving stock instance Eq (Observable M a)
deriving stock instance Eq (ModelValue M a)

{-------------------------------------------------------------------------------
  Interpreters against the real system and against the model
-------------------------------------------------------------------------------}

type RealMonad = IO

runIO :: Action (Lockstep M) a -> LookUp -> RealMonad a
runIO action lookUp =
    case action of
      New       -> newIORef 0
      Write v x -> writeIORef (lookUpRef v) x
      Read  v   -> readIORef (lookUpRef v)
  where
    lookUpRef :: ModelVar M (IORef Int) -> IORef Int
    lookUpRef = realLookupVar lookUp

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
tests = testGroup "Test.IORef.Simple" [
      testProperty "runActions" propIORef
    ]
