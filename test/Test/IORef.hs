{-# OPTIONS_GHC -Wno-orphans #-}

module Test.IORef (tests) where

import Control.Monad.Reader
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

modelWrite :: MRef -> Int -> M -> (Int, M)
modelWrite v x m = (x, Map.insert v x m)

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
    --
    -- The value is specified either as a concrete value or by reference
    -- to a previous write.
    Write ::
         ModelVar M (IORef Int)
      -> Either Int (ModelVar M Int)
      -> Action (Lockstep M) Int

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
    MInt  :: Int  -> ModelValue M Int

  data Observable M a where
    ORef :: Observable M (IORef Int)
    OId  :: (Show a, Eq a) => a -> Observable M a

  observeModel (MRef  _) = ORef
  observeModel (MInt  x) = OId x

  usedVars New                  = []
  usedVars (Write v (Left _))   = [SomeGVar v]
  usedVars (Write v (Right v')) = [SomeGVar v, SomeGVar v']
  usedVars (Read  v)            = [SomeGVar v]

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
            fmap Some $ Write <$> genVar <*> (Left <$> arbitrary)
          , fmap Some $ Read  <$> genVar
          ]

  shrinkWithVars findVars _mock = \case
      New               -> []
      Write v (Left x)  -> concat [
                               Some . Write v . Left  <$> shrink x
                             , Some . Write v . Right <$> findVars (Proxy @Int)
                             ]
      Write _ (Right _) -> []
      Read  _           -> []

instance RunLockstep M RealMonad where
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

type BrokenRef = (IORef Int, Int)
type RealMonad = ReaderT (IORef (Maybe BrokenRef)) IO

runIO :: Action (Lockstep M) a -> LookUp RealMonad -> RealMonad a
runIO action lookUp = ReaderT $ \brokenRef ->
    case action of
      New       -> newIORef 0
      Write v x -> brokenWrite brokenRef (lookUpRef v) (lookUpInt x)
      Read  v   -> readIORef (lookUpRef v)
  where
    lookUpRef :: ModelVar M (IORef Int) -> IORef Int
    lookUpRef = lookUpGVar (Proxy @RealMonad) lookUp

    lookUpInt :: Either Int (ModelVar M Int) -> Int
    lookUpInt (Left  x) = x
    lookUpInt (Right v) = lookUpGVar (Proxy @RealMonad) lookUp v

brokenWrite :: IORef (Maybe BrokenRef) -> IORef Int -> Int -> IO Int
brokenWrite brokenRef v x = do
    broken <- readIORef brokenRef
    case broken of
      Just (v', x') | v == v', x == x' ->
        writeIORef v 0
      _otherwise -> do
        writeIORef v x
        writeIORef brokenRef (Just (v, x))
    return x

runModel ::
     Action (Lockstep M) a
  -> ModelLookUp M
  -> M -> (ModelValue M a, M)
runModel action lookUp =
    case action of
      New       -> first MRef . modelNew
      Write v x -> first MInt . modelWrite (lookUpRef v) (lookUpInt x)
      Read  v   -> first MInt . modelRead  (lookUpRef v)
  where
    lookUpRef :: ModelVar M (IORef Int) -> MRef
    lookUpRef var = case lookUp var of MRef r -> r

    lookUpInt :: Either Int (ModelVar M Int) -> Int
    lookUpInt (Left  x) = x
    lookUpInt (Right v) = case lookUp v of MInt x -> x

{-------------------------------------------------------------------------------
  Top-level tests
-------------------------------------------------------------------------------}

propIORef :: Actions (Lockstep M) -> Property
propIORef = Lockstep.runActionsBracket (Proxy @M) initBroken (\_ -> return ())
  where
    initBroken :: IO (IORef (Maybe BrokenRef))
    initBroken = newIORef Nothing

tests :: TestTree
tests = testGroup "Test.IORef" [
      testProperty "runActions" propIORef
    ]