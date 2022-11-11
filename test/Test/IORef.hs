{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.IORef (tests) where

import Control.Monad.Reader
import Data.Bifunctor
import Data.Constraint (Dict(..))
import Data.IORef
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Proxy
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (testProperty)

import Test.QuickCheck qualified as QC

import Test.QuickCheck.StateModel
import Test.QuickCheck.StateModel.Lockstep
import Test.QuickCheck.StateModel.Lockstep.Defaults qualified as Lockstep
import Test.QuickCheck.StateModel.Lockstep.Run qualified as Lockstep
import Test.QuickCheck.StateModel.Lockstep.Run (labelActions)

{-------------------------------------------------------------------------------
  Model "M"
-------------------------------------------------------------------------------}

type MRef = Int

data M = M {
      -- | Value of every var
      mValues :: Map MRef Int

      -- | How many writes did we do for each var?
      --
      -- This is used for tagging.
    , mWrites :: Map MRef Int
    }
  deriving (Show)

initModel :: M
initModel = M {
      mValues = Map.empty
    , mWrites = Map.empty
    }

modelNew :: M -> (MRef, M)
modelNew M{..} = (
      mockRef
    , M { mValues = Map.insert mockRef 0 mValues
        , mWrites = Map.insert mockRef 0 mWrites
        }
    )
  where
    mockRef :: MRef
    mockRef = Map.size mValues

modelWrite :: MRef -> Int -> M -> (Int, M)
modelWrite v x M{..} = (
      x
    , M { mValues = Map.insert v x mValues
        , mWrites = Map.adjust succ v mWrites
        }
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
            fmap Some $ Write <$> genVar <*> (Left <$> choose (0, 10))
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

  tagStep (_stBefore, stAfter) _action _result = concat [
        [ "WriteSameVarTwice"
        | not
        . Map.null
        . Map.filter (\numWrites -> numWrites > 1)
        $ mWrites stAfter
        ]
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

deriving instance Show (Action (Lockstep M) a)
deriving instance Show (Observable M a)
deriving instance Show (ModelValue M a)

deriving instance Eq (Action (Lockstep M) a)
deriving instance Eq (Observable M a)
deriving instance Eq (ModelValue M a)

{-------------------------------------------------------------------------------
  Interpreters against the real system and against the model
-------------------------------------------------------------------------------}

data Buggy = Buggy | NotBuggy
  deriving (Show, Eq)

instance Arbitrary Buggy where
  arbitrary = elements [Buggy, NotBuggy]

  shrink Buggy    = [NotBuggy]
  shrink NotBuggy = []

type BrokenRef = (IORef Int, Int)
type RealMonad = ReaderT (Buggy, IORef (Maybe BrokenRef)) IO

runIO :: Action (Lockstep M) a -> LookUp RealMonad -> RealMonad a
runIO action lookUp = ReaderT $ \(buggy, brokenRef) ->
    case action of
      New       -> newIORef 0
      Write v x -> brokenWrite buggy brokenRef (lookUpRef v) (lookUpInt x)
      Read  v   -> readIORef (lookUpRef v)
  where
    lookUpRef :: ModelVar M (IORef Int) -> IORef Int
    lookUpRef = lookUpGVar (Proxy @RealMonad) lookUp

    lookUpInt :: Either Int (ModelVar M Int) -> Int
    lookUpInt (Left  x) = x
    lookUpInt (Right v) = lookUpGVar (Proxy @RealMonad) lookUp v

-- | The second write to the same variable will be broken
brokenWrite :: Buggy -> IORef (Maybe BrokenRef) -> IORef Int -> Int -> IO Int
brokenWrite NotBuggy _         v x = writeIORef v x >> return x
brokenWrite Buggy    brokenRef v x = do
    broken <- readIORef brokenRef
    case broken of
      Just (v', x') | v == v', x == x' ->
        writeIORef v 123456789
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

propIORef :: Buggy -> Actions (Lockstep M) -> Property
propIORef buggy actions =
      (if triggersBug then expectFailure  else id)
    $ Lockstep.runActionsBracket (Proxy @M) initBroken (\_ -> return ()) actions
  where
    triggersBug :: Bool
    triggersBug =
           buggy == Buggy
        && "WriteSameVarTwice" `elem` labelActions actions

    initBroken :: IO (Buggy, IORef (Maybe BrokenRef))
    initBroken = (buggy,) <$> newIORef Nothing

tests :: TestTree
tests = testGroup "Test.IORef" [
      testCase "labelledExamples" $
        QC.labelledExamples $ Lockstep.tagActions (Proxy @M)
    , testProperty "runActions" propIORef
    ]