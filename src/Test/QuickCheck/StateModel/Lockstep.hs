-- | Lockstep-style testing using @quickcheck-dynamic@
--
-- Intended for qualified import.
--
-- > import Test.QuickCheck.StateModel.Lockstep (Lockstep(..))
-- > import Test.QuickCheck.StateModel.Lockstep qualified as Lockstep
module Test.QuickCheck.StateModel.Lockstep (
    -- * Main abstraction
    Lockstep -- opaque
  , InLockstep(..)
  , RunLockstep(..)
    -- * Convenience aliases
  , LockstepAction
  , ModelFindVariables
  , ModelLookUp
  , ModelVar
    -- * Default implementations for methods of 'StateModel'
  , Test.QuickCheck.StateModel.Lockstep.initialState
  , Test.QuickCheck.StateModel.Lockstep.nextState
  , Test.QuickCheck.StateModel.Lockstep.precondition
  , Test.QuickCheck.StateModel.Lockstep.arbitraryAction
  , Test.QuickCheck.StateModel.Lockstep.shrinkAction
    -- * Default implementations for methods of 'RunModel'
  , Test.QuickCheck.StateModel.Lockstep.postcondition
  , Test.QuickCheck.StateModel.Lockstep.monitoring
    -- * Running tests
  , tagActions
  , runActionsBracket
  ) where

import Prelude hiding (init)

import Control.Exception
import Control.Monad.Reader
import Data.Kind
import Data.Maybe (isNothing)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Typeable
import Test.QuickCheck.Monadic

import Test.QuickCheck (Gen, Property, Testable)
import Test.QuickCheck qualified as QC
import Test.QuickCheck.StateModel

import Test.QuickCheck.StateModel.Lockstep.EnvF (EnvF)
import Test.QuickCheck.StateModel.Lockstep.EnvF qualified as EnvF
import Test.QuickCheck.StateModel.Lockstep.GVar (GVar, AnyGVar(..), InterpretOp, Op)
import Test.QuickCheck.StateModel.Lockstep.GVar qualified as GVar

{-------------------------------------------------------------------------------
  Lockstep state

  @quickcheck-dynamic@ takes care of keeping track of the responses of the
  system under test, but not the model. We do that here.

  Implementation note: the 'RunModel' class in @quickcheck-dynamic@ uses a type
  family 'Realized': for an @Action state a@, the response from the real system
  is expected to be of type @Realized m a@. This allows the same tests to be run
  against different "test execution backends"; for example, we could run the
  tests in the real IO monad, or using an IO monad simulator.

  This is an orthogonal generalization to what Lockstep provides: no matter the
  test execution backend, the /model/ will always be the same. We could perhaps
  piggy-back on the same abstraction if we introduced a separate monad parameter
  @n@ for the model, and then use @Realized n a@ instead of @ModelValue a@. This
  might work, but it's less clear how to then also that for 'Observable'.
  Overall, it seems cleaner to reserve 'Realized' exclusively for the
  parameterization over test execution backends.
-------------------------------------------------------------------------------}

data Lockstep state = Lockstep {
      lockstepModel :: state
    , lockstepEnv   :: EnvF (ModelValue state)
    }
  deriving (Show)

{-------------------------------------------------------------------------------
  Main lockstep abstraction
-------------------------------------------------------------------------------}

class
     ( StateModel (Lockstep state)
     , Typeable state
     , InterpretOp (ModelOp state) (ModelValue state)
     , forall a. Show (ModelValue state a)
     , forall a. Eq   (Observable state a)
     , forall a. Show (Observable state a)
     )
  => InLockstep state where
  -- | Values in the mock environment
  --
  -- 'ModelValue' witnesses the relation between values returned by the real
  -- system and values returned by the model.
  --
  -- In most cases, we expect the real system and the model to return the
  -- /same/ value. However, for some things we must allow them to diverge:
  -- consider file handles for example.
  data ModelValue state a

  -- | Observable responses
  --
  -- The real system returns values of type @a@, and the model returns values
  -- of type @MockValue a@. @Observable a@ defines the parts of those results
  -- that expect to be the /same/ for both.
  data Observable state a

  -- | Type of operations required on the results of actions
  --
  -- Whenever an action has a result of type @a@, but we later need a variable
  -- of type @b@, we need a constructor
  --
  -- > GetB :: ModelOp state a b
  --
  -- in the 'ModelOp' type. For many tests, the standard 'Op' type will
  -- suffice, but not always.
  type ModelOp state :: Type -> Type -> Type
  type ModelOp state = Op

  -- | Extract the observable part of a response from the model
  observeModel :: ModelValue state a-> Observable state a

  -- | All variables required by a command
  usedVars :: LockstepAction state a -> [AnyGVar (ModelOp state)]

  -- | Step the model
  modelNextState ::
       ModelLookUp state
    -> LockstepAction state a
    -> state
    -> (ModelValue state a, state)

  arbitraryWithVars ::
       ModelFindVariables state
    -> state
    -> Gen (Any (LockstepAction state))

  shrinkWithVars ::
       ModelFindVariables state
    -> state
    -> LockstepAction state a
    -> [Any (LockstepAction state)]

  tagStep ::
       (state, state)
    -> LockstepAction state a
    -> ModelValue state a
    -> [String]

class ( InLockstep state
      , RunModel (Lockstep state) m
      ) => RunLockstep state m where
  -- See also 'Observable'
  observeReal ::
       proxy m
    -> LockstepAction state a -> Realized m a -> Observable state a

{-------------------------------------------------------------------------------
  Convenience aliases
-------------------------------------------------------------------------------}

-- | An action in the lock-step model
type LockstepAction state = Action (Lockstep state)

-- | Look up a variable for model execution
--
-- The type of the variable is the type in the /real/ system.
type ModelLookUp state = forall a.
          (Show a, Typeable a, Eq a)
       => ModelVar state a -> ModelValue state a

-- | Pick variable of the appropriate type
--
-- The type you pass must be the result type of (previously executed) actions.
-- If you want to change the type of the variable, see
-- 'StateModel.Lockstep.GVar.map'.
type ModelFindVariables state = forall proxy a.
          (Show a, Typeable a, Eq a)
       => proxy a -> [GVar (ModelOp state) a]

-- | Variables with a "functor-esque" instance
type ModelVar state = GVar (ModelOp state)

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

varsOfType ::
     InLockstep state
  => EnvF (ModelValue state) -> ModelFindVariables state
varsOfType env p = map GVar.fromVar $ EnvF.keysOfType p env

-- | Check the response of the system under test against the model
--
-- This is used in 'postcondition', where we can however only return a 'Bool',
-- and in 'monitoring', to give the user more detailed feedback.
checkResponse :: forall proxy m state a.
     RunLockstep state m
  => proxy m
  -> Lockstep state -> LockstepAction state a -> Realized m a -> Maybe String
checkResponse p (Lockstep state env) action a =
    compareEquality (observeReal p action a) (observeModel modelResp)
  where
    modelResp :: ModelValue state a
    modelResp = fst $ modelNextState (GVar.lookUpEnvF env) action state

    compareEquality ::  Observable state a -> Observable state a -> Maybe String
    compareEquality real mock
      | real == mock = Nothing
      | otherwise    = Just $ concat [
            "System under test returned "
          , show real
          , " but model returned "
          , show mock
          ]

{-------------------------------------------------------------------------------
  Default implementations for members of 'StateModel'
-------------------------------------------------------------------------------}

-- | Default implementation for 'Test.QuickCheck.StateModel.initialState'
initialState :: state -> Lockstep state
initialState state = Lockstep {
      lockstepModel = state
    , lockstepEnv   = EnvF.empty
    }

-- | Default implementation for 'Test.QuickCheck.StateModel.nextState'
nextState :: forall state a.
     (InLockstep state, Typeable a)
  => Lockstep state
  -> LockstepAction state a
  -> Var a
  -> Lockstep state
nextState (Lockstep state env) action var =
    Lockstep state' $ EnvF.insert var modelResp env
  where
    modelResp :: ModelValue state a
    state'    :: state
    (modelResp, state') = modelNextState (GVar.lookUpEnvF env) action state

-- | Default implementation for 'Test.QuickCheck.StateModel.precondition'
--
-- The default precondition only checks that all variables have a value
-- and that the operations on them are defined.
precondition ::
     InLockstep state
  => Lockstep state -> LockstepAction state a -> Bool
precondition (Lockstep _ env) =
    all (\(SomeGVar var) -> GVar.definedInEnvF env var) . usedVars

-- | Default implementation for 'Test.QuickCheck.StateModel.arbitraryAction'
arbitraryAction ::
     InLockstep state
  => Lockstep state -> Gen (Any (LockstepAction state))
arbitraryAction (Lockstep state env) =
    arbitraryWithVars (varsOfType env) state

-- | Default implementation for 'Test.QuickCheck.StateModel.shrinkAction'
shrinkAction ::
     InLockstep state
  => Lockstep state
  -> LockstepAction state a -> [Any (LockstepAction state)]
shrinkAction (Lockstep state env) =
    shrinkWithVars (varsOfType env) state

{-------------------------------------------------------------------------------
  Default implementations for methods of 'RunModel'
-------------------------------------------------------------------------------}

-- | Default implementation for 'Test.QuickCheck.StateModel.postcondition'
--
-- The default postcondition verifies that the real system and the model
-- return the same results, up to " observability ".
postcondition :: forall m state a.
     RunLockstep state m
  => (Lockstep state, Lockstep state)
  -> LockstepAction state a
  -> LookUp m
  -> Realized m a
  -> m Bool
postcondition (before, _after) action _lookUp a =
    pure $ isNothing $ checkResponse (Proxy @m) before action a

monitoring :: forall proxy m state a.
     RunLockstep state m
  => proxy m
  -> (Lockstep state, Lockstep state)
  -> LockstepAction state a
  -> LookUp m
  -> Realized m a
  -> Property -> Property
monitoring p (before, after) action _lookUp realResp =
      maybe id QC.counterexample (checkResponse p before action realResp)
    . QC.counterexample ("State: " ++ show after)
    . QC.tabulate "Tags" tags
  where
    tags :: [String]
    tags = tagStep (lockstepModel before, lockstepModel after) action modelResp

    modelResp :: ModelValue state a
    modelResp = fst $ modelNextState
                        (GVar.lookUpEnvF $ lockstepEnv before)
                        action
                        (lockstepModel before)

{-------------------------------------------------------------------------------
  Finding labelled examples
-------------------------------------------------------------------------------}

-- | Tag a list of actions
--
-- This is primarily useful for use with QuickCheck's 'labelledExamples':
-- the 'monitoring' hook from 'StateModel' is not useful here, because it will
-- be called multiple times during test execution, which means we must use it
-- with 'tabulate', not 'label'; but 'tabulate' is not supported by
-- 'labelledExamples'.
--
-- So, here we run through the actions independent from 'StateModel', collecting
-- all tags, and then finish on a /single/ call to 'label' at the end with all
-- collected tags.
--
-- The other advantage of this function over 'runAction' is that we do not need
-- a test runner here: this uses the model /only/.
tagActions :: forall proxy state.
     InLockstep state
  => proxy state
  -> Actions (Lockstep state)
  -> Property
tagActions _proxy (Actions steps) =
    go Set.empty Test.QuickCheck.StateModel.initialState steps
  where
    go :: Set String -> Lockstep state -> [Step (Lockstep state)] -> Property
    go tags _st []            = QC.label ("Tags: " ++ show (Set.toList tags)) True
    go tags  st ((v:=a) : ss) = go' tags st v a ss

    go' :: forall a.
         Typeable a
      => Set String                 -- accumulated set of tags
      -> Lockstep state             -- current state
      -> Var a                      -- variable for the result of this action
      -> Action (Lockstep state) a  -- action to execute
      -> [Step (Lockstep state)]    -- remaining steps to execute
      -> Property
    go' tags (Lockstep before env) var action ss =
        go (Set.union (Set.fromList tags') tags) st' ss
      where
        st' :: Lockstep state
        st' = Lockstep after (EnvF.insert var modelResp env)

        modelResp :: ModelValue state a
        after     :: state
        (modelResp, after) = modelNextState (GVar.lookUpEnvF env) action before

        tags' :: [String]
        tags' = tagStep (before, after) action modelResp

{-------------------------------------------------------------------------------
  Running the tests
-------------------------------------------------------------------------------}

ioPropertyBracket ::
     Testable a
  => IO st
  -> (st -> IO ())
  -> ReaderT st IO a
  -> Property
ioPropertyBracket init cleanup (ReaderT prop) = do
    QC.ioProperty $ mask $ \restore -> do
      st <- init
      a  <- restore (prop st) `onException` cleanup st
      cleanup st
      return a

-- | Variation on 'monadicIO' that allows for state initialisation/cleanup
monadicBracketIO :: forall st a.
     Testable a
  => IO st
  -> (st -> IO ())
  -> (PropertyM (ReaderT st IO) a)
  -> Property
monadicBracketIO init cleanup =
    monadic (ioPropertyBracket init cleanup)

-- | Convenience runner with support for state initialization
--
-- This is less general than 'Test.QuickCheck.StateModel.runActions', but
-- will be useful in many scenarios.
runActionsBracket ::
     RunLockstep state (ReaderT st IO)
  => proxy state
  -> IO st                     -- ^ Initialisation
  -> (st -> IO ())             -- ^ Cleanup
  -> Actions (Lockstep state)
  -> Property
runActionsBracket _ init cleanup actions =
    monadicBracketIO init cleanup $
      void $ Test.QuickCheck.StateModel.runActions actions
