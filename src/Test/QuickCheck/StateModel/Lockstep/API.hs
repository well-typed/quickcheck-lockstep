-- | Public API
--
-- This is re-exported through "Test.QuickCheck.StateModel.Lockstep".
module Test.QuickCheck.StateModel.Lockstep.API (
    -- * State
    Lockstep(..)
    -- * Main abstraction
  , InLockstep(..)
  , RunLockstep(..)
    -- * Convenience aliases
  , LockstepAction
  , ModelFindVariables
  , ModelLookUp
  , ModelVar
  ) where

import Data.Kind
import Data.Typeable

import Test.QuickCheck (Gen)
import Test.QuickCheck.StateModel (StateModel, Any, RunModel, Realized, Action)

import Test.QuickCheck.StateModel.Lockstep.EnvF (EnvF)
import Test.QuickCheck.StateModel.Lockstep.GVar (GVar, AnyGVar(..))
import Test.QuickCheck.StateModel.Lockstep.Op
import Test.QuickCheck.StateModel.Lockstep.Op.Identity qualified as Identity

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

-- | The 'Show' instance does not show the internal environment
instance Show state => Show (Lockstep state) where
  show = show . lockstepModel

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
  type ModelOp state = Identity.Op

  -- | Extract the observable part of a response from the model
  observeModel :: ModelValue state a-> Observable state a

  -- | All variables required by a command
  usedVars :: LockstepAction state a -> [AnyGVar (ModelOp state)]

  -- | Step the model
  --
  -- The order of the arguments mimicks 'perform'.
  modelNextState ::
       LockstepAction state a
    -> ModelLookUp state
    -> state
    -> (ModelValue state a, state)

  -- | Generate an arbitrary action, given a way to find variables
  arbitraryWithVars ::
       ModelFindVariables state
    -> state
    -> Gen (Any (LockstepAction state))

  -- | Shrink an action, given a way to find variables
  --
  -- This is optional; without an implementation of 'shrinkWithVars', lists of
  -- actions will still be pruned, but /individual/ actions will not be shrunk.
  shrinkWithVars ::
       ModelFindVariables state
    -> state
    -> LockstepAction state a
    -> [Any (LockstepAction state)]
  shrinkWithVars _ _ _ = []

  -- | Tag actions
  --
  -- Tagging is optional, but can help understand your test input data as
  -- well as your shrinker (see 'tagActions').
  tagStep ::
       (state, state)
    -> LockstepAction state a
    -> ModelValue state a
    -> [String]
  tagStep _ _ _ = []

class ( InLockstep state
      , RunModel (Lockstep state) m
      ) => RunLockstep state m where
  -- See also 'Observable'
  observeReal ::
       Proxy m
    -> LockstepAction state a -> Realized m a -> Observable state a

{-------------------------------------------------------------------------------
  Convenience aliases
-------------------------------------------------------------------------------}

-- | An action in the lock-step model
type LockstepAction state = Action (Lockstep state)

-- | Look up a variable for model execution
--
-- The type of the variable is the type in the /real/ system.
type ModelLookUp state = forall a. ModelVar state a -> ModelValue state a

-- | Find variables of the appropriate type
--
-- The type you pass must be the result type of (previously executed) actions.
-- If you want to change the type of the variable, see
-- 'StateModel.Lockstep.GVar.map'.
type ModelFindVariables state = forall a.
          Typeable a
       => Proxy a -> [GVar (ModelOp state) a]

-- | Variables with a "functor-esque" instance
type ModelVar state = GVar (ModelOp state)

