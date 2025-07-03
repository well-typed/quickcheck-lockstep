{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies          #-}

-- | Public API
--
-- This is re-exported through "Test.QuickCheck.StateModel.Lockstep".
module Test.QuickCheck.StateModel.Lockstep.API (
    -- * State
    Lockstep(..)
  , getModel
    -- * Main abstraction
  , InLockstep(..)
  , RunLockstep(..)
    -- * Convenience aliases
  , LockstepAction
  , ModelFindVariables
  , ModelLookUp
  , ModelVar
  , ModelShrinkVar
  , RealLookUp
    -- * Variable context
  , ModelVarContext
  , findVars
  , lookupVar
  , shrinkVar
  , realLookupVar
  ) where

import           Control.Monad.Identity (Identity)

import           Data.Constraint (Dict (..))
import           Data.Kind
import           Data.Typeable

import           Test.QuickCheck (Gen)
import           Test.QuickCheck.StateModel (Action, Any, LookUp, RunModel,
                     StateModel)

import qualified Test.QuickCheck.StateModel.Lockstep.EnvF as EnvF
import           Test.QuickCheck.StateModel.Lockstep.EnvF (EnvF)
import qualified Test.QuickCheck.StateModel.Lockstep.GVar as EnvF
import           Test.QuickCheck.StateModel.Lockstep.GVar (AnyGVar (..), GVar,
                     fromVar)
import           Test.QuickCheck.StateModel.Lockstep.Op
import qualified Test.QuickCheck.StateModel.Lockstep.Op.Identity as Identity

{-------------------------------------------------------------------------------
  Lockstep state

  @quickcheck-dynamic@ takes care of keeping track of the responses of the
  system under test, but not the model. We do that here.
-------------------------------------------------------------------------------}

data Lockstep state = Lockstep {
      lockstepModel :: state
    , lockstepEnv   :: EnvF (ModelValue state)
    }

-- | The 'Show' instance does not show the internal environment
instance Show state => Show (Lockstep state) where
  show = show . lockstepModel

-- | Inspect the model that resides inside the Lockstep state
getModel :: Lockstep state -> state
getModel = lockstepModel

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
    -> ModelVarContext state
    -> state
    -> (ModelValue state a, state)

  -- | Generate an arbitrary action, given a variable context
  arbitraryWithVars ::
       ModelVarContext state
    -> state
    -> Gen (Any (LockstepAction state))

  -- | Shrink an action, given a variable context
  --
  -- This is optional; without an implementation of 'shrinkWithVars', lists of
  -- actions will still be pruned, but /individual/ actions will not be shrunk.
  shrinkWithVars ::
       ModelVarContext state
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
    -> LockstepAction state a -> a -> Observable state a

  -- | Show responses from the real system
  --
  -- This method does not need to be implemented, but if it is, counter-examples
  -- can include the real response in addition to the observable response.
  showRealResponse ::
       Proxy m
    -> LockstepAction state a
    -> Maybe (Dict (Show a))
  showRealResponse _ _ = Nothing

{-------------------------------------------------------------------------------
  Convenience aliases
-------------------------------------------------------------------------------}

-- | An action in the lock-step model
type LockstepAction state = Action (Lockstep state)

-- | See 'lookupVar'.
type ModelLookUp state = forall a. ModelVar state a -> ModelValue state a

-- | See 'findVars'.
type ModelFindVariables state = forall a.
          Typeable a
       => Proxy a -> [GVar (ModelOp state) a]

-- | See 'shrinkVar'.
type ModelShrinkVar state = forall a. ModelVar state a -> [ModelVar state a]

-- | See 'realLookupVar'.
type RealLookUp op = forall a. LookUp -> GVar op a -> a

-- | Variables with a "functor-esque" instance
type ModelVar state = GVar (ModelOp state)

{-------------------------------------------------------------------------------
  Variable context
-------------------------------------------------------------------------------}

-- | The environment of known variables and their (model) values.
--
-- This environment can be queried for information about known variables through
-- 'findVars', 'lookupVar', and 'shrinkVar'. This environment is updated
-- automically by the lockstep framework.
type ModelVarContext state = EnvF (ModelValue state)

-- | Find variables of the appropriate type
--
-- The type you pass must be the result type of (previously executed) actions.
-- If you want to change the type of the variable, see 'EnvF.mapGVar'.
findVars ::
     InLockstep state
  => ModelVarContext state -> ModelFindVariables state
findVars env _ = map fromVar $ EnvF.keysOfType env

-- | Look up a variable for execution of the model.
--
-- The type of the variable is the type in the /real/ system.
lookupVar ::
     InLockstep state
  => ModelVarContext state -> ModelLookUp state
lookupVar env gvar = case EnvF.lookUpEnvF env gvar of
    Just x -> x
    Nothing -> error
      "lookupVar: the variable (ModelVar) must be well-defined and evaluable, \
      \but this requirement was violated. Normally, this is guaranteed by the \
      \default test 'precondition'."

-- | Shrink variables to /earlier/ variables of the same type.
shrinkVar ::
     (Typeable state, InterpretOp (ModelOp state) (ModelValue state))
  => ModelVarContext state -> ModelShrinkVar state
shrinkVar env var = EnvF.shrinkGVar env var

-- | Look up a variable for execution of the real system.
--
-- The type of the variable is the type in the /real/ system.
realLookupVar :: InterpretOp op Identity => RealLookUp op
realLookupVar lookUp gvar = case EnvF.lookUpGVar lookUp gvar of
    Just x -> x
    Nothing -> error
      "realLookupVar: the variable (GVar) must be well-defined and evaluable, \
      \but this requirement was violated. Normally, this is guaranteed by the \
      \default test 'precondition'."
