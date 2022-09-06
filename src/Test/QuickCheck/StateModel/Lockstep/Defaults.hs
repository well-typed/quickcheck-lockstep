-- | Default implementations for the @quickcheck-dynamic@ class methods
--
-- Intended for qualified import.
--
-- > import Test.QuickCheck.StateModel.Lockstep.Defaults qualified as Lockstep
module Test.QuickCheck.StateModel.Lockstep.Defaults (
    -- * Default implementations for methods of 'StateModel'
    initialState
  , nextState
  , precondition
  , arbitraryAction
  , shrinkAction
    -- * Default implementations for methods of 'RunModel'
  , postcondition
  , monitoring
  ) where

import Prelude hiding (init)

import Data.Maybe (isNothing)
import Data.Typeable

import Test.QuickCheck (Gen, Property)
import Test.QuickCheck qualified as QC
import Test.QuickCheck.StateModel (Var, Any(..), LookUp, Realized)

import Test.QuickCheck.StateModel.Lockstep.API
import Test.QuickCheck.StateModel.Lockstep.EnvF (EnvF)
import Test.QuickCheck.StateModel.Lockstep.EnvF qualified as EnvF
import Test.QuickCheck.StateModel.Lockstep.GVar

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
    (modelResp, state') = modelNextState action (lookUpEnvF env) state

-- | Default implementation for 'Test.QuickCheck.StateModel.precondition'
--
-- The default precondition only checks that all variables have a value
-- and that the operations on them are defined.
precondition ::
     InLockstep state
  => Lockstep state -> LockstepAction state a -> Bool
precondition (Lockstep _ env) =
    all (\(SomeGVar var) -> definedInEnvF env var) . usedVars

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

monitoring :: forall m state a.
     RunLockstep state m
  => Proxy m
  -> (Lockstep state, Lockstep state)
  -> LockstepAction state a
  -> LookUp m
  -> Realized m a
  -> Property -> Property
monitoring p (before, after) action _lookUp realResp =
      QC.counterexample ("State: " ++ show after)
    . maybe id QC.counterexample (checkResponse p before action realResp)
    . QC.tabulate "Tags" tags
  where
    tags :: [String]
    tags = tagStep (lockstepModel before, lockstepModel after) action modelResp

    modelResp :: ModelValue state a
    modelResp = fst $ modelNextState
                        action
                        (lookUpEnvF $ lockstepEnv before)
                        (lockstepModel before)

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

varsOfType ::
     InLockstep state
  => EnvF (ModelValue state) -> ModelFindVariables state
varsOfType env _ = map fromVar $ EnvF.keysOfType env

-- | Check the response of the system under test against the model
--
-- This is used in 'postcondition', where we can however only return a 'Bool',
-- and in 'monitoring', to give the user more detailed feedback.
checkResponse :: forall m state a.
     RunLockstep state m
  => Proxy m
  -> Lockstep state -> LockstepAction state a -> Realized m a -> Maybe String
checkResponse p (Lockstep state env) action a =
    compareEquality (observeReal p action a) (observeModel modelResp)
  where
    modelResp :: ModelValue state a
    modelResp = fst $ modelNextState action (lookUpEnvF env) state

    compareEquality ::  Observable state a -> Observable state a -> Maybe String
    compareEquality real mock
      | real == mock = Nothing
      | otherwise    = Just $ concat [
            "System under test returned: "
          , show real
          , "\nbut model returned:         "
          , show mock
          ]


