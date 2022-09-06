-- | Lockstep-style testing using @quickcheck-dynamic@
--
-- This module is intended for unqualified import alongside imports of
-- "Test.QuickCheck.StateModel".
--
-- > import Test.QuickCheck.StateModel
-- > import Test.QuickCheck.StateModel.Lockstep
-- > import Test.QuickCheck.StateModel.Lockstep.Run      qualified as Lockstep
-- > import Test.QuickCheck.StateModel.Lockstep.Defaults qualified as Lockstep
module Test.QuickCheck.StateModel.Lockstep (
    -- * Main abstraction
    Lockstep -- opaque
  , InLockstep(..)
  , RunLockstep(..)
    -- ** Convenience aliases
  , LockstepAction
  , ModelFindVariables
  , ModelLookUp
  , ModelVar
    -- * Variables
  , GVar -- opaque
  , AnyGVar(..)
  , lookUpGVar
  , mapGVar
    -- ** Operations
  , Operation(..)
  , InterpretOp(..)
  ) where

import Prelude hiding (init)

import Test.QuickCheck.StateModel.Lockstep.API
import Test.QuickCheck.StateModel.Lockstep.Op
import Test.QuickCheck.StateModel.Lockstep.GVar
