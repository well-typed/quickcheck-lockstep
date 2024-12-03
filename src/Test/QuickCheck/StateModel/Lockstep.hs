-- | Lockstep-style testing using @quickcheck-dynamic@
--
-- See <https://well-typed.com/blog/2022/09/lockstep-with-quickcheck-dynamic/>
-- for a tutorial.
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
  , getModel
  , InLockstep(..)
  , RunLockstep(..)
    -- ** Convenience aliases
  , LockstepAction
  , ModelFindVariables
  , ModelLookUp
  , ModelVar
    -- * Variable context
  , ModelVarContext
  , lookupVar
  , findVars
    -- * Variables
  , GVar -- opaque
  , AnyGVar(..)
  , unsafeMkGVar
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
