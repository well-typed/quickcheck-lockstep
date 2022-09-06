module Test.QuickCheck.StateModel.Lockstep.Op (
    Operation(..)
  , InterpretOp(..)
  , WrapRealized(..)
  , intOpRealizedId
  ) where

import Test.QuickCheck.StateModel (Realized)
import Data.Coerce

{-------------------------------------------------------------------------------
  Operations

  This is basically reified functions. We reify them for two reasons:

  1. It means we can define proper Show/Eq instances for 'GVar'
  2. It allows us to give separate interpretations of 'Op' for mock values
     and for real values.
-------------------------------------------------------------------------------}

class Operation op where
  opIdentity :: op a a

class Operation op => InterpretOp op f where
  intOp :: op a b -> f a -> Maybe (f b)

{-------------------------------------------------------------------------------
  Interop with 'Realized'

  We want to execute operations against 'Realized' values, but since that is
  a newtype, we need to wrap and unwrap in order to be able to give the
  appropriate 'InterpretOp' instances.
-------------------------------------------------------------------------------}

newtype WrapRealized m a = WrapRealized {
      unwrapRealized :: Realized m a
    }

-- | Convenience function for defining 'InterpretOp' instances
--
-- This can be used for monads like @IO@ where @Realized m a@ is just @a@.
intOpRealizedId ::
     (Realized m a ~ a, Realized m b ~ b)
  => (op a b -> a -> Maybe b)
  -> op a b -> WrapRealized m a -> Maybe (WrapRealized m b)
intOpRealizedId = coerce
