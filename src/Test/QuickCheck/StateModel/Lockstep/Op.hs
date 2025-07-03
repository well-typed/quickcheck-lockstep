{-# LANGUAGE TypeOperators #-}

module Test.QuickCheck.StateModel.Lockstep.Op (
    Operation(..)
  , InterpretOp(..)
  , WrapRealized(..)
  , intOpRealizedId
  , intOpTransformer
  ) where

import           Data.Coerce
import           Test.QuickCheck.StateModel (Realized)

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

  We want to execute operations against 'Realized' values, but since that is a
  type family, we need to wrap and unwrap in order to be able to give the
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

-- | Convenience function for defining 'InterpretOp' instances for monad
-- transformer stacks.
intOpTransformer ::
     forall t m a b op.
     ( Realized (t m) a ~ Realized m a
     , Realized (t m) b ~ Realized m b
     , InterpretOp op (WrapRealized m)
     )
  => op a b
  -> WrapRealized (t m) a
  -> Maybe (WrapRealized (t m) b)
intOpTransformer op wr = coerceOut <$> intOp op (coerceIn wr)
  where
    coerceIn :: WrapRealized (t m) a -> WrapRealized m a
    coerceIn = coerce

    coerceOut :: WrapRealized m b -> WrapRealized (t m) b
    coerceOut = coerce
