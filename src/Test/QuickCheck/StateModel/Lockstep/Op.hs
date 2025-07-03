module Test.QuickCheck.StateModel.Lockstep.Op (
    Operation(..)
  , InterpretOp(..)
  , intOpIdentity
  ) where

import           Control.Monad.Identity (Identity (..))
import           Data.Coerce

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

intOpIdentity ::
     (op a b -> a -> Maybe b)
  -> op a b -> Identity a -> Maybe (Identity b)
intOpIdentity = coerce
