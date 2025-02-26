-- | Environment parameterised by functor @f@
--
-- Intended for qualified import:
--
-- > import Test.QuickCheck.StateModel.Lockstep.EnvF (EnvF)
-- > import Test.QuickCheck.StateModel.Lockstep.EnvF qualified as EnvF
module Test.QuickCheck.StateModel.Lockstep.EnvF (
    EnvF -- opaque
    -- * Construction
  , empty
  , insert
    -- * Query
  , lookup
  , keysOfType
  , shrinkVar
  ) where

import Prelude hiding (lookup)

import Control.Monad
import Data.Foldable (asum)
import Data.Maybe (mapMaybe)
import Data.Typeable

import Test.QuickCheck.StateModel.Variables (Var, unsafeCoerceVar)

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

data EnvEntry f where
  EnvEntry :: Typeable a => Var a -> f a -> EnvEntry f

newtype EnvF f = EnvF [EnvEntry f]

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

empty :: EnvF f
empty = EnvF []

insert :: Typeable a => Var a -> f a -> EnvF f -> EnvF f
insert x fa (EnvF env) = EnvF (EnvEntry x fa : env)

{-------------------------------------------------------------------------------
  Query
-------------------------------------------------------------------------------}

lookup :: forall f a. (Typeable f, Typeable a) => Var a -> EnvF f -> Maybe (f a)
lookup = \var (EnvF env) ->
    asum $ map (\(EnvEntry var' fa') -> aux var var' fa') env
  where
    aux :: Typeable a' => Var a -> Var a' -> f a' -> Maybe (f a)
    aux v1 v2 fa' = do
        guard (v1 == unsafeCoerceVar v2)
        cast fa'

keysOfType :: Typeable a => EnvF f -> [Var a]
keysOfType (EnvF env) = mapMaybe (\(EnvEntry var _) -> cast var) env

-- | Shrink a variable to variables of the same type, but with a smaller
-- variable number.
--
-- The numbering of variables is according to their age, meaning that this
-- function shrinks a variable to /earlier/ variables.
shrinkVar :: Typeable a => EnvF f -> Var a -> [Var a]
shrinkVar envf v = [ v' | v' <- keysOfType envf, v' < v ]