{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | Generalized variables
--
-- Intended for unqualified import.
module Test.QuickCheck.StateModel.Lockstep.GVar (
    GVar -- opaque
  , AnyGVar(..)
    -- * Construction
  , unsafeMkGVar
  , fromVar
  , mapGVar
    -- * Interop with 'Env'
  , lookUpGVar
    -- * Interop with 'EnvF'
  , lookUpEnvF
  , definedInEnvF
  , shrinkGVar
    -- * Internal: exposed for testing
  , pattern GVar
  ) where

import           Prelude hiding (map)

import           Control.Monad.Identity (Identity (..))

import           Data.Maybe (isJust)
import           Data.Typeable

import           GHC.Show

import           Test.QuickCheck.StateModel (HasVariables (..), LookUp, Var)

import qualified Test.QuickCheck.StateModel.Lockstep.EnvF as EnvF
import           Test.QuickCheck.StateModel.Lockstep.EnvF (EnvF)
import           Test.QuickCheck.StateModel.Lockstep.Op

{-------------------------------------------------------------------------------
  Main type
-------------------------------------------------------------------------------}

-- | Generalized variables
--
-- The key difference between 'GVar' and the standard 'Var' type is that
-- 'GVar' have a functor-esque structure: see 'mapGVar'.
data GVar op f where
  GVar :: Typeable x => Var x -> op x y -> GVar op y

data AnyGVar op where
  SomeGVar :: GVar op y -> AnyGVar op

instance (forall x. Show (op x a)) => Show (GVar op a) where
  showsPrec n (GVar v op) =
      showParen (n >= 11)
      $ showString "unsafeMkGVar "
      . showsPrec 11 v
      . showSpace
      . showsPrec 11 op

instance (forall x. Eq (op x a)) => Eq (GVar op a) where
  (==) = \(GVar x op) (GVar x' op') -> aux x x' op op'
    where
      aux :: forall x x'.
           (Typeable x, Typeable x')
        => Var x -> Var x' -> op x a -> op x' a -> Bool
      aux x x' op op' =
          case eqT @x @x' of
            Nothing   -> False
            Just Refl -> (x, op) == (x', op')

instance HasVariables (GVar op f) where
  getAllVariables (GVar v _) = getAllVariables v

instance HasVariables (AnyGVar op) where
  getAllVariables (SomeGVar v) = getAllVariables v

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

-- | Only for pretty-printing counter-examples, do not use directly
unsafeMkGVar :: Typeable a => Var a -> op a b -> GVar op b
unsafeMkGVar = GVar

fromVar :: (Operation op, Typeable a) => Var a -> GVar op a
fromVar var = GVar var opIdentity

mapGVar :: (forall x. op x a -> op x b) -> GVar op a -> GVar op b
mapGVar f (GVar var op) = GVar var (f op)

{-------------------------------------------------------------------------------
  Interop with 'Env'
-------------------------------------------------------------------------------}

lookUpWrapped :: Typeable a => LookUp -> Var a -> Identity a
lookUpWrapped m v = Identity (m v)

-- | Lookup 'GVar' given a lookup function for 'Var'
--
-- The result is 'Just' if the variable is in the environment and evaluation
-- succeeds. This is normally guaranteed by the default test 'precondition'.
lookUpGVar ::
     InterpretOp op Identity
  => LookUp -> GVar op a -> Maybe a
lookUpGVar lookUp (GVar var op) =
    runIdentity <$> intOp op (lookUpWrapped lookUp var)

{-------------------------------------------------------------------------------
  Interop with EnvF
-------------------------------------------------------------------------------}

-- | Lookup 'GVar'
--
-- The result is 'Just' if the variable is in the environment and evaluation
-- succeeds. This is normally guaranteed by the default test 'precondition'.
lookUpEnvF :: (Typeable f, InterpretOp op f) => EnvF f -> GVar op a -> Maybe (f a)
lookUpEnvF env (GVar var op) =
    EnvF.lookup var env >>= intOp op

-- | Check if the variable is well-defined and evaluation will succeed.
definedInEnvF :: (Typeable f, InterpretOp op f) => EnvF f -> GVar op a -> Bool
definedInEnvF env gvar = isJust (lookUpEnvF env gvar)

-- | Shrink a 'GVar' to earlier 'GVar's of the same type. It is guaranteed that
-- the shrunk variables are in the environment and that evaluation will succeed.
shrinkGVar :: (Typeable f, InterpretOp op f)  => EnvF f -> GVar op a -> [GVar op a]
shrinkGVar env (GVar var op) =
    [ gvar'
    | var' <- EnvF.shrinkVar env var
    , let gvar' = GVar var' op
    , definedInEnvF env gvar'
    ]
