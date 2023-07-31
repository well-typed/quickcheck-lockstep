{-# LANGUAGE CPP #-}

module Test.QuickCheck.StateModel.Lockstep.Op.SumProd (Op(..), intOpId) where

#if __GLASGOW_HASKELL__ >= 906
import Control.Monad ((<=<))
#endif
import Control.Monad.Reader (ReaderT)
import Control.Monad.State
import GHC.Show (appPrec)

import Test.QuickCheck.StateModel.Lockstep.Op

{-------------------------------------------------------------------------------
  Example (but very useful) 'Operation' example

  Because this is designed for testing where we want everything to be 'Show'able
  and 'Typeable', matching on 'Op' might reveal some additonal constrants.
  This is useful in 'OpComp' where we have an existential variable (@b@), but
  it's also useful for example in 'OpRight': the caller might have a constraint
  @Show (Either a b)@, but that doesn't give them a way to obtain a constraint
  @Show a@; the implication only goes one way.

  (These are the same constraints that 'Any' imposes.)
-------------------------------------------------------------------------------}

-- | Operations with support for products (pairs) and sums ('Either')
data Op a b where
  OpId    :: Op a a
  OpFst   :: Op (a, b) a
  OpSnd   :: Op (b, a) a
  OpLeft  :: Op (Either a b) a
  OpRight :: Op (Either b a) a
  OpComp  :: Op b c -> Op a b -> Op a c

intOpId :: Op a b -> a -> Maybe b
intOpId OpId         = Just
intOpId OpFst        = Just . fst
intOpId OpSnd        = Just . snd
intOpId OpLeft       = either Just (const Nothing)
intOpId OpRight      = either (const Nothing) Just
intOpId (OpComp g f) = intOpId g <=< intOpId f

{-------------------------------------------------------------------------------
  'InterpretOp' instances
-------------------------------------------------------------------------------}

instance Operation Op where
  opIdentity = OpId

instance InterpretOp Op (WrapRealized IO) where
  intOp = intOpRealizedId intOpId

instance InterpretOp Op (WrapRealized m)
      => InterpretOp Op (WrapRealized (StateT s m)) where
  intOp = intOpTransformer

instance InterpretOp Op (WrapRealized m)
      => InterpretOp Op (WrapRealized (ReaderT r m)) where
  intOp = intOpTransformer

{-------------------------------------------------------------------------------
  'Show' and 'Eq' instances
-------------------------------------------------------------------------------}

sameOp :: Op a b -> Op c d -> Bool
sameOp = go
  where
    go :: Op a b -> Op c d -> Bool
    go OpId         OpId           = True
    go OpFst        OpFst          = True
    go OpSnd        OpSnd          = True
    go OpLeft       OpLeft         = True
    go OpRight      OpRight        = True
    go (OpComp g f) (OpComp g' f') = go g g' && go f f'
    go _            _              = False

    _coveredAllCases :: Op a b -> ()
    _coveredAllCases = \case
        OpId    -> ()
        OpFst   -> ()
        OpSnd   -> ()
        OpLeft  -> ()
        OpRight -> ()
        OpComp{} -> ()

instance Eq (Op a b)  where
  (==) = sameOp

instance Show (Op a b) where
  showsPrec p = \op -> case op of
      OpComp{} -> showParen (p > appPrec) (go op)
      _        -> go op
    where
      go :: Op x y -> String -> String
      go OpId         = showString "id"
      go OpFst        = showString "fst"
      go OpSnd        = showString "snd"
      go OpLeft       = showString "fromLeft"
      go OpRight      = showString "fromRight"
      go (OpComp g f) = go g . showString " . " . go f

