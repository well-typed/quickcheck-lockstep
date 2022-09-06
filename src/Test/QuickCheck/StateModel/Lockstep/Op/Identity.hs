module Test.QuickCheck.StateModel.Lockstep.Op.Identity (Op(..)) where

import Test.QuickCheck.StateModel.Lockstep.Op

-- | Very simple operation type that supports identity only
--
-- This can be used by tests that don't need to map over variables. That is,
-- where variables always refer to the /exact/ result of previously executed
-- commands. Such tests will not need to define any 'InterpretOp' instances.
data Op a b where
  OpId :: Op a a

deriving instance Show (Op a b)
deriving instance Eq   (Op a b)

instance Operation   Op   where opIdentity = OpId
instance InterpretOp Op f where intOp OpId = Just

