-- | Run lockstep tests
--
-- Intended for qualified import.
--
-- > import Test.QuickCheck.StateModel.Lockstep.Run qualified as Lockstep
module Test.QuickCheck.StateModel.Lockstep.Run (
    -- * Finding labelled examples
    tagActions
    -- * Run tests
  , runActions
  , runActionsBracket
  ) where

import Prelude hiding (init)

import Control.Exception
import Control.Monad.Reader
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Typeable
import Test.QuickCheck.Monadic

import Test.QuickCheck (Property, Testable)
import Test.QuickCheck qualified as QC
import Test.QuickCheck.StateModel hiding (runActions)
import Test.QuickCheck.StateModel qualified as StateModel

import Test.QuickCheck.StateModel.Lockstep.API
import Test.QuickCheck.StateModel.Lockstep.EnvF qualified as EnvF
import Test.QuickCheck.StateModel.Lockstep.GVar

{-------------------------------------------------------------------------------
  Finding labelled examples

  Implementation note: the 'monitoring' hook from 'StateModel' cannot be used
  for finding labelled examples. This hook is called many times during test
  execution (once per action); this means that we cannot call 'label' inside
  'monitoring', but must instead use 'tabulate'. However, 'tabulate' is not
  supported by 'labelledExamples'. In 'tagActions' we therefore run over all
  actions, collecting tags as we go, and then do a /single/ call to 'label'
  at the end.
-------------------------------------------------------------------------------}

-- | Tag a list of actions
--
-- This can be used together with QuickCheck's 'labelledExamples' to test your
-- tagging code as well as your shrinker (QuickCheck will try to produce
-- /minimal/ labelled examples).
--
-- Unlike 'runActions', this does not require a 'RunModel' instance; this is
-- executed against the model /only/.
tagActions :: forall state.
     InLockstep state
  => Proxy state
  -> Actions (Lockstep state)
  -> Property
tagActions _p (Actions steps) =
    go Set.empty Test.QuickCheck.StateModel.initialState steps
  where
    go :: Set String -> Lockstep state -> [Step (Lockstep state)] -> Property
    go tags _st []            = QC.label ("Tags: " ++ show (Set.toList tags)) True
    go tags  st ((v:=a) : ss) = go' tags st v a ss

    go' :: forall a.
         Typeable a
      => Set String                 -- accumulated set of tags
      -> Lockstep state             -- current state
      -> Var a                      -- variable for the result of this action
      -> Action (Lockstep state) a  -- action to execute
      -> [Step (Lockstep state)]    -- remaining steps to execute
      -> Property
    go' tags (Lockstep before env) var action ss =
        go (Set.union (Set.fromList tags') tags) st' ss
      where
        st' :: Lockstep state
        st' = Lockstep after (EnvF.insert var modelResp env)

        modelResp :: ModelValue state a
        after     :: state
        (modelResp, after) = modelNextState action (lookUpEnvF env) before

        tags' :: [String]
        tags' = tagStep (before, after) action modelResp

{-------------------------------------------------------------------------------
  Run tests
-------------------------------------------------------------------------------}

runActions ::
     RunLockstep state IO
  => Proxy state
  -> Actions (Lockstep state) -> Property
runActions _ actions = monadicIO $ void $ StateModel.runActions actions

-- | Convenience runner with support for state initialization
--
-- This is less general than 'Test.QuickCheck.StateModel.runActions', but will
-- be useful in many scenarios.
--
-- For most lockstep-style tests, a suitable monad to run the tests in is
-- @'ReaderT' r 'IO'@. In this case, using @'runReaderT'@ as the runner argument
-- is a reasonable choice.
runActionsBracket ::
     RunLockstep state m
  => Proxy state
  -> IO st         -- ^ Initialisation
  -> (st -> IO ()) -- ^ Cleanup
  -> (m Property -> st -> IO Property) -- ^ Runner
  -> Actions (Lockstep state) -> Property
runActionsBracket _ init cleanup runner actions =
    monadicBracketIO init cleanup runner $
      void $ StateModel.runActions actions

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

ioPropertyBracket ::
     Testable a
  => IO st
  -> (st -> IO ())
  -> (m a -> st -> IO a)
  -> m a
  -> Property
ioPropertyBracket init cleanup runner prop =
    QC.ioProperty $ mask $ \restore -> do
      st <- init
      a <- restore (runner prop st) `onException` cleanup st
      cleanup st
      return a

-- | Variation on 'monadicIO' that allows for state initialisation/cleanup
monadicBracketIO :: forall st a m.
     (Monad m, Testable a)
  => IO st
  -> (st -> IO ())
  -> (m Property -> st -> IO Property)
  -> PropertyM m a
  -> Property
monadicBracketIO init cleanup runner =
    monadic (ioPropertyBracket init cleanup runner)

