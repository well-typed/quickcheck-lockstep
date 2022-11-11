{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module Test.MockFS (tests) where

import Prelude hiding (init)

import Control.Exception (catch, throwIO)
import Control.Monad
import Control.Monad.Reader
import Data.Bifunctor
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Typeable
import System.Directory (removeDirectoryRecursive)
import System.Directory qualified as IO
import System.IO qualified as IO
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (testProperty)

import Test.QuickCheck (Gen)
import Test.QuickCheck qualified as QC
import Test.QuickCheck.StateModel

import Test.QuickCheck.StateModel.Lockstep
import Test.QuickCheck.StateModel.Lockstep.Defaults qualified as Lockstep
import Test.QuickCheck.StateModel.Lockstep.Op.SumProd
import Test.QuickCheck.StateModel.Lockstep.Run qualified as Lockstep

import Test.MockFS.Mock (Mock, Dir(..), File(..), Err)
import Test.MockFS.Mock qualified as Mock

{-------------------------------------------------------------------------------
  Model state
-------------------------------------------------------------------------------}

data FsState = FsState {
      fsStateMock  :: Mock
    , fsStateStats :: Stats
    }
  deriving (Show)

initState :: FsState
initState = FsState {
      fsStateMock  = Mock.emptyMock
    , fsStateStats = initStats
    }

{-------------------------------------------------------------------------------
  StateModel and 'RunModel' instances
-------------------------------------------------------------------------------}

type RealMonad = ReaderT FilePath IO

type FsVar a = ModelVar FsState a
type FsAct a = Action (Lockstep FsState) (Either Err a)

instance StateModel (Lockstep FsState) where
  data Action (Lockstep FsState) a where
    MkDir :: Dir                        -> FsAct ()
    Open  :: File                       -> FsAct (IO.Handle, File)
    Write :: FsVar IO.Handle  -> String -> FsAct ()
    Close :: FsVar IO.Handle            -> FsAct ()
    Read  :: Either (FsVar File) File   -> FsAct String

  initialState    = Lockstep.initialState initState
  nextState       = Lockstep.nextState
  precondition    = Lockstep.precondition
  arbitraryAction = Lockstep.arbitraryAction
  shrinkAction    = Lockstep.shrinkAction

instance RunModel (Lockstep FsState) RealMonad where
  perform       = \_st -> runIO
  postcondition = Lockstep.postcondition
  monitoring    = Lockstep.monitoring (Proxy @RealMonad)

deriving instance Show (Action (Lockstep FsState) a)
deriving instance Eq   (Action (Lockstep FsState) a)

{-------------------------------------------------------------------------------
  InLockstep instance
-------------------------------------------------------------------------------}

type FsVal a = ModelValue FsState a
type FsObs a = Observable FsState a

instance InLockstep FsState where

  --
  -- Model values
  --
  -- For model values, we must be sure that if we have a value of type
  --
  -- > FsVal IO.Handle
  --
  -- that it is a in fact mock handle. This means that here we cannot define
  --
  -- > ModelId :: a -> FsVal a
  --
  -- unlike in 'FsObs'.

  data ModelValue FsState a where
    MHandle :: Mock.MHandle -> FsVal IO.Handle

    -- Rest is regular:

    MErr    :: Err    -> FsVal Err
    MFile   :: File   -> FsVal File
    MString :: String -> FsVal String
    MUnit   :: ()     -> FsVal ()

    MEither :: Either (FsVal a) (FsVal b) -> FsVal (Either a b)
    MPair   :: (FsVal a, FsVal b)         -> FsVal (a, b)

  --
  -- Observable results
  --

  data Observable FsState a where
    OHandle :: FsObs IO.Handle
    OId     :: (Show a, Typeable a, Eq a) => a -> FsObs a
    OEither :: Either (FsObs a) (FsObs b) -> FsObs (Either a b)
    OPair   :: (FsObs a, FsObs b) -> FsObs (a, b)

  observeModel :: FsVal a -> FsObs a
  observeModel = \case
      MEither x -> OEither $ bimap observeModel observeModel x
      MPair   x -> OPair   $ bimap observeModel observeModel x
      MErr    x -> OId x
      MString x -> OId x
      MUnit   x -> OId x
      MFile   x -> OId x
      MHandle _ -> OHandle

  --
  -- Semantics
  --

  modelNextState :: forall a.
       LockstepAction FsState a
    -> ModelLookUp FsState
    -> FsState -> (FsVal a, FsState)
  modelNextState action lookUp (FsState mock stats) =
      auxStats $ runMock lookUp action mock
    where
      auxStats :: (FsVal a, Mock) -> (FsVal a, FsState)
      auxStats (result, state') =
          (result, FsState state' $ updateStats action result stats)

  --
  -- Variables
  --

  type ModelOp FsState = Op

  usedVars :: LockstepAction FsState a -> [AnyGVar (ModelOp FsState)]
  usedVars = \case
      MkDir{}        -> []
      Open{}         -> []
      Write h _      -> [SomeGVar h]
      Close h        -> [SomeGVar h]
      Read (Left h)  -> [SomeGVar h]
      Read (Right _) -> []

  --
  -- Generation, shrinking and labelling
  --

  arbitraryWithVars findVars _mock = arbitraryFsAction findVars
  shrinkWithVars    findVars _mock = shrinkFsAction    findVars

  tagStep (_, FsState _ after) act = map show . tagFsAction after act

deriving instance Show (Observable FsState a)
deriving instance Eq   (Observable FsState a)

deriving instance Show (FsVal a)

{-------------------------------------------------------------------------------
  RunLockstep instance
-------------------------------------------------------------------------------}

instance RunLockstep FsState RealMonad where
  observeReal ::
       Proxy RealMonad
    -> LockstepAction FsState a -> Realized RealMonad a -> FsObs a
  observeReal _ = \case
      MkDir{} -> OEither . bimap OId OId
      Open{}  -> OEither . bimap OId (OPair . bimap (const OHandle) OId)
      Write{} -> OEither . bimap OId OId
      Close{} -> OEither . bimap OId OId
      Read{}  -> OEither . bimap OId OId

{-------------------------------------------------------------------------------
  Interpreter against the model
-------------------------------------------------------------------------------}

runMock ::
     ModelLookUp FsState
  -> Action (Lockstep FsState) a
  -> Mock -> (FsVal a, Mock)
runMock lookUp = \case
    MkDir d   -> wrap MUnit     . Mock.mMkDir d
    Open f    -> wrap (mOpen f) . Mock.mOpen f
    Write h s -> wrap MUnit     . Mock.mWrite (getHandle $ lookUp h) s
    Close h   -> wrap MUnit     . Mock.mClose (getHandle $ lookUp h)
    Read f    -> wrap MString   . Mock.mRead (either (getFile . lookUp) id f)
  where
    wrap :: (a -> FsVal b) -> (Either Err a, Mock) -> (FsVal (Either Err b), Mock)
    wrap f = first (MEither . bimap MErr f)

    mOpen :: File -> Mock.MHandle -> FsVal (IO.Handle, File)
    mOpen f h = MPair (MHandle h, MFile f)

    getHandle :: ModelValue FsState IO.Handle -> Mock.MHandle
    getFile   :: ModelValue FsState File      -> File

    getHandle (MHandle h) = h
    getFile   (MFile   f) = f

{-------------------------------------------------------------------------------
  Generator and shrinking
-------------------------------------------------------------------------------}

arbitraryFsAction ::
     ModelFindVariables FsState
  -> Gen (Any (LockstepAction FsState))
arbitraryFsAction findVars = QC.oneof $ concat [
      withoutVars
    , case findVars (Proxy @((Either Err (IO.Handle, File)))) of
        []   -> []
        vars -> withVars (QC.elements vars)
    ]
  where
    withoutVars :: [Gen (Any (LockstepAction FsState))]
    withoutVars = [
          fmap Some $ MkDir <$> genDir
        , fmap Some $ Open  <$> genFile
        , fmap Some $ Read  <$> (Right <$> genFile)
        ]

    withVars ::
         Gen (FsVar (Either Err (IO.Handle, File)))
      -> [Gen (Any (LockstepAction FsState))]
    withVars genVar = [
          fmap Some $ Write <$> (handle <$> genVar) <*> genString
        , fmap Some $ Close <$> (handle <$> genVar)
        ]
      where
        handle :: GVar Op (Either Err (IO.Handle, File)) -> GVar Op IO.Handle
        handle = mapGVar (\op -> OpFst `OpComp` OpRight `OpComp` op)

    genDir :: Gen Dir
    genDir = do
        n <- QC.choose (0, 3)
        Dir <$> replicateM n (QC.elements ["x", "y", "z"])

    genFile :: Gen File
    genFile = File <$> genDir <*> QC.elements ["a", "b", "c"]

    genString :: Gen String
    genString = QC.sized $ \n -> replicateM n (QC.elements "ABC")

shrinkFsAction ::
     ModelFindVariables FsState
  -> Action (Lockstep FsState) a -> [Any (LockstepAction FsState)]
shrinkFsAction findVars = \case
    Open (File (Dir []) ('t' : n)) ->
      [openTemp n' | n' <- QC.shrink (read n)]
    Open _ ->
      [openTemp 100]
    Read (Right _) ->
      [ Some $ Read (Left $ mapGVar (\op -> OpSnd `OpComp` OpRight `OpComp` op) v)
      | v <- findVars (Proxy @((Either Err (IO.Handle, File))))
      ]
    _otherwise ->
      []
  where
    openTemp :: Int -> Any (LockstepAction FsState)
    openTemp n = Some $ Open (File (Dir []) ('t' : show n))

{-------------------------------------------------------------------------------
  Interpret 'Op' against 'ModelValue'
-------------------------------------------------------------------------------}

instance InterpretOp Op (ModelValue FsState) where
  intOp OpId         = Just
  intOp OpFst        = \case MPair   x -> Just (fst x)
  intOp OpSnd        = \case MPair   x -> Just (snd x)
  intOp OpLeft       = \case MEither x -> either Just (const Nothing) x
  intOp OpRight      = \case MEither x -> either (const Nothing) Just x
  intOp (OpComp g f) = intOp g <=< intOp f

{-------------------------------------------------------------------------------
  Interpreter for IO
-------------------------------------------------------------------------------}

runIO :: LockstepAction FsState a -> LookUp RealMonad -> RealMonad (Realized RealMonad a)
runIO action lookUp = ReaderT $ \root -> aux root action
  where
    aux :: FilePath -> LockstepAction FsState a -> IO a
    aux root = \case
        MkDir d -> catchErr $
          IO.createDirectory (Mock.dirFP root d)
        Open f -> catchErr $
          (,f) <$> IO.openFile (Mock.fileFP root f) IO.AppendMode
        Write h s -> catchErr $
          IO.hPutStr (lookUp' h) s
        Close h -> catchErr $
          IO.hClose (lookUp' h)
        Read f -> catchErr $
          IO.readFile (Mock.fileFP root $ either lookUp' id f)
      where
        lookUp' :: FsVar x -> x
        lookUp' = lookUpGVar (Proxy @RealMonad) lookUp

catchErr :: forall a. IO a -> IO (Either Err a)
catchErr act = catch (Right <$> act) handler
  where
    handler :: IOError -> IO (Either Err h)
    handler e = maybe (throwIO e) (return . Left) (Mock.fromIOError e)

{-------------------------------------------------------------------------------
  Statistics and tagging
-------------------------------------------------------------------------------}

-- The only statistics we need to track for this example is the files we opened
type Stats = Set File

initStats :: Stats
initStats = Set.empty

updateStats :: LockstepAction FsState a -> FsVal a -> Stats -> Stats
updateStats action result  =
   case (action, result) of
     (Open f, MEither (Right _)) -> Set.insert f
     _otherwise                  -> id

data Tag = OpenTwo | SuccessfulRead
  deriving (Show)

tagFsAction :: Stats -> LockstepAction FsState a -> FsVal a -> [Tag]
tagFsAction openedFiles = \case
    Read _ -> \v -> [SuccessfulRead | MEither (Right _) <- [v]]
    Open _ -> \_ -> [OpenTwo        | Set.size openedFiles >= 2]
    _      -> \_ -> []

{-------------------------------------------------------------------------------
  Top-level test
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.MockFS" [
      testCase "labelledExamples" $
        QC.labelledExamples $ Lockstep.tagActions (Proxy @FsState)
    , testProperty "propLockstep" $
        Lockstep.runActionsBracket (Proxy @FsState)
          (createSystemTempDirectory "QSM")
          removeDirectoryRecursive
          runReaderT
    ]

createSystemTempDirectory :: [Char] -> IO FilePath
createSystemTempDirectory prefix = do
    systemTempDir <- getCanonicalTemporaryDirectory
    createTempDirectory systemTempDir prefix
