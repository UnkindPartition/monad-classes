{-# LANGUAGE GeneralizedNewtypeDeriving, NoMonomorphismRestriction,
             DataKinds, TypeFamilies, TemplateHaskell, ScopedTypeVariables,
             MagicHash #-}
import Test.Tasty
import Test.Tasty.HUnit
import Control.Monad.Trans.Class
import qualified Data.Functor.Identity as I
import qualified Control.Monad.Trans.Except as E
import qualified Control.Monad.Trans.Reader as R
import qualified Control.Monad.Trans.Writer as W
import Control.Monad.Classes
import Control.Monad.Classes.Run
import Control.Applicative
import Control.Exception hiding (catch, throw)
import Data.Lens.Light
import GHC.Prim (Proxy#, proxy#)

-- for IO tests
import qualified Foreign.Storable as Foreign
import qualified Foreign.Marshal.Alloc as Foreign

-- for monad-control tests
import qualified Data.Conduit as C
import Control.Monad.Morph

-- for zoom tests
data Record = Record
  { _listL :: [Int]
  , _intL :: Int
  }
  deriving (Show, Eq)

makeLens ''Record

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ simpleStateTests
  , twoStatesTests
  , liftingTest
  , localState
  , exceptTests
  , execTests
  , zoomTests
  , liftNTests
  , liftConduitTest
  , mapWriterTest
  ]

simpleStateTests = testGroup "Simple State"
  [ testCase "get" $
      (run $ runStateLazy (0 :: Int) get) @?= (0 :: Int, 0 :: Int)
  , testCase "put" $
      (run $ runStateLazy (0 :: Int) (put (1 :: Int))) @?= ((), 1 :: Int)
  , testCase "put-get-put" $
      (run $ runStateLazy (0 :: Int) (put (1 :: Int) *> get <* put (2 :: Int))) @?= (1 :: Int, 2 :: Int)
  ]

twoStatesComp = put 'b' >> put True >> put 'c'

twoStatesTests = testCase "Two States" $
  (run $ runStateLazy 'a' $ runStateLazy False twoStatesComp) @?= (((), True), 'c')

newtype Foo m a = Foo { runFoo :: m a }
  deriving (Functor, Applicative, Monad)
instance MonadTrans Foo where
  lift = Foo
type instance CanDo (Foo m) eff = False

liftingTest = testCase "Lifting through an unknown transformer" $
  (run $ runStateLazy 'a' $ runFoo $ runStateLazy False twoStatesComp) @?= (((), True), 'c')

localState = testCase "MonadLocal StateT" $
  (run $ evalStateStrict 'a' $
    do
      s1 <- get
      (s2,s3) <- local (toEnum . (+1) . fromEnum :: Char -> Char) $ do
        s2 <- get
        put 'x'
        s3 <- get
        return (s2,s3)
      s4 <- get
      return [s1,s2,s3,s4]) @?= "abxa"

exceptTests = testGroup "Except"
  [ testCase "Catch before IO" $ do
      r <- runExcept $ runStateStrict False $ throw $ ErrorCall "foo"
      (r :: Either ErrorCall ((), Bool)) @?= Left (ErrorCall "foo")
  , testCase "Let escape to IO" $ do
      r <- try $ runExcept $ runStateStrict False $ throw UserInterrupt
      (r :: Either AsyncException (Either ErrorCall ((), Bool))) @?= Left UserInterrupt
  , testCase "Catch directly in ExceptT" $ do
      r <- runExcept $ flip catch (\UserInterrupt -> return ((), False)) $ runStateStrict True $ throw UserInterrupt
      (r :: Either AsyncException ((), Bool)) @?= Right ((), False)
  , testCase "Catch in IO" $ do
      r <- flip catch (\UserInterrupt -> return ((), False)) $ runStateStrict True $ throw UserInterrupt
      (r :: ((), Bool)) @?= ((), False)
  , testCase "Catch below ExceptT" $ do
      r <- runExcept $ runStateStrict True $ flip catch (\UserInterrupt -> put False) $ throw UserInterrupt
      (r :: Either AsyncException ((), Bool)) @?= Right ((), False)
  , testCase "withExcept in ExceptT" $ do
      let r = I.runIdentity $ runExcept $ withExcept (\UserInterrupt -> ErrorCall "foo") $ runStateStrict True $ throw UserInterrupt
      (r :: Either ErrorCall ((), Bool)) @?= Left (ErrorCall "foo")
  , testCase "withExcept in IO" $ do
      r <- try $ withExcept (\UserInterrupt -> ErrorCall "foo") $ runStateStrict True $ throw UserInterrupt
      (r :: Either ErrorCall ((), Bool)) @?= Left (ErrorCall "foo")
  ]

execTests = testCase "Exec" $ do
  r <- runWriterStrict $ exec $
    Foreign.alloca $ \ptr -> do
      Foreign.poke ptr True
      Foreign.peek ptr
  r @?= (True, ())

zoomTests = testCase "Zoom" $ do
  ((4, [2,5], 6), Record [2,5,10] 6) @?=
    (run $ runStateStrict (Record [2] 4) $ runZoom (vanLaarhoven intL) $ runZoom (vanLaarhoven listL) $ do
      (s0 :: Int) <- get
      tell [5 :: Int]
      (s1 :: [Int]) <- ask
      put (6 :: Int)
      (s2 :: Int) <- ask
      tell [10 :: Int]
      return (s0, s1, s2)
    )

liftNTests = testCase "liftN" $ do
  (run $ runReader 'a' $ runReader 'b' $ runReader 'c' $
    liftN (proxy# :: Proxy# (Suc Zero)) R.ask)
  @?= 'b'


liftConduit
  :: forall m n effM eff i o r .
     ( n ~ Find eff m
     , MonadLiftN n m
     , effM ~ Down n m
     , Monad effM
     )
  => Proxy# eff
  -> C.ConduitM i o effM r
  -> C.ConduitM i o m    r
liftConduit _ = hoist (liftN (proxy# :: Proxy# n))

liftConduitTest = testCase "lift conduit" $
  (let
    src :: C.Source I.Identity Int
    src = C.yield 1 >> C.yield 2

    sink :: C.Sink Int (W.Writer [Int]) ()
    sink =
      C.await >>=
        maybe (return ()) (\x -> do lift $ tell [x::Int]; sink)
   in
    W.execWriter $ hoist (liftN (proxy# :: Proxy# (Suc Zero))) src C.$$ sink
  ) @?= [1,2]
  {-

  execWriterStrict $ runReader (3 :: Int) $
    liftConduit (proxy# :: Proxy# (EffReader Int))
      (do
        x <- ask
        liftConduit (C.yield x)
        liftConduit (C.yield (x :: Int)))
    C.$$
      (proxy# :: Proxy# (EffWriter String)) (do C.awaitForever $ \y -> tell (show (y :: Int) ++ "\n")))
  @?= ""-}

mapWriterTest = testCase "mapWriter" $ do
  run (execWriterStrict $ mapWriter (\(w :: Char) -> [w]) $ do { tell 'a'; tell 'b'; tell 'c' }) @?= "abc"
