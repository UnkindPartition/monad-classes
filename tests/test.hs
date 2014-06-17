{-# LANGUAGE GeneralizedNewtypeDeriving, NoMonomorphismRestriction,
             DataKinds, TypeFamilies #-}
import Test.Tasty
import Test.Tasty.HUnit
import Control.Monad.Trans.Class
import Control.Monad.Classes
import Control.Monad.Classes.Run
import Control.Applicative
import Control.Exception hiding (throw)

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ simpleStateTests
  , twoStatesTests
  , liftingTest
  , localState
  , exceptTests
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
      r @?= Left (ErrorCall "foo")
  , testCase "Let escape to IO" $ do
      r <- try $ runExcept $ runStateStrict False $ throw UserInterrupt
      (r :: Either AsyncException (Either ErrorCall ((), Bool))) @?= Left UserInterrupt
  ]
