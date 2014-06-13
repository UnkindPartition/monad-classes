{-# LANGUAGE GeneralizedNewtypeDeriving, NoMonomorphismRestriction,
             DataKinds, TypeFamilies #-}
import Test.Tasty
import Test.Tasty.HUnit
import Control.Monad.Trans.Class
import Control.Monad.Classes
import Control.Monad.Classes.Run
import Control.Applicative

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ simpleStateTests
  , twoStatesTests
  , liftingTest
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
