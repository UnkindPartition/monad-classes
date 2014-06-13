{-# LANGUAGE UndecidableInstances #-}
module Control.Monad.MC.Core where

data Nat = Zero | Suc Nat

type family CanDo (t :: (* -> *)) (eff :: k) :: Bool

type family MapCanDo (x :: k) (m :: * -> *) :: [Bool] where
  MapCanDo eff (t m)= (CanDo (t m) eff) ': MapCanDo eff m
  MapCanDo eff m = '[ CanDo eff m ]

type family FindTrue
  (bs :: [Bool]) -- results of calling Contains
  :: Nat
  where
  FindTrue (True ': t) = Zero
  FindTrue (False ': t) = Suc (FindTrue t)

type Find eff (m :: * -> *) =
  FindTrue (MapCanDo eff m)
