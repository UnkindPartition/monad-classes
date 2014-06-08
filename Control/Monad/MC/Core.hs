module Control.Monad.MC.Core where

data Nat = Zero | Suc Nat

type family Find (t :: (* -> *) -> (* -> *)) (m :: * -> *) :: Nat where
  Find t (t m) = Zero
  Find t (p m) = Suc (Find t m)
