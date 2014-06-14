{-# LANGUAGE UndecidableInstances #-}
module Control.Monad.Classes.Core where

-- | Peano naturals; used at the type level to denote how far a
-- computation should be lifted
data Nat = Zero | Suc Nat

-- | @'CanDo' m eff@ describes whether the given effect can be performed in the
-- monad @m@ (without any additional lifting)
type family CanDo (m :: (* -> *)) (eff :: k) :: Bool

-- | @'MapCanDo' eff stack@ maps the type-level function @(\m -> 'CanDo'
-- m eff)@ over all layers that a monad transformer stack @stack@ consists of
type family MapCanDo (eff :: k) (stack :: * -> *) :: [Bool] where
  MapCanDo eff (t m)= (CanDo (t m) eff) ': MapCanDo eff m
  MapCanDo eff m = '[ CanDo eff m ]

-- | @'FindTrue' bs@ returns a (type-level) index of the first occurrence
-- of 'True' in a list of booleans
type family FindTrue
  (bs :: [Bool]) -- results of calling Contains
  :: Nat
  where
  FindTrue (True ': t) = Zero
  FindTrue (False ': t) = Suc (FindTrue t)

-- | @'Find' eff m@ finds the first transformer in a monad transformer
-- stack that can handle the effect @eff@
type Find eff (m :: * -> *) =
  FindTrue (MapCanDo eff m)
