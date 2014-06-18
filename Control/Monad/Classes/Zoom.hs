module Control.Monad.Classes.Zoom where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Classes.Core
import Control.Monad.Classes.Effects
import Control.Monad.Classes.Reader
import Control.Monad.Classes.State
import Control.Monad.Classes.Writer
import Data.Functor.Identity
import Data.Monoid
import Data.Reflection
import Data.Proxy

newtype ZoomT q big small m a = ZoomT (m a)
  deriving (Functor, Applicative, Monad, Alternative, MonadPlus)

instance MonadTrans (ZoomT q big small) where
  lift = ZoomT

newtype VLLens big small = VLLens (forall f . Functor f => (small -> f small) -> big -> f big)

vlGet :: VLLens b a -> b -> a
vlGet (VLLens l) s = getConst (l Const s)

vlSet :: VLLens b a -> a -> b -> b
vlSet (VLLens l) v s = runIdentity (l (\_ -> Identity v) s)

-- N.B. applies function eagerly
vlMod' :: VLLens b a -> (a -> a) -> b -> b
vlMod' (VLLens l) f s = runIdentity (l (\x -> Identity $! f x) s)

runZoom
  :: forall big small m a .
     (forall f. Functor f => (small -> f small) -> big -> f big)
  -> (forall (q :: *). Reifies q (VLLens big small) => ZoomT q big small m a)
  -> m a
runZoom l a =
  reify (VLLens l) $ \(Proxy :: Proxy q) ->
    case a :: ZoomT q big small m a of
      ZoomT a' -> a'

type instance CanDo (ZoomT q big small m) eff = ZoomCanDo small eff

type family ZoomCanDo s eff where
  ZoomCanDo s (EffState s) = True
  ZoomCanDo s (EffReader s) = True
  ZoomCanDo s (EffWriter s) = True
  ZoomCanDo s eff = False

instance
  ( MonadReader big m
  , Reifies q (VLLens big small)
  )
  => MonadReaderN Zero small (ZoomT q big small m)
  where
  askN _ = ZoomT $ vlGet (reflect (Proxy :: Proxy q)) `liftM` ask

instance
  ( MonadState big m
  , Reifies q (VLLens big small)
  )
  => MonadStateN Zero small (ZoomT q big small m)
  where
  stateN _ f = ZoomT $ state $ \s ->
    case f (vlGet l s) of
      (a, t') -> (a, vlSet l t' s)
    where
      l = reflect (Proxy :: Proxy q)

instance
  ( MonadState big m
  , Reifies q (VLLens big small)
  , Monoid small
  )
  => MonadWriterN Zero small (ZoomT q big small m)
  where
  tellN _ w = ZoomT $ state $ \s ->
    let s' = vlMod' l (<> w) s
    in s' `seq` ((), s')
    where
      l = reflect (Proxy :: Proxy q)
