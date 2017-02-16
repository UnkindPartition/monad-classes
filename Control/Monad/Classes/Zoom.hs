module Control.Monad.Classes.Zoom where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Base
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Control.Monad.Classes.Core
import Control.Monad.Classes.Effects
import Control.Monad.Classes.Reader
import Control.Monad.Classes.State
import Control.Monad.Classes.Writer
import Control.Monad.Classes.Proxied
import Data.Functor.Identity
import Data.Monoid

newtype ZoomT big small m a = ZoomT (Proxied (VLLens big small) m a)
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadTrans, MonadBase b, MonadIO)

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
  -> ZoomT big small m a
  -> m a
runZoom l a =
  reify (VLLens l) $ \px ->
    case a of ZoomT (Proxied f) -> f px

type instance CanDo (ZoomT big small m) eff = ZoomCanDo small eff

type family ZoomCanDo s eff where
  ZoomCanDo s (EffState s) = 'True
  ZoomCanDo s (EffReader s) = 'True
  ZoomCanDo s (EffWriter s) = 'True
  ZoomCanDo s eff = 'False

instance MonadReader big m => MonadReaderN 'Zero small (ZoomT big small m)
  where
  askN _ = ZoomT $ Proxied $ \px -> vlGet (reflect px) `liftM` ask

instance MonadState big m => MonadStateN 'Zero small (ZoomT big small m)
  where
  stateN _ f = ZoomT $ Proxied $ \px ->
    let l = reflect px in
    state $ \s ->
      case f (vlGet l s) of
        (a, t') -> (a, vlSet l t' s)

instance (MonadState big m, Monoid small) => MonadWriterN 'Zero small (ZoomT big small m)
  where
  tellN _ w = ZoomT $ Proxied $ \px ->
    let l = reflect px in
    state $ \s ->
      let s' = vlMod' l (<> w) s
      in s' `seq` ((), s')

instance MonadTransControl (ZoomT big small) where
  type StT (ZoomT big small) a = a
  liftWith = defaultLiftWith ZoomT (\(ZoomT a) -> a)
  restoreT = defaultRestoreT ZoomT

instance MonadBaseControl b m => MonadBaseControl b (ZoomT big small m) where
    type StM (ZoomT big small m) a = StM m a
    liftBaseWith = defaultLiftBaseWith
    restoreM     = defaultRestoreM
