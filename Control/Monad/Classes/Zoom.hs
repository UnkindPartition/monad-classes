module Control.Monad.Classes.Zoom where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class

newtype ZoomT q big small m a = ZoomT (m a)
  deriving (Functor, Applicative, Monad, Alternative, MonadPlus)

instance MonadTrans (ZoomT q big small) where
  lift = ZoomT

newtype VLLens big small = VLLens (forall f . Functor f => (small -> f small) -> big -> f big)
