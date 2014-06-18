module Control.Monad.Classes.Zoom where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Data.Reflection
import Data.Proxy

newtype ZoomT q big small m a = ZoomT (m a)
  deriving (Functor, Applicative, Monad, Alternative, MonadPlus)

instance MonadTrans (ZoomT q big small) where
  lift = ZoomT

newtype VLLens big small = VLLens (forall f . Functor f => (small -> f small) -> big -> f big)

runZoom
  :: forall big small m a .
     (forall f. Functor f => (small -> f small) -> big -> f big)
  -> (forall (q :: *). Reifies q (VLLens big small) => ZoomT q big small m a)
  -> m a
runZoom l a =
  reify (VLLens l) $ \(Proxy :: Proxy q) ->
    case a :: ZoomT q big small m a of
      ZoomT a' -> a'
