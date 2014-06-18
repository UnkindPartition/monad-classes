module Control.Monad.Classes.Proxied
  ( module Control.Monad.Classes.Proxied
  , R.Reifies
  , Proxy#
  , proxy#
  )
  where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import GHC.Prim (Proxy#, proxy#)
import qualified Data.Reflection as R
import Data.Proxy

newtype Proxied x m a = Proxied (forall (q :: *). R.Reifies q x => Proxy# q -> m a)

instance Functor m => Functor (Proxied x m) where
  fmap f (Proxied g) = Proxied (\px -> fmap f (g px))

instance Applicative m => Applicative (Proxied x m) where
  pure x = Proxied (\_ -> pure x)
  Proxied a <*> Proxied b = Proxied (\px -> a px <*> b px)

instance Monad m => Monad (Proxied x m) where
  return x = Proxied (\_ -> return x)
  Proxied a >>= k = Proxied $ \px ->
    a px >>= \v ->
    case k v of
      Proxied b -> b px

instance Alternative m => Alternative (Proxied x m) where
  empty = Proxied $ \_ -> empty
  Proxied a <|> Proxied b = Proxied (\px -> a px <|> b px)

instance MonadPlus m => MonadPlus (Proxied x m) where
  mzero = Proxied $ \_ -> mzero
  Proxied a `mplus` Proxied b = Proxied (\px -> a px `mplus` b px)

instance MonadTrans (Proxied x) where
  lift a = Proxied $ \_ -> a

fromProxy# :: Proxy# a -> Proxy a
fromProxy# _ = Proxy

toProxy# :: Proxy a -> Proxy# a
toProxy# _ = proxy#

reify :: a -> (forall (q :: *). R.Reifies q a => Proxy# q -> r) -> r
reify a k = R.reify a $ \px -> k (toProxy# px)

reflect :: R.Reifies q a => Proxy# q -> a
reflect px = R.reflect (fromProxy# px)
