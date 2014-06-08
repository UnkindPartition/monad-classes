module Control.Monad.MC.Reader
  ( MonadReader
  , MonadLocal
  , ask
  , local
  , reader
  , MonadReaderN(..)
  , MonadLocalN(..)
  )
  where
import qualified Control.Monad.Trans.Reader as Trans
import Control.Monad.Morph (MFunctor, hoist)
import Control.Monad.Trans.Class
import Data.Proxy
import Control.Monad.MC.Core

class Monad m => MonadReaderN (n :: Nat) r m where
  readerN :: Proxy n -> ((r -> a) -> m a)

instance Monad m => MonadReaderN Zero r (Trans.ReaderT r m) where
  readerN _ = Trans.reader

instance (MonadTrans t, Monad (t m), MonadReaderN n r m, Monad m)
  => MonadReaderN (Suc n) r (t m)
  where
    readerN _ = lift . readerN (Proxy :: Proxy n)

class Monad m => MonadLocalN (n :: Nat) r m where
  localN :: Proxy n -> ((r -> r) -> m a -> m a)

instance Monad m => MonadLocalN Zero r (Trans.ReaderT r m) where
  localN _ = Trans.local

instance (MonadTrans t, Monad (t m), MFunctor t, MonadLocalN n r m, Monad m)
  => MonadLocalN (Suc n) r (t m)
  where
    localN _ = \f -> hoist (localN (Proxy :: Proxy n) f)

-- | The @'MonadReader' r m@ constraint asserts that @m@ is a monad stack
-- that supports a fixed environment of type @r@
type MonadReader r m = MonadReaderN (Find (Trans.ReaderT r) m) r m

-- | The @'MonadLocal r m@ constraint asserts that @m@ is a monad stack
-- that supports a fixed environment of type @r@ that can be changed
-- externally to the monad
type MonadLocal r m = MonadLocalN (Find (Trans.ReaderT r) m) r m

-- | Fetch the environment passed through the reader monad
ask :: MonadReader r m => m r
ask = reader id

-- | Executes a computation in a modified environment.
local :: forall a m r. MonadLocal r m
      => (r -> r)  -- ^ The function to modify the environment.
      -> m a       -- ^ @Reader@ to run in the modified environment.
      -> m a
local = localN (Proxy :: Proxy (Find (Trans.ReaderT r) m))

-- | Retrieves a function of the current environment.
reader :: forall a r m . MonadReader r m
       => (r -> a)  -- ^ The selector function to apply to the environment.
       -> m a
reader = readerN (Proxy :: Proxy (Find (Trans.ReaderT r) m))
