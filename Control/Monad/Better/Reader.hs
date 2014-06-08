module Control.Monad.Better.Reader
  ( MonadReader
  , ask
  , local
  , reader
  , MonadReaderN(..)
  )
  where
import qualified Control.Monad.Trans.Reader as Trans
import Control.Monad.Morph (hoist)
import Control.Monad.Trans.Class
import Data.Proxy
import Data.Tagged
import Control.Monad.Better.Core

class Monad m => MonadReaderN (n :: Nat) r m where
  askN :: Tagged n (m r)
  localN :: Proxy n -> ((r -> r) -> m a -> m a)
  readerN :: Proxy n -> ((r -> a) -> m a)

instance Monad m => MonadReaderN Zero r (Trans.ReaderT r m) where
  askN = Tagged Trans.ask
  localN _ = Trans.local
  readerN _ = Trans.reader

instance (MonadReaderN n r m, Monad m)
  => MonadReaderN (Suc n) r (Trans.ReaderT sr' m)
  where
    askN = retag $ fmap lift $ (askN :: Tagged n (m r))
    localN _ = \f -> hoist (localN (Proxy :: Proxy n) f)
    readerN _ = lift . readerN (Proxy :: Proxy n)

-- | The @'MonadReader' r m@ constraint asserts that @m@ is a monad stack
-- that supports a fixed environment of type @r@
type MonadReader r m = MonadReaderN (Find (Trans.ReaderT r) m) r m

-- | Fetch the environment passed through the reader monad
ask :: forall r m. MonadReader r m => m r
ask = untag (askN :: Tagged (Find (Trans.ReaderT r) m) (m r))

-- | Executes a computation in a modified environment.
local :: forall a m r. MonadReader r m
      => (r -> r) -- ^ The function to modify the environment.
      -> m a      -- ^ @Reader@ to run in the modified environment.
      -> m a
local = localN (Proxy :: Proxy (Find (Trans.ReaderT r) m))

-- | Retrieves a function of the current environment.
reader :: MonadReader r m
       => (r -> a) -- ^ The selector function to apply to the environment.
       -> m a
reader f = do
  r <- ask
  return (f r)
