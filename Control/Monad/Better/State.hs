module Control.Monad.Better.State where
import Control.Monad.Trans.State hiding (get, put)
import qualified Control.Monad.Trans.State as Trans
import Control.Monad.Trans.Class
import Data.Tagged
import Control.Monad.Better.Core

class Monad m => MonadStateN (n :: Nat) s m where
  getN :: Tagged n (m s)
  putN :: Tagged n (s -> m ())

instance Monad m => MonadStateN Zero s (StateT s m) where
  getN = Tagged Trans.get
  putN = Tagged Trans.put

instance (MonadStateN n s m, Monad m)
  => MonadStateN (Suc n) s (StateT s' m)
  where
    getN = retag . fmap lift $ (getN :: Tagged n (m s))
    putN = retag . fmap (lift .) $ (putN :: Tagged n (s -> m ()))

type MonadState s m = MonadStateN (Find (StateT s) m) s m

get :: forall s m. (MonadState s m) => m s
get = untag (getN :: Tagged (Find (StateT s) m) (m s))

put :: forall s m . (MonadState s m) => s -> m ()
put = untag (putN :: Tagged (Find (StateT s) m) (s -> m ()))
