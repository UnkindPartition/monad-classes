module Control.Monad.Better.State
  ( MonadState
  , get
  , put
  , state
  , modify
  , modify'
  , gets
  , MonadStateN(..)
  )
  where
import qualified Control.Monad.Trans.State as Trans
import Control.Monad.Trans.Class
import Data.Proxy
import Control.Monad.Better.Core

class Monad m => MonadStateN (n :: Nat) s m where
  stateN :: Proxy n -> ((s -> (a, s)) -> m a)

instance Monad m => MonadStateN Zero s (Trans.StateT s m) where
  stateN _ = Trans.state

instance (MonadStateN n s m, Monad m)
  => MonadStateN (Suc n) s (Trans.StateT s' m)
  where
    stateN _ = lift . stateN (Proxy :: Proxy n)

-- | The @'MonadState' s m@ constraint asserts that @m@ is a monad stack
-- that supports state operations on type @s@
type MonadState s m = MonadStateN (Find (Trans.StateT s) m) s m

-- | Construct a state monad computation from a function
state :: forall s m a. (MonadState s m) => (s -> (a, s)) -> m a
state = stateN (Proxy :: Proxy (Find (Trans.StateT s) m))

-- | @'put' s@ sets the state within the monad to @s@
put :: MonadState s m => s -> m ()
put s = state $ \_ -> ((), s)

-- | Fetch the current value of the state within the monad
get :: MonadState a m => m a
get = state $ \s -> (s, s)

-- | Gets specific component of the state, using a projection function
-- supplied.
gets :: MonadState s m => (s -> a) -> m a
gets f = do
    s <- get
    return (f s)

-- | Maps an old state to a new state inside a state monad layer
modify :: MonadState s m => (s -> s) -> m ()
modify f = state (\s -> ((), f s))

-- | A variant of 'modify' in which the computation is strict in the
-- new state
modify' :: MonadState s m => (s -> s) -> m ()
modify' f = state (\s -> let s' = f s in s' `seq` ((), s'))
