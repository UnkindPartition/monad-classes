module Control.Monad.Better.State
  ( MonadState
  , get
  , put
  , state
  , modify
  , modify'
  , MonadStateN(..)
  )
  where
import qualified Control.Monad.Trans.State as Trans
import Control.Monad.Trans.Class
import Data.Tagged
import Control.Monad.Better.Core

class Monad m => MonadStateN (n :: Nat) s m where
  stateN :: Tagged n ((s -> (a, s)) -> m a)

instance Monad m => MonadStateN Zero s (Trans.StateT s m) where
  stateN = Tagged Trans.state

instance (MonadStateN n s m, Monad m)
  => MonadStateN (Suc n) s (Trans.StateT s' m)
  where
    stateN = retag . fmap (lift .) $ (stateN :: Tagged n ((s -> (a, s)) -> m a))

-- | The @'MonadState' s m@ constraint asserts that @m@ is a monad stack
-- that supports state operations on type @s@
type MonadState s m = MonadStateN (Find (Trans.StateT s) m) s m

-- | Construct a state monad computation from a function
state :: forall s m a. (MonadState s m) => (s -> (a, s)) -> m a
state = untag (stateN :: Tagged (Find (Trans.StateT s) m) ((s -> (a, s)) -> m a))

-- | @'put' s@ sets the state within the monad to @s@
put :: MonadState s m => s -> m ()
put s = state $ \_ -> ((), s)

-- | Fetch the current value of the state within the monad
get :: MonadState a m => m a
get = state $ \s -> (s, s)


-- | Maps an old state to a new state inside a state monad layer
modify :: MonadState s m => (s -> s) -> m ()
modify f = state (\s -> ((), f s))

-- | A variant of 'modify' in which the computation is strict in the
-- new state
modify' :: MonadState s m => (s -> s) -> m ()
modify' f = state (\s -> let s' = f s in s' `seq` ((), s'))
