module Control.Monad.MC.State
  ( MonadState
  , get
  , put
  , state
  , modify
  , modify'
  , gets
  , MonadStateN(..)
  , EffState
  )
  where
import qualified Control.Monad.Trans.State.Lazy as SL
import qualified Control.Monad.Trans.State.Strict as SS
import Control.Monad.Trans.Class
import Data.Proxy
import Control.Monad.MC.Core

data EffState s -- effect

type instance CanDo (SS.StateT s m) eff = StateCanDo s eff
type instance CanDo (SL.StateT s m) eff = StateCanDo s eff

type family StateCanDo s eff where
  StateCanDo s (EffState s) = True
  StateCanDo s eff = False

class Monad m => MonadStateN (n :: Nat) s m where
  stateN :: Proxy n -> ((s -> (a, s)) -> m a)

instance Monad m => MonadStateN Zero s (SL.StateT s m) where
  stateN _ = SL.state

instance Monad m => MonadStateN Zero s (SS.StateT s m) where
  stateN _ = SS.state

instance (Monad (t m), MonadTrans t, MonadStateN n s m, Monad m)
  => MonadStateN (Suc n) s (t m)
  where
    stateN _ = lift . stateN (Proxy :: Proxy n)

-- | The @'MonadState' s m@ constraint asserts that @m@ is a monad stack
-- that supports state operations on type @s@
type MonadState s m = MonadStateN (Find (EffState s) m) s m

-- | Construct a state monad computation from a function
state :: forall s m a. (MonadState s m) => (s -> (a, s)) -> m a
state = stateN (Proxy :: Proxy (Find (EffState s) m))

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
