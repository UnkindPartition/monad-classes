-- | Functions to run outer layers of monadic stacks.
--
-- These are provided for convenience only; you can use the running
-- functions (like 'SL.runState') from the transformers' modules directly.
--
-- Note that reader and state runners have their arguments swapped around;
-- this makes it convenient to chain them.
module Control.Monad.Better.Run
  ( -- * Identity
    run
    -- * State
  , runStateLazy
  , runStateStrict
  , evalStateLazy
  , evalStateStrict
  , execStateLazy
  , execStateStrict
  ) where

import Data.Functor.Identity
import Control.Monad.Trans.State.Lazy as SL
import Control.Monad.Trans.State.Strict as SS

run :: Identity a -> a
run = runIdentity

runStateLazy   :: s -> SL.StateT s m a -> m (a, s)
runStateLazy   =  flip SL.runStateT
runStateStrict :: s -> SS.StateT s m a -> m (a, s)
runStateStrict =  flip SS.runStateT

evalStateLazy   :: Monad m => s -> SL.StateT s m a -> m a
evalStateLazy   =  flip SL.evalStateT
evalStateStrict :: Monad m => s -> SS.StateT s m a -> m a
evalStateStrict =  flip SS.evalStateT

execStateLazy   :: Monad m => s -> SL.StateT s m a -> m s
execStateLazy   = flip SL.execStateT
execStateStrict :: Monad m => s -> SS.StateT s m a -> m s
execStateStrict = flip SS.execStateT
