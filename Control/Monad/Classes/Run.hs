-- | Functions to run outer layers of monadic stacks.
--
-- These are provided for convenience only; you can use the running
-- functions (like 'SL.runState') from the transformers' modules directly.
--
-- Note that reader and state runners have their arguments swapped around;
-- this makes it convenient to chain them.
module Control.Monad.Classes.Run
  ( -- * Identity
    run
    -- * Reader
  , runReader
    -- * State
  , runStateLazy
  , runStateStrict
  , evalStateLazy
  , evalStateStrict
  , execStateLazy
  , execStateStrict
    -- * Writer
  , runWriterStrict
  , evalWriterStrict
  , execWriterStrict
  , evalWriterWith
    -- * Except
  , runExcept
    -- * Zoom
  , runZoom
  ) where

import Data.Functor.Identity
import Data.Monoid
import Data.Reflection
import Data.Proxy
import Control.Monad.Trans.State.Lazy as SL
import Control.Monad.Trans.State.Strict as SS
import qualified Control.Monad.Trans.Reader as R
import qualified Control.Monad.Trans.Except as Exc
import Control.Monad.Classes.Custom
import Control.Monad.Classes.Zoom

run :: Identity a -> a
run = runIdentity

runReader :: r -> R.ReaderT r m a -> m a
runReader = flip R.runReaderT

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

runWriterStrict :: (Monad m, Monoid w) => SS.StateT w m a -> m (a, w)
runWriterStrict = runStateStrict mempty

evalWriterStrict :: (Monad m, Monoid w) => SS.StateT w m a -> m a
evalWriterStrict = evalStateStrict mempty

execWriterStrict :: (Monad m, Monoid w) => SS.StateT w m a -> m w
execWriterStrict = execStateStrict mempty

evalWriterWith
  :: forall w m a . (w -> m ())
  -> (forall (q :: *). Reifies q (w -> m ()) => CustomWriterT q w m a)
  -> m a
evalWriterWith tellFn a =
  reify tellFn $ \(Proxy :: Proxy q) ->
    case a :: CustomWriterT q w m a of
      CustomWriterT a' -> a'

runZoom
  :: forall big small m a .
     (forall f. Functor f => (small -> f small) -> big -> f big)
  -> (forall (q :: *). Reifies q (VLLens big small) => ZoomT q big small m a)
  -> m a
runZoom l a =
  reify (VLLens l) $ \(Proxy :: Proxy q) ->
    case a :: ZoomT q big small m a of
      ZoomT a' -> a'

runExcept :: Exc.ExceptT e m a -> m (Either e a)
runExcept = Exc.runExceptT
