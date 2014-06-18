module Control.Monad.Classes.Writer where
import Control.Applicative
import Control.Monad
import qualified Control.Monad.Trans.Writer.Lazy as WL
import qualified Control.Monad.Trans.Writer.Strict as WS
import qualified Control.Monad.Trans.State.Lazy as SL
import qualified Control.Monad.Trans.State.Strict as SS
import Control.Monad.Trans.Class
import Control.Monad.Classes.Core
import Control.Monad.Classes.Effects
import Control.Monad.Classes.Proxied
import Data.Monoid

type instance CanDo (WL.WriterT w m) eff = WriterCanDo w eff
type instance CanDo (WS.WriterT w m) eff = WriterCanDo w eff
type instance CanDo (CustomWriterT w m) eff = WriterCanDo w eff

type family WriterCanDo w eff where
  WriterCanDo w (EffWriter w) = True
  WriterCanDo w eff = False

class Monad m => MonadWriterN (n :: Nat) w m where
  tellN :: Proxy# n -> (w -> m ())

instance (Monad m, Monoid w) => MonadWriterN Zero w (WL.WriterT w m) where
  tellN _ = WL.tell

instance (Monad m, Monoid w) => MonadWriterN Zero w (WS.WriterT w m) where
  tellN _ = WS.tell

instance (Monad m, Monoid w) => MonadWriterN Zero w (SL.StateT w m) where
  -- lazy
  tellN _ w = SL.modify (<> w)

instance (Monad m, Monoid w) => MonadWriterN Zero w (SS.StateT w m) where
  tellN _ w = modify' (<> w)
    where
      modify' :: (s -> s) -> SS.StateT s m ()
      modify' f = SS.state (\s -> let s' = f s in s' `seq` ((), s'))

instance Monad m => MonadWriterN Zero w (CustomWriterT w m) where
  tellN _ w = CustomWriterT $ Proxied $ \px -> reflect px w

instance (MonadTrans t, Monad (t m), MonadWriterN n w m, Monad m)
  => MonadWriterN (Suc n) w (t m)
  where
    tellN _ = lift . tellN (proxy# :: Proxy# n)

-- | The @'MonadWriter' w m@ constraint asserts that @m@ is a monad stack
-- that supports outputting values of type @w@
type MonadWriter w m = MonadWriterN (Find (EffWriter w) m) w m

-- | @'tell' w@ is an action that produces the output @w@
tell :: forall w m . MonadWriter w m => w -> m ()
tell = tellN (proxy# :: Proxy# (Find (EffWriter w) m))

runWriterStrict :: (Monad m, Monoid w) => SS.StateT w m a -> m (a, w)
runWriterStrict = flip SS.runStateT mempty

evalWriterStrict :: (Monad m, Monoid w) => SS.StateT w m a -> m a
evalWriterStrict = flip SS.evalStateT mempty

execWriterStrict :: (Monad m, Monoid w) => SS.StateT w m a -> m w
execWriterStrict = flip SS.execStateT mempty

newtype CustomWriterT w m a = CustomWriterT (Proxied (w -> m ()) m a)
  deriving (Functor, Applicative, Monad, Alternative, MonadPlus)

instance MonadTrans (CustomWriterT w) where
  lift a = CustomWriterT $ Proxied $ \_ -> a

evalWriterWith
  :: forall w m a . (w -> m ())
  -> CustomWriterT w m a
  -> m a
evalWriterWith tellFn a =
  reify tellFn $ \px ->
    case a of
      CustomWriterT (Proxied a') -> a' px
