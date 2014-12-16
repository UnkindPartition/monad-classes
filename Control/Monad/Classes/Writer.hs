module Control.Monad.Classes.Writer where
import Control.Applicative
import Control.Monad
import qualified Control.Monad.Trans.Writer.Lazy as WL
import qualified Control.Monad.Trans.Writer.Strict as WS
import qualified Control.Monad.Trans.State.Lazy as SL
import qualified Control.Monad.Trans.State.Strict as SS
import Control.Monad.Base
import Control.Monad.Trans.Control
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad.Classes.Core
import Control.Monad.Classes.Effects
import Control.Monad.Classes.Proxied
import Data.Monoid

type instance CanDo (WL.WriterT w m) eff = WriterCanDo w eff
type instance CanDo (WS.WriterT w m) eff = WriterCanDo w eff
type instance CanDo (CustomWriterT' w n m) eff = WriterCanDo w eff

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

instance Monad m => MonadWriterN Zero w (CustomWriterT' w m m) where
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

runWriterLazy :: (Monad m, Monoid w) => WL.WriterT w m a -> m (a, w)
runWriterLazy = WL.runWriterT

evalWriterLazy :: (Monad m, Monoid w) => WL.WriterT w m a -> m a
evalWriterLazy = liftM fst . runWriterLazy

execWriterLazy :: (Monad m, Monoid w) => WL.WriterT w m a -> m w
execWriterLazy = WL.execWriterT

-- The separation between 'n' and 'm' types is needed to implement
-- the MonadTransControl instance
newtype CustomWriterT' w n m a = CustomWriterT (Proxied (w -> n ()) m a)
  deriving (Functor, Applicative, Monad, Alternative, MonadPlus, MonadBase b, MonadIO)
type CustomWriterT w m a = CustomWriterT' w m m a

instance MonadTrans (CustomWriterT' w n) where
  lift a = CustomWriterT $ Proxied $ \_ -> a

instance MonadTransControl (CustomWriterT' w n) where
  type StT (CustomWriterT' w n) a = StT (Proxied (w -> n ())) a
  liftWith = defaultLiftWith CustomWriterT (\(CustomWriterT a) -> a)
  restoreT = defaultRestoreT CustomWriterT

instance MonadBaseControl b m => MonadBaseControl b (CustomWriterT' w n m) where
  type StM (CustomWriterT' w n m) a = ComposeSt (CustomWriterT' w n) m a
  liftBaseWith = defaultLiftBaseWith
  restoreM     = defaultRestoreM

evalWriterWith
  :: forall w m a . (w -> m ())
  -> CustomWriterT w m a
  -> m a
evalWriterWith tellFn a =
  reify tellFn $ \px ->
    case a of
      CustomWriterT (Proxied a') -> a' px

-- | Transform all writer requests with a given function
mapWriter
  :: forall w1 w2 m a . MonadWriter w2 m
  => (w1 -> w2)
  -> CustomWriterT w1 m a
  -> m a
mapWriter f a =
  evalWriterWith (\w1 -> tell (f w1)) a
