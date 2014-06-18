module Control.Monad.Classes.Writer
  ( MonadWriter
  , tell
  , MonadWriterN(..)
  , EffWriter
  )
  where
import qualified Control.Monad.Trans.Writer as W
import qualified Control.Monad.Trans.State.Lazy as SL
import qualified Control.Monad.Trans.State.Strict as SS
import Control.Monad.Trans.Class
import GHC.Prim (Proxy#, proxy#)
import Control.Monad.Classes.Core
import Data.Monoid

-- | Writer effect
data EffWriter (w :: *)

type instance CanDo (W.WriterT w m) eff = WriterCanDo w eff

type family WriterCanDo w eff where
  WriterCanDo w (EffWriter w) = True
  WriterCanDo w eff = False

class Monad m => MonadWriterN (n :: Nat) w m where
  tellN :: Proxy# n -> (w -> m ())

instance (Monad m, Monoid w) => MonadWriterN Zero w (W.WriterT w m) where
  tellN _ = W.tell

instance (Monad m, Monoid w) => MonadWriterN Zero w (SL.StateT w m) where
  -- lazy
  tellN _ w = SL.modify (<> w)

instance (Monad m, Monoid w) => MonadWriterN Zero w (SS.StateT w m) where
  tellN _ w = modify' (<> w)
    where
      modify' :: (s -> s) -> SS.StateT s m ()
      modify' f = SS.state (\s -> let s' = f s in s' `seq` ((), s'))

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
