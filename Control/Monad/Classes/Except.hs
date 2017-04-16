module Control.Monad.Classes.Except where
import qualified Control.Monad.Trans.Except as Exc
import qualified Control.Monad.Trans.Maybe as Mb
import qualified Control.Exception as E
import Control.Monad
import Control.Monad.Trans.Class
import GHC.Prim (Proxy#, proxy#)
import Control.Monad.Classes.Core
import Control.Monad.Classes.Effects

type instance CanDo IO (EffExcept e) = 'True

type instance CanDo (Exc.ExceptT e m) eff = ExceptCanDo e eff

type instance CanDo (Mb.MaybeT m) eff = ExceptCanDo () eff

type family ExceptCanDo e eff where
  ExceptCanDo e (EffExcept e) = 'True
  ExceptCanDo e eff = 'False

class Monad m => MonadExceptN (n :: Nat) e m where
  throwN :: Proxy# n -> (e -> m a)

instance Monad m => MonadExceptN 'Zero e (Exc.ExceptT e m) where
  throwN _ = Exc.throwE

instance E.Exception e => MonadExceptN 'Zero e IO where
  throwN _ = E.throwIO

instance Monad m => MonadExceptN 'Zero () (Mb.MaybeT m) where
  throwN _ _ = mzero

instance (MonadTrans t, Monad (t m), MonadExceptN n e m, Monad m)
  => MonadExceptN ('Suc n) e (t m)
  where
    throwN _ = lift . throwN (proxy# :: Proxy# n)

-- | The @'MonadExcept' e m@ constraint asserts that @m@ is a monad stack
-- that supports throwing exceptions of type @e@
type MonadExcept e m = MonadExceptN (Find (EffExcept e) m) e m

-- | Throw an exception
throw :: forall a e m . MonadExcept e m => e -> m a
throw = throwN (proxy# :: Proxy# (Find (EffExcept e) m))

-- | Run an 'MonadExcept' effect using 'Exc.ExceptT'.
runExcept :: Exc.ExceptT e m a -> m (Either e a)
runExcept = Exc.runExceptT

-- | Run an 'MonadExcept' effect using 'Exc.MaybeT'.
runMaybe :: Mb.MaybeT m a -> m (Maybe a)
runMaybe = Mb.runMaybeT
