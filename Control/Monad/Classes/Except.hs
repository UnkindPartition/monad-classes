module Control.Monad.Classes.Except
  ( MonadExcept
  , throw
  , MonadExceptN(..)
  , EffExcept
  )
  where
import qualified Control.Monad.Trans.Except as Exc
import qualified Control.Exception as E
import Control.Monad.Trans.Class
import GHC.Prim (Proxy#, proxy#)
import Control.Monad.Classes.Core

-- | Except effect
data EffExcept (e :: *)

type instance CanDo IO (EffExcept e) = True

type instance CanDo (Exc.ExceptT e m) eff = ExceptCanDo e eff

type family ExceptCanDo e eff where
  ExceptCanDo e (EffExcept e) = True
  ExceptCanDo e eff = False

class Monad m => MonadExceptN (n :: Nat) e m where
  throwN :: Proxy# n -> (e -> m a)

instance (Monad m) => MonadExceptN Zero e (Exc.ExceptT e m) where
  throwN _ = Exc.throwE

instance (E.Exception e) => MonadExceptN Zero e IO where
  throwN _ = E.throwIO

instance (MonadTrans t, Monad (t m), MonadExceptN n e m, Monad m)
  => MonadExceptN (Suc n) e (t m)
  where
    throwN _ = lift . throwN (proxy# :: Proxy# n)

-- | The @'MonadExcept' e m@ constraint asserts that @m@ is a monad stack
-- that supports throwing exceptions of type @e@
type MonadExcept e m = MonadExceptN (Find (EffExcept e) m) e m

-- | Throw an exception
throw :: forall a e m . MonadExcept e m => e -> m a
throw = throwN (proxy# :: Proxy# (Find (EffExcept e) m))
