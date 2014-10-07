module Control.Monad.Classes.Except where
import qualified Control.Monad.Trans.Except as Exc
import qualified Control.Monad.Trans.Maybe as Mb
import qualified Control.Exception as E
import Control.Monad
import Control.Monad.Morph (MFunctor, hoist)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Control (MonadTransControl, liftWith, restoreT)
import GHC.Prim (Proxy#, proxy#)
import Data.Type.Equality (type (==))
import Control.Monad.Classes.Core
import Control.Monad.Classes.Effects

type instance CanDo IO (EffExcept e) = True
type instance CanDo IO (EffCatch e) = True
type instance CanDo IO (EffMapExcept e) = True

type instance CanDo (Exc.ExceptT e m) eff = ExceptCanDo eff e

type family ExceptCanDo eff e where
  ExceptCanDo (EffExcept e') e = e == e'
  ExceptCanDo (EffCatch e') e = e == e'
  ExceptCanDo (EffMapExcept e') e = True
  ExceptCanDo eff e = False

type instance CanDo (Mb.MaybeT m) eff = MaybeTCanDo eff

type family MaybeTCanDo eff where
  MaybeTCanDo (EffExcept ()) = True
  MaybeTCanDo (EffCatch ()) = True
  MaybeTCanDo eff = False

class Monad m => MonadExceptN (n :: Nat) e m where
  throwN :: Proxy# n -> e -> m a

instance Monad m => MonadExceptN Zero e (Exc.ExceptT e m) where
  throwN _ = Exc.throwE

instance E.Exception e => MonadExceptN Zero e IO where
  throwN _ = E.throwIO

instance Monad m => MonadExceptN Zero () (Mb.MaybeT m) where
  throwN _ _ = mzero

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


class Monad m => MonadCatchN (n :: Nat) e m where
  catchN :: Proxy# n -> m a -> (e -> m a) -> m a

instance Monad m => MonadCatchN Zero e (Exc.ExceptT e m)
  where
    catchN _ = Exc.catchE

instance (Monad m) => MonadCatchN Zero () (Mb.MaybeT m)
  where
    catchN _ a handler = Mb.MaybeT $
        Mb.runMaybeT a >>= maybe (Mb.runMaybeT $ handler ())
                                 (return . Just)

instance E.Exception e => MonadCatchN Zero e IO
  where
    catchN _ = E.catch

instance (Monad (t m), MonadTransControl t, MonadCatchN n e m) => MonadCatchN (Suc n) e (t m)
  where
    catchN _ a handler =
        liftWith (\run -> catchN (proxy# :: Proxy# n) (run a) (run . handler))
        >>= restoreT . return

-- | The @'MonadCatch' e m@ constraint asserts that @m@ is a monad stack
-- that supports catching exceptions of type @e@
type MonadCatch e m = MonadCatchN (Find (EffCatch e) m) e m

-- | Catch an exception
--
-- Note, that it does not catch asynchronous exceptions.
catch :: forall a e m . MonadCatch e m => m a -> (e -> m a) -> m a
catch = catchN (proxy# :: Proxy# (Find (EffCatch e) m))


-- class ( Monad m
--       , MonadExceptN n e m
--       , MonadExceptN n e' (MapExceptFromMonadN n e m))
--   => MonadMapExceptN (n :: Nat) e' e m

class MonadMapExceptN (n :: Nat) e' e m
  where
    type MapExceptFromMonadN n e' m :: * -> *
    withExceptN :: Proxy# n -> (e' -> e) -> MapExceptFromMonadN n e' m a -> m a

instance
  ( MFunctor t
  , Monad (MapExceptFromMonadN n e1 m2)
  , MonadMapExceptN n e1 e2 m2
  ) => MonadMapExceptN (Suc n) e1 e2 (t m2)
  where
    type MapExceptFromMonadN (Suc n) e1 (t m2) = t (MapExceptFromMonadN n e1 m2)
    withExceptN _ f = hoist (withExceptN (proxy# :: Proxy# n) f)

instance (E.Exception e, E.Exception e') => MonadMapExceptN Zero e' e IO where
    type MapExceptFromMonadN Zero e' IO = IO
    withExceptN _ f = flip E.catch (E.throw . f)

instance Functor m => MonadMapExceptN Zero e' e (Exc.ExceptT e m)
  where
    type MapExceptFromMonadN Zero e' (Exc.ExceptT e m) = Exc.ExceptT e' m
    withExceptN _ = Exc.withExceptT

type MonadMapExcept e' e m = MonadMapExceptN (Find (EffMapExcept e') m) e' e m
type MapExceptFromMonad e' m = MapExceptFromMonadN (Find (EffMapExcept e') m) e' m

withExcept :: forall a e e' m . MonadMapExcept e' e m
  => (e' -> e) -> MapExceptFromMonad e' m a -> m a
withExcept = withExceptN (proxy# :: Proxy# (Find (EffMapExcept e') m))

runExcept :: Exc.ExceptT e m a -> m (Either e a)
runExcept = Exc.runExceptT

runMaybe :: Mb.MaybeT m a -> m (Maybe a)
runMaybe = Mb.runMaybeT

hoistEither :: MonadExcept e m => Either a e -> m a
hoistEither = either return throw

hoistMaybe :: MonadExcept () m => Maybe a -> m a
hoistMaybe = note ()

note :: MonadExcept e m => e -> Maybe a -> m a
note e = maybe (throw e) return

hush :: MonadExcept () m => Either e a -> m a
hush = either (const $ throw ()) return
