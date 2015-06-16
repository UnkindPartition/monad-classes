module Control.Monad.Classes.ReadState where

import Control.Monad.Classes.Core
import Control.Monad.Classes.Effects
import Control.Monad.Classes.Reader
import Control.Monad.Classes.State
import Control.Monad.Trans.Class
import Control.Applicative
import Data.Proxy

-- | 'ReadState' is used to translate reader effects into state effects.
--
-- If you run a computation with 'StateT', this handler is not needed,
-- since 'StateT' already handles read requests.
--
-- This is useful in cases when you work in an abstract 'MonadState' monad
-- and thus have no guarantee that its handler will also accept reader
-- requests.
newtype ReadStateT s m a = ReadStateT (m a)
  deriving (Functor, Applicative, Monad)

instance MonadTrans (ReadStateT s) where
  lift = ReadStateT

runReadState :: Proxy s -> ReadStateT s m a -> m a
runReadState _ (ReadStateT a) = a

type instance CanDo (ReadStateT s m) eff = ReadStateCanDo s eff

type family ReadStateCanDo s eff where
  ReadStateCanDo s (EffReader s) = True
  ReadStateCanDo s eff = False

instance MonadState s m => MonadReaderN Zero s (ReadStateT s m) where
  askN _ = ReadStateT get
