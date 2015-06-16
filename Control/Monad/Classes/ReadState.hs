module Control.Monad.Classes.ReadState where

import Control.Monad.Classes.Core
import Control.Monad.Classes.Effects
import Control.Monad.Classes.Reader
import Control.Monad.Classes.State
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
newtype ReadState s m a = ReadState (m a)
  deriving (Functor, Applicative, Monad)

runReadState :: Proxy s -> ReadState s m a -> m a
runReadState _ (ReadState a) = a

type instance CanDo (ReadState s m) eff = ReadStateCanDo s eff

type family ReadStateCanDo s eff where
  ReadStateCanDo s (EffReader s) = True
  ReadStateCanDo s eff = False

instance MonadState s m => MonadReaderN Zero s (ReadState s m) where
  askN _ = ReadState get
