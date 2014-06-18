module Control.Monad.Classes.Custom where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class

newtype CustomWriterT q w m a = CustomWriterT (m a)
  deriving (Functor, Applicative, Monad, Alternative, MonadPlus)

instance MonadTrans (CustomWriterT q w) where
  lift = CustomWriterT
