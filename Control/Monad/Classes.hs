module Control.Monad.Classes
  ( -- * State
    MonadState
  , get
  , put
  , modify
  , modify'
  , gets
    -- * Reader
  , MonadReader
  , MonadLocal
  , ask
  , local
  , reader
    -- * Core classes
  , MonadStateN(..)
  , MonadReaderN(..)
  , MonadLocalN(..)
  , Nat(..)
  , Find
  ) where

import Control.Monad.Classes.Core
import Control.Monad.Classes.State
import Control.Monad.Classes.Reader
