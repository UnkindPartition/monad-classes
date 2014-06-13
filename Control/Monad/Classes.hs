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
    -- * Core classes and types
    -- ** Effects
  , EffState
  , EffReader
  , EffLocal
    -- ** N-classes
  , MonadStateN(..)
  , MonadReaderN(..)
  , MonadLocalN(..)
    -- ** Type families
  , Nat(..)
  , Find
  , FindTrue
  , MapCanDo
  , CanDo
  ) where

import Control.Monad.Classes.Core
import Control.Monad.Classes.State
import Control.Monad.Classes.Reader
