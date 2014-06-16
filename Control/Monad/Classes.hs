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
  , Nat(..)
  , MonadStateN(..)
  , MonadReaderN(..)
  , MonadLocalN(..)
    -- ** Type families
    -- | You should rarely need these. They are exported mostly for
    -- documentation and pedagogical purposes.
  , Find
  , FindTrue
  , MapCanDo
  , CanDo
  ) where

import Control.Monad.Classes.Core
import Control.Monad.Classes.State
import Control.Monad.Classes.Reader
