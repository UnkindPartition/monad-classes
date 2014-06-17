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
    -- * Writer
  , MonadWriter
  , tell
    -- * Exceptions
  , MonadExcept
  , throw
    -- * Core classes and types
    -- ** Effects
  , EffState
  , EffReader
  , EffLocal
  , EffWriter
  , EffExcept
    -- ** N-classes
  , Nat(..)
  , MonadStateN(..)
  , MonadReaderN(..)
  , MonadLocalN(..)
  , MonadWriterN(..)
  , MonadExceptN(..)
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
import Control.Monad.Classes.Writer
import Control.Monad.Classes.Except
