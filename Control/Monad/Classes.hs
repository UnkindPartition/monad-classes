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
    -- * Writer
  , MonadWriter
  , tell
    -- * Exceptions
  , MonadExcept
  , throw
  , catch
  , withExcept
  , note
  , hush
  , hoistMaybe
  , hoistEither
    -- * Exec
  , MonadExec
  , exec
    -- * Core classes and types
    -- ** Generic lifting
  , MonadLiftN(..)
    -- ** Effects
  , module Control.Monad.Classes.Effects
    -- ** N-classes
  , Nat(..)
  , MonadStateN(..)
  , MonadReaderN(..)
  , MonadLocalN(..)
  , MonadWriterN(..)
  , MonadExceptN(..)
  , MonadExecN(..)
    -- ** Type families
    -- | You should rarely need these. They are exported mostly for
    -- documentation and pedagogical purposes.
  , Find
  , FindTrue
  , MapCanDo
  , CanDo
  ) where

import Control.Monad.Classes.Effects
import Control.Monad.Classes.Core
import Control.Monad.Classes.State
import Control.Monad.Classes.Reader
import Control.Monad.Classes.Writer
import Control.Monad.Classes.Except
import Control.Monad.Classes.Exec
