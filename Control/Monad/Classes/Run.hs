-- | Functions to run outer layers of monadic stacks.
--
-- These are provided for convenience only; you can use the running
-- functions (like 'SL.runState') from the transformers' modules directly.
--
-- Note that reader and state runners have their arguments swapped around;
-- this makes it convenient to chain them.
module Control.Monad.Classes.Run
  ( -- * Identity
    run
    -- * Reader
  , runReader
    -- * State
  , runStateLazy
  , runStateStrict
  , evalStateLazy
  , evalStateStrict
  , execStateLazy
  , execStateStrict
    -- * Writer
  , runWriterLazy
  , runWriterStrict
  , evalWriterLazy
  , evalWriterStrict
  , execWriterLazy
  , execWriterStrict
  , evalWriterWith
  , mapWriter
  , CustomWriterT'(..)
  , CustomWriterT
    -- * Except
  , runExcept
  , runMaybe
    -- * Zoom
  , runZoom
  , ZoomT(..)
    -- * ReadState
  , ReadStateT(..)
  , runReadState
  ) where

import Data.Functor.Identity
import Control.Monad.Classes.Zoom
import Control.Monad.Classes.State
import Control.Monad.Classes.Writer
import Control.Monad.Classes.Reader
import Control.Monad.Classes.Except
import Control.Monad.Classes.ReadState

run :: Identity a -> a
run = runIdentity
