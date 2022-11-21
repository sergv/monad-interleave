-----------------------------------------------------------------------------
-- |
-- Module     : Control.Monad.Interleave
-- Copyright  : Copyright (c) Patrick Perry <patperry@stanford.edu>, Sergey Vinokurov <serg.foo@gmail.com>
-- License    : BSD3
-- Maintainer : Sergey Vinokurov <serg.foo@gmail.com>
--
-- Monads with an unsaveInterleaveIO-like operation.

module Control.Monad.Interleave
  ( MonadInterleave(..)
  ) where

import qualified Control.Monad.ST as Strict
import qualified Control.Monad.ST.Lazy as Lazy
import qualified Control.Monad.ST.Lazy.Unsafe as Lazy
import qualified Control.Monad.ST.Unsafe as Strict
import System.IO.Unsafe

-- | Monads that have an operation like 'unsafeInterleaveIO'.
class Monad m => MonadInterleave m where
  -- | Get the baton from the monad without doing any computation.
  unsafeInterleave :: m a -> m a

instance MonadInterleave IO where
  {-# INLINE unsafeInterleave #-}
  unsafeInterleave = unsafeInterleaveIO

instance MonadInterleave (Strict.ST s) where
  {-# INLINE unsafeInterleave #-}
  unsafeInterleave = Strict.unsafeInterleaveST

instance MonadInterleave (Lazy.ST s) where
  {-# INLINE unsafeInterleave #-}
  unsafeInterleave = Lazy.unsafeInterleaveST
