{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE Safe           #-}

module Control.Monad.STM (
    module Reexported

    -- * Redefined
  , STMException(..)
  , atomically
  ) where

import GHC.Stack
import Control.Exception

import Hidden.Monad.STM as Reexported hiding (
    atomically
  )

import qualified Hidden.Monad.STM as STM

{-------------------------------------------------------------------------------
  Redefined
-------------------------------------------------------------------------------}

data STMException = STMException CallStack SomeException
  deriving (Show, Exception)

-- | Rethrow STM exceptions with a callstack
--
-- This is especially helpful to track down "blocked indefinitely" exceptions.
--
-- Implementation note: To catch such exceptions, we /must/ have the exception
-- handler /outside/ of the STM transaction (it's the /transaction/ that blocks,
-- not individual operations /inside/ the transaction: after all, even if we
-- \"block\" on say a 'takeTMVar', then an earlier 'readTVar' might unblock the
-- transaction).
atomically :: HasCallStack => STM a -> IO a
atomically action =
    STM.atomically action `catch` (throwIO . STMException callStack)
