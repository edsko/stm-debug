{-# LANGUAGE Safe #-}

module Control.Concurrent.STM (
    module Reexported

    -- Top-level
  , STMException(..)
  , atomically

    -- TVar
  , TVar
  , newTVar
  , newTVarIO
  , readTVar
  , readTVarIO
  , writeTVar
  , modifyTVar
  , modifyTVar'
  , mkWeakTVar
  , registerDelay

    -- TMVar
  , TMVar
  , newTMVar
  , newTMVarIO
  , newEmptyTMVar
  , newEmptyTMVarIO
  , readTMVar
  , tryReadTMVar
  , putTMVar
  , takeTMVar
  , tryPutTMVar
  , swapTMVar
  , mkWeakTMVar
) where

import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TMVar

import Hidden.Concurrent.STM as Reexported hiding (
    -- Top-level
    atomically

    -- TVar
  , TVar
  , newTVar
  , newTVarIO
  , readTVar
  , readTVarIO
  , writeTVar
  , modifyTVar
  , modifyTVar'
  , mkWeakTVar
  , registerDelay

    -- TMVar
  , TMVar
  , newTMVar
  , newTMVarIO
  , newEmptyTMVar
  , newEmptyTMVarIO
  , readTMVar
  , tryReadTMVar
  , putTMVar
  , takeTMVar
  , tryPutTMVar
  , swapTMVar
  , mkWeakTMVar
  )

import Control.Monad.STM (atomically, STMException(..))
