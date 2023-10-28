{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE Trustworthy                #-}
{-# LANGUAGE UnboxedTuples              #-}

module Control.Concurrent.STM.TMVar (
    module Reexported

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

import Debug.AssignId
import GHC.Exts
import GHC.IO
import GHC.Weak

import Hidden.Monad.STM

import Hidden.Concurrent.STM.TMVar as Reexported hiding (
    TMVar
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

import qualified Hidden.Concurrent.STM.TMVar as STM
import qualified GHC.Conc as GHC

{-------------------------------------------------------------------------------
  Wrap TMVar
-------------------------------------------------------------------------------}

newtype TMVar a = WrapTMVar {
      unwrapTMVar :: AssignedId "TMVar" (STM.TMVar a)
    }
  deriving newtype (Show, Eq)

newTMVar :: a -> STM (TMVar a)
newTMVar x = fmap WrapTMVar . assignSTM =<< STM.newTMVar x

newTMVarIO :: a -> IO (TMVar a)
newTMVarIO x = fmap WrapTMVar . assignIO =<< STM.newTMVarIO x

newEmptyTMVar :: STM (TMVar a)
newEmptyTMVar = fmap WrapTMVar . assignSTM =<< STM.newEmptyTMVar

newEmptyTMVarIO :: IO (TMVar a)
newEmptyTMVarIO = fmap WrapTMVar . assignIO =<< STM.newEmptyTMVarIO

putTMVar :: TMVar a -> a -> STM ()
putTMVar = STM.putTMVar . forgetId . unwrapTMVar

readTMVar :: TMVar a -> STM a
readTMVar = STM.readTMVar . forgetId . unwrapTMVar

takeTMVar :: TMVar a -> STM a
takeTMVar = STM.takeTMVar . forgetId . unwrapTMVar

tryPutTMVar :: TMVar a -> a -> STM Bool
tryPutTMVar = STM.tryPutTMVar . forgetId . unwrapTMVar

tryReadTMVar :: TMVar a -> STM (Maybe a)
tryReadTMVar = STM.tryReadTMVar . forgetId . unwrapTMVar

swapTMVar :: TMVar a -> a -> STM a
swapTMVar = STM.swapTMVar . forgetId . unwrapTMVar

-- 'Weak' is not a functor so we cannot reuse 'mkWeakTMVar'
mkWeakTMVar :: TMVar a -> IO () -> IO (Weak (TMVar a))
mkWeakTMVar tmv@(WrapTMVar (AssignedId (STM.TMVar (GHC.TVar t#)) _))
            (IO finalizer)
          = IO $ \s ->
    case mkWeak# t# tmv finalizer s of (# s1, w #) -> (# s1, Weak w #)
