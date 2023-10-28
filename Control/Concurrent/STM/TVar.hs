{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE Trustworthy                #-}
{-# LANGUAGE UnboxedTuples              #-}

module Control.Concurrent.STM.TVar (
    module Reexported

  , TVar -- opaque
  , newTVar
  , newTVarIO
  , readTVar
  , readTVarIO
  , writeTVar
  , modifyTVar
  , modifyTVar'
  , mkWeakTVar
  , registerDelay
  ) where

import Debug.AssignId
import GHC.Exts
import GHC.IO
import GHC.Weak

import Hidden.Monad.STM

import Hidden.Concurrent.STM.TVar as Reexported hiding (
    TVar
  , newTVar
  , newTVarIO
  , readTVar
  , readTVarIO
  , writeTVar
  , modifyTVar
  , modifyTVar'
  , mkWeakTVar
  , registerDelay
  )

import qualified Hidden.Concurrent.STM.TVar as STM

{-------------------------------------------------------------------------------
  Wrap TVar
-------------------------------------------------------------------------------}

newtype TVar a = WrapTVar {
      unwrapTVar :: AssignedId "TVar" (STM.TVar a)
    }
  deriving newtype (Show, Eq)

{-------------------------------------------------------------------------------
  Wrap API
-------------------------------------------------------------------------------}

newTVar :: a -> STM (TVar a)
newTVar x = fmap WrapTVar . assignSTM =<< STM.newTVar x

newTVarIO :: a -> IO (TVar a)
newTVarIO x = fmap WrapTVar . assignIO =<< STM.newTVarIO x

readTVar :: TVar a -> STM a
readTVar = STM.readTVar . forgetId . unwrapTVar

readTVarIO :: TVar a -> IO a
readTVarIO = STM.readTVarIO . forgetId . unwrapTVar

writeTVar :: TVar a -> a -> STM ()
writeTVar = STM.writeTVar . forgetId . unwrapTVar

modifyTVar :: TVar a -> (a -> a) -> STM ()
modifyTVar = STM.modifyTVar . forgetId . unwrapTVar

modifyTVar' :: TVar a -> (a -> a) -> STM ()
modifyTVar' = STM.modifyTVar' . forgetId . unwrapTVar

mkWeakTVar :: TVar a -> IO () -> IO (Weak (TVar a))
mkWeakTVar t@(WrapTVar (AssignedId (STM.TVar t#) _)) (IO finalizer) = IO $ \s ->
    case mkWeak# t# t finalizer s of (# s1, w #) -> (# s1, Weak w #)

registerDelay :: Int -> IO (TVar Bool)
registerDelay d = fmap WrapTVar . assignIO =<< STM.registerDelay d

