{-# LANGUAGE Trustworthy #-}

module Debug.AssignId (
    AssignedId(..)
  , assignIO
  , assignSTM
  ) where

import Data.Function
import Data.IORef
import Data.Proxy
import GHC.Conc
import GHC.TypeLits
import System.IO.Unsafe (unsafePerformIO)

data AssignedId label a = AssignedId { forgetId :: a, getId :: Int }

instance KnownSymbol label => Show (AssignedId label a) where
  show x = "<" ++ symbolVal (Proxy @label) ++ " " ++ show (getId x) ++ ">"

-- | 'Eq' instance ignores the label (which is strictly for debugging only)
instance Eq a => Eq (AssignedId label a) where
  (==) = (==) `on` forgetId

nextId :: IORef Int
{-# NOINLINE nextId #-}
nextId = unsafePerformIO $ newIORef 1

getNextId :: IO Int
getNextId = atomicModifyIORef nextId $ \i -> let !i' = succ i in (i', i)

assignIO :: a -> IO (AssignedId label a)
assignIO x = AssignedId x <$> getNextId

-- This will assign a new ID each time the transaction is retried, but that's OK
assignSTM :: a -> STM (AssignedId label a)
assignSTM = unsafeIOToSTM . assignIO