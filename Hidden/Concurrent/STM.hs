{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ >= 709
{-# LANGUAGE Safe #-}
#elif __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Hidden.Concurrent.STM
-- Copyright   :  (c) The University of Glasgow 2004
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (requires STM)
--
-- Software Transactional Memory: a modular composable concurrency
-- abstraction.  See
--
--  * /Composable memory transactions/, by Tim Harris, Simon Marlow, Simon
--    Peyton Jones, and Maurice Herlihy, in
--    /ACM Conference on Principles and Practice of Parallel Programming/ 2005.
--    <https://www.microsoft.com/en-us/research/publication/composable-memory-transactions/>
--
-----------------------------------------------------------------------------

module Hidden.Concurrent.STM (
        module Hidden.Monad.STM,
        module Hidden.Concurrent.STM.TVar,
#ifdef __GLASGOW_HASKELL__
        module Hidden.Concurrent.STM.TMVar,
        module Hidden.Concurrent.STM.TChan,
        module Hidden.Concurrent.STM.TQueue,
        module Hidden.Concurrent.STM.TBQueue,
#endif
        module Hidden.Concurrent.STM.TArray
  ) where

import Hidden.Monad.STM
import Hidden.Concurrent.STM.TVar
#ifdef __GLASGOW_HASKELL__
import Hidden.Concurrent.STM.TMVar
import Hidden.Concurrent.STM.TChan
#endif
import Hidden.Concurrent.STM.TArray
import Hidden.Concurrent.STM.TQueue
import Hidden.Concurrent.STM.TBQueue
