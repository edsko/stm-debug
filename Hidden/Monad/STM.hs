{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP, MagicHash, UnboxedTuples #-}

#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Hidden.Monad.STM
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
-- This module only defines the 'STM' monad; you probably want to
-- import "Hidden.Concurrent.STM" (which exports "Hidden.Monad.STM").
--
-- Note that invariant checking (namely the @always@ and @alwaysSucceeds@
-- functions) has been removed. See ticket [#14324](https://ghc.haskell.org/trac/ghc/ticket/14324) and
-- the [removal proposal](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0011-deprecate-stm-invariants.rst).
-- Existing users are encouraged to encapsulate their STM operations in safe
-- abstractions which can perform the invariant checking without help from the
-- runtime system.

-----------------------------------------------------------------------------

module Hidden.Monad.STM (
        STM,
        atomically,
#ifdef __GLASGOW_HASKELL__
        retry,
        orElse,
        check,
#endif
        throwSTM,
        catchSTM
  ) where

#ifdef __GLASGOW_HASKELL__
#if ! (MIN_VERSION_base(4,3,0))
import GHC.Conc hiding (catchSTM)
import Control.Monad    ( MonadPlus(..) )
import Control.Exception
#else
import GHC.Conc
#endif
import GHC.Exts
import Control.Monad.Fix
#else
import Hidden.Sequential.STM
#endif

#ifdef __GLASGOW_HASKELL__
#if ! (MIN_VERSION_base(4,3,0))
import Hidden.Applicative
import Control.Monad (ap)
#endif
#endif

#if !MIN_VERSION_base(4,17,0)
import Control.Monad (liftM2)
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup (Semigroup (..))
#endif
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (Monoid (..))
#endif
#endif


#ifdef __GLASGOW_HASKELL__
#if ! (MIN_VERSION_base(4,3,0))
instance MonadPlus STM where
  mzero = retry
  mplus = orElse

instance Applicative STM where
  pure = return
  (<*>) = ap

instance Alternative STM where
  empty = retry
  (<|>) = orElse
#endif

-- | Check that the boolean condition is true and, if not, 'retry'.
--
-- In other words, @check b = unless b retry@.
--
-- @since 2.1.1
check :: Bool -> STM ()
check b = if b then return () else retry
#endif

#if ! (MIN_VERSION_base(4,3,0))
-- |Exception handling within STM actions.
catchSTM :: Exception e => STM a -> (e -> STM a) -> STM a
catchSTM (STM m) handler = STM $ catchSTM# m handler'
    where
      handler' e = case fromException e of
                     Just e' -> case handler e' of STM m' -> m'
                     Nothing -> raiseIO# e

-- | A variant of 'throw' that can only be used within the 'STM' monad.
--
-- Throwing an exception in @STM@ aborts the transaction and propagates the
-- exception. (Note: Allocation effects, such as  'newTVar' are not rolled back
-- when this happens. All other effects are discarded. See <https://gitlab.haskell.org/ghc/ghc/-/issues/18453 ghc#18453.>)
--
-- Although 'throwSTM' has a type that is an instance of the type of 'throw', the
-- two functions are subtly different:
--
-- > throw e    `seq` x  ===> throw e
-- > throwSTM e `seq` x  ===> x
--
-- The first example will cause the exception @e@ to be raised,
-- whereas the second one won\'t.  In fact, 'throwSTM' will only cause
-- an exception to be raised when it is used within the 'STM' monad.
-- The 'throwSTM' variant should be used in preference to 'throw' to
-- raise an exception within the 'STM' monad because it guarantees
-- ordering with respect to other 'STM' operations, whereas 'throw'
-- does not.
throwSTM :: Exception e => e -> STM a
throwSTM e = STM $ raiseIO# (toException e)
#endif


data STMret a = STMret (State# RealWorld) a

liftSTM :: STM a -> State# RealWorld -> STMret a
liftSTM (STM m) = \s -> case m s of (# s', r #) -> STMret s' r

-- | @since 2.3
instance MonadFix STM where
  mfix k = STM $ \s ->
    let ans        = liftSTM (k r) s
        STMret _ r = ans
    in case ans of STMret s' x -> (# s', x #)

#if !MIN_VERSION_base(4,17,0)
instance Semigroup a => Semigroup (STM a) where
    (<>) = liftM2 (<>)

instance Monoid a => Monoid (STM a) where
    mempty = return mempty
#if !MIN_VERSION_base(4,13,0)
    mappend = liftM2 mappend
#endif
#endif
