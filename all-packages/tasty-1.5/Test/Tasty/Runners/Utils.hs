{-# LANGUAGE BangPatterns #-}

-- | Note: this module is re-exported as a whole from "Test.Tasty.Runners"
module Test.Tasty.Runners.Utils where

import Control.Exception
import Control.Applicative
import Control.Concurrent (mkWeakThreadId, myThreadId)
import Control.Monad (forM_)
import Data.Typeable (Typeable)
import Prelude  -- Silence AMP import warnings
import Text.Printf
import Foreign.C (CInt)

#if MIN_VERSION_base(4,11,0)
import GHC.Clock (getMonotonicTime)
#else
import Data.Time.Clock.POSIX (getPOSIXTime)
#endif

#ifdef VERSION_unix
#include "HsUnixConfig.h"
#ifdef HAVE_SIGNAL_H
#define INSTALL_HANDLERS 1
#else
#define INSTALL_HANDLERS 0
#endif
#else
#define INSTALL_HANDLERS 0
#endif

#if INSTALL_HANDLERS
import System.Posix.Signals
import System.Mem.Weak (deRefWeak)
#endif

import Test.Tasty.Core (Time)

-- | Catch possible exceptions that may arise when evaluating a string.
-- For normal (total) strings, this is a no-op.
--
-- This function should be used to display messages generated by the test
-- suite (such as test result descriptions).
--
-- See e.g. <https://github.com/UnkindPartition/tasty/issues/25>.
--
-- @since 0.10.1
formatMessage :: String -> IO String
formatMessage = go 3
  where
    -- to avoid infinite recursion, we introduce the recursion limit
    go :: Int -> String -> IO String
    go 0        _ = return "exceptions keep throwing other exceptions!"
    go recLimit msg = do
      mbStr <- try $ evaluate $ forceElements msg
      case mbStr of
        Right () -> return msg
        Left e' -> printf "message threw an exception: %s" <$> go (recLimit-1) (show (e' :: SomeException))

-- | Force elements of a list
-- (<https://ro-che.info/articles/2015-05-28-force-list>).
--
-- @since 1.0.1
forceElements :: [a] -> ()
forceElements = foldr seq ()

-- from https://ro-che.info/articles/2014-07-30-bracket
-- | Install signal handlers so that e.g. the cursor is restored if the test
-- suite is killed by SIGTERM. Upon a signal, a 'SignalException' will be
-- thrown to the thread that has executed this action.
--
-- This function is called automatically from the @defaultMain*@ family of
-- functions. You only need to call it explicitly if you call
-- 'Test.Tasty.Runners.tryIngredients' yourself.
--
-- This function does nothing when POSIX signals are not supported.
--
-- @since 1.2.1
installSignalHandlers :: IO ()
installSignalHandlers = do
#if INSTALL_HANDLERS
  main_thread_id <- myThreadId
  weak_tid <- mkWeakThreadId main_thread_id
  forM_ [ sigHUP, sigTERM, sigUSR1, sigUSR2, sigXCPU, sigXFSZ ] $ \sig ->
    installHandler sig (Catch $ send_exception weak_tid sig) Nothing
  where
    send_exception weak_tid sig = do
      m <- deRefWeak weak_tid
      case m of
        Nothing  -> return ()
        Just tid -> throwTo tid (toException $ SignalException sig)
#else
  return ()
#endif

-- | This exception is thrown when the program receives a signal, assuming
-- 'installSignalHandlers' was called.
--
-- The 'CInt' field contains the signal number, as in
-- 'System.Posix.Signals.Signal'. We don't use that type synonym, however,
-- because it's not available on non-UNIXes.
--
-- @since 1.2.1
newtype SignalException = SignalException CInt
  deriving (Show, Typeable)
instance Exception SignalException

-- | Measure the time taken by an 'IO' action to run.
--
-- @since 1.2.2
timed :: IO a -> IO (Time, a)
timed t = do
  start <- getTime
  !r    <- t
  end   <- getTime
  return (end-start, r)

#if MIN_VERSION_base(4,11,0)
-- | Get monotonic time.
--
-- Warning: This is not the system time, but a monotonically increasing time
-- that facilitates reliable measurement of time differences.
--
-- @since 1.2.2
getTime :: IO Time
getTime = getMonotonicTime
#else
-- | Get system time.
--
-- @since 1.2.2
getTime :: IO Time
getTime = realToFrac <$> getPOSIXTime
#endif