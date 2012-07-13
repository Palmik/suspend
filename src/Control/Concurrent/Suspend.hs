module Control.Concurrent.Suspend
( suspend

, Delay
, usDelay
, msDelay
, sDelay
, mDelay
, hDelay
, (.+.)
) where

------------------------------------------------------------------------------
import           Control.Concurrent (threadDelay)
import           Control.Monad      (when)
------------------------------------------------------------------------------
import           Control.Concurrent.Delay
------------------------------------------------------------------------------

-- | Analogy of `Control.Concurrent.threadDelay` that allows for longer delays.
--
-- Suspends the current thread for the given delay (GHC only).
--
-- There is no guarantee that the thread will be rescheduled promptly when the
-- delay has expired, but the thread will never continue to run earlier than specified.
suspend :: Delay -> IO ()
suspend (Delay us) = when (us > 0) $ do
    let wait = min us $ fromIntegral (maxBound :: Int)
    threadDelay (fromIntegral wait)
    suspend $! Delay (us - wait)
{-# INLINEABLE suspend #-}