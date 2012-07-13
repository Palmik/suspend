module Control.Concurrent.Delay
(
-- * Delay
  Delay(Delay)

, usDelay
, msDelay
, sDelay
, mDelay
, hDelay
, (.+.)
) where

------------------------------------------------------------------------------
import           Data.Int (Int64)
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- | DELAY
--
-- TODO:
-- * Add overflow checks.

-- | Type representing delay in microseconds.
newtype Delay = Delay Int64

-- | Sums two delays.
(.+.) :: Delay -> Delay -> Delay
(Delay x) .+. (Delay y) = Delay $ x + y
{-# INLINE (.+.) #-}

infixl 6 .+.

-- | Delay in microseconds.
usDelay :: Int64 -> Delay
usDelay = Delay
{-# INLINE usDelay #-}

-- | Delay in milliseconds.
msDelay :: Int64 -> Delay
msDelay = Delay . (1000 *)
{-# INLINE msDelay #-}

-- | Delay in seconds.
sDelay :: Int64 -> Delay
sDelay = Delay . (1000 * 1000 *)
{-# INLINE sDelay #-}

-- | Delay in minutes.
mDelay :: Int64 -> Delay
mDelay = Delay . (1000 * 1000 * 60 *)
{-# INLINE mDelay #-}

-- | Delay in hours.
hDelay :: Int64 -> Delay
hDelay = Delay . (1000 * 1000 * 60 * 24 *)
{-# INLINE hDelay #-}
