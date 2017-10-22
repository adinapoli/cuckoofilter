{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.CuckooFilter (
    CuckooFilter
  , new
  , insert
  , lookup
  , delete
  , fstBucket
  , sndBucket
  ) where

import Prelude hiding (lookup)
import Data.Primitive.ByteArray
import Control.Monad.Primitive
import Data.Word
import Data.Bits
import Data.CuckooFilter.Fingerprint

-- | A CuckooFilter parametrised over an item type `a`
-- is essentially a compressed cuckoo hash table which
-- has a fixed number of buckets and each bucket contains
-- 4 entries.
data CuckooFilter m a = CuckooFilter {
    maxSize :: {-# UNPACK #-} !Int
  -- ^ The internal storage's size in bytes.
  , maxNumKicks :: {-# UNPACK #-} !Int
  -- ^ The maximum number of allowed evictions before failing.
  , storage :: MutableByteArray (PrimState m)
  -- ^ The internal storage for this CuckooFilter.
  }

-- | Computes the first available bucket position for this element.
-- Referred as `i1` or `h1` in the original paper.
fstBucket :: Hashable32 a => a -> CuckooFilter m a -> Int
fstBucket a CuckooFilter{..} =
  -- Potential overflows of casting a Word32 (which is unsigned) into
  -- an Int (which is 32 bit signed on 32-bit platforms) is reified
  -- into a positive integer by `mod`.
  fromIntegral (asWord32 $ hash32 a) `mod` maxSize

-- ^ Computes the second available bucket position for this element.
-- Referred as `i2` or `h2` in the original paper.
sndBucket :: (Hashable32 a, ToFingerprint a) => a -> CuckooFilter m a -> Int
sndBucket a cf@CuckooFilter{..} =
  let (h1 :: Word64) = fromIntegral (fstBucket a cf)
      (h2 :: Word64) = fromIntegral $ asWord32 $ toHash32 (toFingerprint a)
  in (fromIntegral (h1 `xor` h2)) `mod` maxSize

-- | Creates a new `CuckooFilter` with sensible default parameters.
new :: PrimMonad m => m (CuckooFilter m a)
new = do
  let sz = 2 * 1000000 -- ~ 2MB
  ba <- newByteArray sz
  return CuckooFilter {
      maxSize = sz
    , maxNumKicks = 500
    , storage = ba
    }

insert :: a -> CuckooFilter s a -> CuckooFilter s a
insert _ cf = cf

delete :: a -> CuckooFilter s a -> CuckooFilter s a
delete _ cf = cf

lookup :: a -> CuckooFilter s a -> Bool
lookup _ _ = False
