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
import Control.Monad
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
  forM_ [0 .. sz] (\i -> writeByteArray ba i emptyMarker)
  return CuckooFilter {
      maxSize = sz
    , maxNumKicks = 500
    , storage = ba
    }

insert :: (PrimMonad m, Hashable32 a, ToFingerprint a)
       => a
       -> CuckooFilter m a
       -> m ()
insert a cf@CuckooFilter{..} = do
  let f  = toFingerprint a
  let i1 = fstBucket a cf
  let i2 = sndBucket a cf
  v1 <- readByteArray storage i1
  case v1 == emptyMarker of
    True  -> writeByteArray storage i1 (toStorable f)
    False -> do
      v2 <- readByteArray storage i2
      case v2 == emptyMarker of
        True  -> writeByteArray storage i2 (toStorable f)
        False -> return ()

delete :: a -> CuckooFilter s a -> CuckooFilter s a
delete _ cf = cf

lookup :: (PrimMonad m, Hashable32 a, ToFingerprint a)
       => a
       -> CuckooFilter m a
       -> m Bool
lookup a cf@CuckooFilter{..} = do
  let f  = toFingerprint a
  let i1 = fstBucket a cf
  let i2 = sndBucket a cf
  v1 <- readByteArray storage i1
  v2 <- readByteArray storage i2
  return (v1 /= emptyMarker || v2 /= emptyMarker)
