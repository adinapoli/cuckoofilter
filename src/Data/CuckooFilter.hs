{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
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
import System.Random.MWC
import Control.Monad.Primitive
import Control.Monad
import Data.Word
import Data.Bits
import Data.CuckooFilter.Fingerprint

numEntries :: Int
numEntries = 4

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
  , randomGen :: Gen (PrimState m)
  }

data InsertionFailure =
  MaxKicksExhausted

data CuckooFilterError =
  InsertionFailed InsertionFailure

type BucketIndex = Int

-- | Computes the first available bucket position for this element.
-- Referred as `i1` or `h1` in the original paper.
fstBucket :: Hashable32 a => a -> CuckooFilter m a -> BucketIndex
fstBucket a CuckooFilter{..} =
  -- Potential overflows of casting a Word32 (which is unsigned) into
  -- an Int (which is 32 bit signed on 32-bit platforms) is reified
  -- into a positive integer by `mod`.
  fromIntegral (toWord32 a) `mod` maxSize

-- ^ Computes the second available bucket position for this element.
-- Referred as `i2` or `h2` in the original paper.
sndBucket :: Hashable32 a => a -> CuckooFilter m a -> BucketIndex
sndBucket a cf@CuckooFilter{..} =
  let (h1 :: Word64) = fromIntegral (fstBucket a cf)
      (h2 :: Word64) = fromIntegral (toWord32 a)
  in fromIntegral (h1 `xor` h2) `mod` maxSize

-- | Generic function to get a 'BucketIndex' out of an already existent
-- 'BucketIndex' and an input 'Fingerprint'.
toBucket :: BucketIndex -> Fingerprint -> CuckooFilter m a -> BucketIndex
toBucket i f CuckooFilter{..} =
  let (h1 :: Word64) = fromIntegral i
      (h2 :: Word64) = fromIntegral (unsafeExtractFingerprint f)
  in fromIntegral (h1 `xor` h2) `mod` maxSize

-- | Creates a new `CuckooFilter` with sensible default parameters.
new :: PrimMonad m => m (CuckooFilter m a)
new = do
  let sz = 2 * 4 * 1000000 -- ~ 8MB
  ba <- newByteArray sz
  gen <- create
  forM_ [0 .. sz] (\i -> writeByteArray ba i emptyMarker)
  return CuckooFilter {
      maxSize = sz
    , maxNumKicks = 500
    , storage = ba
    , randomGen = gen
    }

-- Try inserting at index 'i', and iterate up until 'numEntries'
-- in trying finding an empty slot.
tryInsert :: forall m a. PrimMonad m
          => Fingerprint
          -> BucketIndex
          -> CuckooFilter m a
          -> m Bool
tryInsert fp bucketIndex CuckooFilter{..} = go 0
  where
    go :: Int -> m Bool
    go !x
      | x == numEntries - 1 = pure False
      | otherwise = do
          let idx = bucketIndex + x
          v <- readByteArray storage idx
          if v == emptyMarker then do
            writeByteArray storage idx fp
            pure True
            else go (x + 1)

-- | TODO: Fixme
-- Picks a random 'BucketIndex'.
pickOne :: PrimMonad m
        => Gen (PrimState m)
        -> BucketIndex
        -> BucketIndex
        -> m BucketIndex
pickOne gen i1 i2 = do
  (binaryChoice :: BucketIndex) <- uniform gen
  pure $ case binaryChoice `mod` 2 of
    0 -> i1
    _ -> i2

-- | Inserts a new element inside the filter, failing if the 'CuckooFilter' is empty.
insert :: (PrimMonad m, Hashable32 a)
       => a
       -> CuckooFilter m a
       -> m (Either CuckooFilterError ())
insert a cf@CuckooFilter{..} = do
  let f  = toFingerprint a
  let i1 = fstBucket a cf
  let i2 = sndBucket a cf
  -- Try writing in i1
  inserted1 <- tryInsert f i1 cf
  case inserted1 of
    True -> pure (Right ())
    False -> do
      -- Try writing in i2
      inserted2 <- tryInsert f i2 cf
      case inserted2 of
        True -> pure (Right ())
        False -> do
          randomIndex <- pickOne randomGen i1 i2
          relocate f randomIndex cf

relocate :: PrimMonad m
         => Fingerprint
         -> BucketIndex
         -> CuckooFilter m a
         -> m (Either CuckooFilterError ())
relocate fp idx cf@CuckooFilter{..} = go fp maxNumKicks
  where
    go _ 0 = pure (Left (InsertionFailed MaxKicksExhausted))
    go f !x = do
      -- Randomly select an entry @e@ from bucket[idx]
      eIdx <- uniformR (0, numEntries - 1) randomGen
      f' <- readByteArray storage (idx + eIdx)
      -- Swap f with f'.
      writeByteArray storage (idx + eIdx) f
      let i = toBucket i f' cf
      inserted <- tryInsert f' i cf
      case inserted of
        True  -> pure (Right ())
        False -> go f' (x - 1)

-- | TODO: fixme.
delete :: a -> CuckooFilter s a -> CuckooFilter s a
delete _ cf = cf

-- | TODO: fixme.
lookup :: (PrimMonad m, Hashable32 a)
       => a
       -> CuckooFilter m a
       -> m Bool
lookup a cf@CuckooFilter{..} = do
  let i1 = fstBucket a cf
  let i2 = sndBucket a cf
  v1 <- readByteArray storage i1
  v2 <- readByteArray storage i2
  return (v1 /= emptyMarker || v2 /= emptyMarker)
