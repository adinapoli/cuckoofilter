{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.CuckooFilter.Fingerprint (
    Fingerprint
  , ToFingerprint(..)
  , toWord32
  , emptyMarker
  , module HashImplementation
  ) where

import Data.Digest.Murmur32 as HashImplementation
import Data.Primitive.Types
import Data.Word

-- We choose 8 as the fingerprint size. This value is somewhat
-- abitrary but hopefully sufficient. In the original paper
-- (Section 4, see "Minimum Fingerprint size") it's written
-- that the number of bits required for `f` increases as the
-- size of the Cuckoo Filter expands. They give the formula:
--
-- f = Ω(log n/b) bits
--
-- Where `n` is the size of input and `b` is the number of
-- buckets. Using 8 bits for `f` and setting `b = 4` would
-- allow us to comfortably store 1 billion elements with enough
-- collision resistance (assuming a base-2 log).
newtype Fingerprint = FP Word8 deriving (Show, Eq)

instance Prim Fingerprint where
  sizeOf# (FP w) = sizeOf# w
  alignment# (FP w) = alignment# w
  indexByteArray# a b = FP (indexByteArray# a b)
  readByteArray# a b c = case readByteArray# a b c of
    (# s, w8 #) -> (# s, FP w8 #)
  writeByteArray# a b (FP w) = writeByteArray# a b w
  setByteArray# a b c (FP w) = setByteArray# a b c w
  indexOffAddr# a b = FP (indexOffAddr# a b)
  readOffAddr# a b c = case readOffAddr# a b c of
    (# s, w8 #) -> (# s, FP w8 #)
  writeOffAddr# a b (FP w) = writeOffAddr# a b w
  setOffAddr# a b c (FP w) = setOffAddr# a b c w

-- | `ToFingerprint` represent the class of types which can be
-- turned into a `Fingerprint`.
-- The paper defines a `Fingerprint` as being "a bit string derived
-- from the item using a (non-cryptographic) hash function".
-- The hash function of choice here is `MurmurHash2` and is once again
-- an arbitrary choice.
class ToFingerprint a where
  toFingerprint :: a -> Fingerprint

instance Hashable32 a => ToFingerprint a where
  toFingerprint x = case fromIntegral $ asWord32 (hash32 x) `mod` (fromIntegral (maxBound :: Word8)) of
    0x0 -> FP 0x1
    w8  -> FP w8

emptyMarker :: Word8
emptyMarker = 0

toWord32 :: Hashable32 a => a -> Word32
toWord32 = asWord32 . hash32
