{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.CuckooFilter.Fingerprint (
    Fingerprint
  , ToFingerprint(..)
  , toHash32
  , emptyMarker
  , toStorable
  , module HashImplementation
  ) where

import Data.Digest.Murmur32 as HashImplementation
import qualified Data.ByteString as B
import Data.Word
import Data.Primitive.Types

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

newtype StorableFingerprint = SFP Word8 deriving (Show, Eq)

instance Prim StorableFingerprint where
  sizeOf# (SFP w) = sizeOf# w
  alignment# (SFP w) = alignment# w
  indexByteArray# a b = SFP (indexByteArray# a b)
  readByteArray# a b c = case readByteArray# a b c of
    (# s, w8 #) -> (# s, SFP w8 #)
  writeByteArray# a b (SFP w) s = writeByteArray# a b w s
  setByteArray# a b c (SFP w) s = setByteArray# a b c w s
  indexOffAddr# a b = SFP (indexOffAddr# a b)
  readOffAddr# a b c = case readOffAddr# a b c of
    (# s, w8 #) -> (# s, SFP w8 #)
  writeOffAddr# a b (SFP w) s = writeOffAddr# a b w s
  setOffAddr# a b c (SFP w) d = setOffAddr# a b c w d

-- | `ToFingerprint` represent the class of types which can be
-- turned into a `Fingerprint`.
-- The paper defines a `Fingerprint` as being "a bit string derived
-- from the item using a (non-cryptographic) hash function".
-- The hash function of choice here is `MurmurHash2` and is once again
-- an arbitrary choice.
class ToFingerprint a where
  toFingerprint :: a -> Fingerprint

instance Hashable32 a => ToFingerprint a where
  toFingerprint x = FP $ fromIntegral $ asWord32 (hash32 x) `mod` (fromIntegral (maxBound :: Word8))

emptyMarker :: Word8
emptyMarker = 0

toStorable :: Fingerprint -> StorableFingerprint
toStorable (FP 0x0) = SFP 0x1
toStorable (FP w8)  = SFP w8

toHash32 :: Fingerprint -> Hash32
toHash32 (FP a) = hash32 $ (B.pack [a])
