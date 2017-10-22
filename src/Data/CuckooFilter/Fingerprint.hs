{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.CuckooFilter.Fingerprint (
    Fingerprint
  , ToFingerprint(..)
  , toHash32
  , module HashImplementation
  ) where

import Data.Digest.Murmur32 as HashImplementation
import qualified Data.ByteString as B
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

-- | `ToFingerprint` represent the class of types which can be
-- turned into a `Fingerprint`.
-- The paper defines a `Fingerprint` as being "a bit string derived
-- from the item using a (non-cryptographic) hash function".
-- The hash function of choice here is `MurmurHash2` and is once again
-- an arbitrary choice.
class ToFingerprint a where
  toFingerprint :: a -> Fingerprint

toHash32 :: Fingerprint -> Hash32
toHash32 (FP a) = hash32 $ (B.pack [a])
