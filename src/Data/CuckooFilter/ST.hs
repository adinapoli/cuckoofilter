{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
module Data.CuckooFilter.ST (
    new
  , lookup
  ) where

import qualified Data.CuckooFilter as Prim
import Prelude hiding (lookup)
import Data.CuckooFilter (CuckooFilter)
import Data.CuckooFilter.Fingerprint
import Control.Monad.ST

new :: ST s (CuckooFilter (ST s) a)
new = Prim.new

lookup :: (Hashable32 a)
       => a
       -> (forall s. CuckooFilter (ST s) a)
       -> Bool
lookup a cf = runST $ Prim.lookup a cf
