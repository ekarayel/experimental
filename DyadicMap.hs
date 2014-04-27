{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module DyadicMap where

import Data.Bits
import Prelude hiding (lookup)

newtype Height a = Height Int

class DyadicMap m b where
    height :: Height (m b)
    create :: (Bits t) => (t -> b) -> m b
    lookup :: (Bits t) => t -> m b -> b

newtype Leave b = Leave b

instance DyadicMap Leave b where
    height = Height 0
    create f = Leave (f $ clearBit (bit 0) 0)
    lookup _ (Leave v) = v

data Node m b = Node (m b) (m b)

-- | Map of size 2^8
type Map8 v = Node (Node (Node (Node 
             (Node (Node (Node (Node Leave))))))) v

instance (DyadicMap m b) => DyadicMap (Node m) b where
    height = Height $ 1 + h
      where Height h = height :: Height (m b)
    create f = Node l r
      where Height h = height :: Height (m b)
            l = create f
            r = create (f . (.|. (bit h)))
    lookup k (Node l r)
      | testBit k h = lookup k r
      | otherwise = lookup k l
      where Height h = height :: Height (m b)

