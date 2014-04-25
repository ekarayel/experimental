{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module DyadicMap where

import Data.Word
import Data.Bits
import Prelude hiding (lookup)

newtype Height a = Height Int

class DyadicMap m b where
    height :: Height (m b)
    create :: (Word8 -> b) -> m b
    lookup :: Word8 -> m b -> b

newtype Leave b = Leave b

instance DyadicMap Leave b where
    height = Height 0
    create f = Leave (f 0)
    lookup k (Leave v) = v

data Node m b = Node (m b) (m b)

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

