module Sound.LV2.Atom.Util where

import Data.Bits
import Data.Word

{-# INLINE alignment #-}
alignment :: Word32
alignment = 8

{-# INLINE padding #-}
padding :: Word32 -> Word32
padding size = (size + mask) .&. (complement mask) - size
    where mask = alignment - 1

headerSize :: Word32
headerSize = 8
