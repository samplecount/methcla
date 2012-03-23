module Sound.LV2.Atom.Util where

import Data.Bits
import Data.Word

{-# INLINE alignment #-}
alignment :: Word32
alignment = 8

{-# INLINE pad #-}
pad :: Word32 -> Word32
pad size = (size + mask) .&. (complement mask)
    where mask = alignment - 1

{-# INLINE padding #-}
padding :: Word32 -> Word32
padding size = pad size - size

headerSize :: Word32
headerSize = 8
