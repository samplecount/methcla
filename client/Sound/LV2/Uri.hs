module Sound.LV2.Uri (
    Uri
  , Urid
  , Map(..)
) where

import Data.Word

type Uri = String
type Urid = Word32

data Map m = Map {
    map :: Uri -> m Urid
  , unmap :: Urid -> m (Maybe Uri)
  }
