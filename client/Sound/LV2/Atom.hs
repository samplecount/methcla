module Sound.LV2.Atom (
    ToAtom(..)
  , FromAtom(..)
  , encode
  , decode
) where

import Sound.LV2.Atom.Get (FromAtom(..), decode)
import Sound.LV2.Atom.Put (ToAtom(..), encode)
