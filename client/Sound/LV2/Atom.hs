module Sound.LV2.Atom where

data Put a = Put a
data Get a = Get a

class Get a where
    get :: Get a

