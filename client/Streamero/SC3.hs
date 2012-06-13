module Streamero.SC3 (
    toStereo
) where

import Sound.SC3
import Sound.SC3.Lang.Collection (clump)

toStereo :: UGen -> UGen
toStereo u =
    case mceChannels u of
        [x]    -> pan2 x pan amp
        [x, y] -> balance2 x y pan amp
        xs     -> sum (map (\[x, y] -> balance2 x y pan amp) $ clump 2 xs)
    where
        pan = 0 -- control KR "pan" 0
        amp = 1 -- control KR "amp" 1
