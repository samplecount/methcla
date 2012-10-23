{-# LANGUAGE FlexibleInstances
           , TypeSynonymInstances #-}
module Streamero.SC3 (
  toStereo
, ControlName(..)
, ToControlValue(..)
, control
) where

import           Sound.SC3 hiding (control)
import           Sound.SC3.Lang.Collection (clump)
import qualified Sound.SC3.Server.Monad.Command as SC

toStereo :: UGen -> UGen
toStereo u =
    case mceChannels u of
        [x]    -> pan2 x pan amp
        [x, y] -> balance2 x y pan amp
        xs     -> sum (map (\[x, y] -> balance2 x y pan amp) $ clump 2 xs)
    where
        pan = 0 -- control KR "pan" 0
        amp = 1 -- control KR "amp" 1

class ControlName a where
    controlName :: a -> String

instance ControlName String where
    controlName = id

class ToControlValue a where
    toControlValue :: a -> Double

instance ToControlValue Double where
    toControlValue = id

instance ToControlValue Float where
    toControlValue = realToFrac

instance ToControlValue Int where
    toControlValue = fromIntegral

instance ToControlValue Bool where
    toControlValue True = 1
    toControlValue False = 0

instance ToControlValue SC.AudioBus where
    toControlValue = fromIntegral . SC.busId

instance ToControlValue SC.ControlBus where
    toControlValue = fromIntegral . SC.busId

instance ToControlValue SC.Buffer where
    toControlValue = fromIntegral . SC.bufferId

control :: (ToControlValue a) => String -> a -> (String, Double)
control s a = (s, toControlValue a)
