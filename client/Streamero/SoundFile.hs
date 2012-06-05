module Streamero.SoundFile (
    getInfo
  , SF.Info(..)
) where

import Sound.File.Sndfile as SF

getInfo :: FilePath -> IO SF.Info
getInfo path = do
    h <- SF.openFile path SF.ReadMode SF.defaultInfo
    SF.hClose h
    return (SF.hInfo h)
