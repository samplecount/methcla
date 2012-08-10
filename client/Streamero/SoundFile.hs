module Streamero.SoundFile (
    SF.Info(..)
  , getInfo
  , isSupported
  , toFLAC
  , toPCM
) where

import qualified Control.Exception as E
import qualified Sound.File.Sndfile as SF
import           System.Exit (ExitCode(..))
import           System.Process (rawSystem)

getInfo :: FilePath -> IO SF.Info
getInfo path = do
    h <- SF.openFile path SF.ReadMode SF.defaultInfo
    SF.hClose h
    return (SF.hInfo h)

isSupported :: FilePath -> IO Bool
isSupported path =
    either (const False) (const True)
        `fmap` (E.try (getInfo path) :: IO (Either E.SomeException SF.Info))

toFLAC :: FilePath -> FilePath -> IO ()
toFLAC input output = do
    e <- rawSystem "ffmpeg" [ "-y", "-i", input, "-acodec", "flac", output ]
    case e of
        ExitSuccess -> return ()
        _ -> E.throw e

toPCM :: FilePath -> FilePath -> IO ()
toPCM input output = do
    e <- rawSystem "ffmpeg" [ "-y", "-i", input, output ]
    case e of
        ExitSuccess -> return ()
        _ -> E.throw e
