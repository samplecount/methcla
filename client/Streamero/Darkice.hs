{-# LANGUAGE OverloadedStrings #-}
module Streamero.Darkice (
    Options(..)
  , defaultOptions
  , optionList
  , createProcess
  , Config(..)
  , defaultConfig
  , configText
) where

import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Filesystem.Path.CurrentOS as FS
import qualified Streamero.Process as P

data Options = Options {
    configFile :: Maybe FS.FilePath
  , verbosity :: Int
  } deriving (Show)

defaultOptions :: Options
defaultOptions = Options Nothing 0

optionList :: Options -> [String]
optionList opts = concat [ maybe [] (\c -> ["-c", FS.encodeString c]) (configFile opts)
                         , [ "-v", show (verbosity opts) ] ]

createProcess :: FS.FilePath -> Options -> P.CreateProcess
createProcess program = P.proc (FS.encodeString program) . optionList

data Config = Config {
    jackClientName :: Maybe String
  , mountPoint :: String
  } deriving (Show)

defaultConfig :: Config
defaultConfig = Config Nothing "default"

configText :: Config -> Text
configText config = Text.unlines [
    "[general]"
  , "duration       = 0"
  , "bufferSecs     = 1"

  , "[input]"
  , "device         = jack"
  , maybe
    "# jackClientName = "
    (aps "jackClientName = ")
    (jackClientName config)
  , "sampleRate     = 44100"
  , "bitsPerSample  = 16"
  , "channel        = 2"

  , "[icecast2-0]"
  , "format          = mp3"
  , "bitrateMode     = cbr"
  , "bitrate         = 128"
  , "quality         = 0.8"
  , "server          = 127.0.0.1"
  , "port            = 8000"
  , "password        = hackme"
  , "mountPoint      = " `aps` mountPoint config
  , "name            = Streamero"
  , "description     = Streamero soundscape so und so"
  , "url             = http://www.puesnada.es"
  , "genre           = soundscape"
  , "public          = no"
  -- , "remoteDumpFile  = /tmp/server-dump.mp3"
  -- , "localDumpFile   = /tmp/encoder-dump.mp3"
  -- , "fileAddDate     = no"
  ]
  where a `aps` b = a `Text.append` Text.pack b

