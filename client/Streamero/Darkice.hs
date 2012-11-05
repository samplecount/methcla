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
  , server :: String
  , port :: Int
  , password :: String
  , mountPoint :: String
  , bufferSize :: Double
  , name :: Text
  , description :: Text
  , url :: Text
  , genre :: Text
  , public :: Bool
  } deriving (Show)

defaultConfig :: Config
defaultConfig = Config {
    jackClientName = Nothing
  , server = "127.0.0.1"
  , port = 8000
  , password = "hackme"
  , mountPoint = "default"
  , bufferSize = 1
  , name = ""
  , description = ""
  , url = ""
  , genre = ""
  , public = False
  }

configText :: Config -> Text
configText config = Text.unlines [
    "[general]"
  , "duration       = 0"
  , "bufferSecs     = " `aps` show (bufferSize config)

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
  , "bitrate         = 64"
  , "quality         = 0.8"
  , "server          = " `aps` server config
  , "port            = " `aps` show (port config)
  , "password        = " `aps` password config
  , "mountPoint      = " `aps` mountPoint config
  , "name            = " `Text.append` name config
  , "description     = " `Text.append` description config
  , "url             = " `Text.append` url config
  , "genre           = " `Text.append` genre config
  , "public          = " `Text.append` Text.pack (if public config then "yes" else "no")
  -- , "remoteDumpFile  = /tmp/server-dump.mp3"
  -- , "localDumpFile   = /tmp/encoder-dump.mp3"
  -- , "fileAddDate     = no"
  ]
  where a `aps` b = a `Text.append` Text.pack b

