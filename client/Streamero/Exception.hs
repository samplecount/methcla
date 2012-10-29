{-# LANGUAGE DeriveDataTypeable #-}
module Streamero.Exception (
  Exception(..)
) where

import qualified Control.Exception as E
import           Data.Aeson (ToJSON(..), (.=), object)
import           Data.Typeable (Typeable)
import qualified Data.Text as T

data Exception =
    FileError FilePath String
  | FileConversionError FilePath FilePath
    deriving (Show, Typeable)

instance E.Exception Exception

instance ToJSON Exception where
  toJSON a =
    case a of
      FileError path message ->
        object [ t "FileError"
               , T.pack "path" .= path
               , T.pack "message" .= message ]
      FileConversionError input output ->
        object [ T.pack "type" .= "FileConversionError"
               , T.pack "input" .= input
               , T.pack "output" .= output ]
    where t s = T.pack "type" .= s
