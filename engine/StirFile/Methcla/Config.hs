{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}

-- | A module for parsing and using config files in a Shake build system. Config files
--   consist of variable bindings, for example:
--
-- > # This is my Config file
-- > HEADERS_DIR = /path/to/dir
-- > CFLAGS = -g -I${HEADERS_DIR}
-- > CFLAGS = $CFLAGS -O2
-- > include extra/file.cfg
--
--   This defines the variable @HEADERS_DIR@ (equal to @\/path\/to\/dir@), and
--   @CFLAGS@ (equal to @-g -I\/path\/to\/dir -O2@), and also includes the configuration
--   statements in the file @extra/file.cfg@. The full lexical syntax for configuration
--   files is defined here: <http://martine.github.io/ninja/manual.html#_lexical_syntax>.
--
--   To use the configuration file either use 'readConfigFile' to parse the configuration file
--   and use the values directly, or 'usingConfigFile' and 'getConfig' to track the configuration
--   values, so they become build dependencies.
module Methcla.Config(
    -- useConfigFile
  withConfig
) where

import Control.Applicative
import Control.Exception
import qualified Data.HashMap.Strict as Map
import Development.Shake
import Development.Shake.Classes
import Development.Shake.Config (readConfigFile)
import System.Directory

-- newtype Config a = Config String deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
--
-- configKey :: a -> String -> Config a
-- configKey _ = Config
--
-- -- | Specify the file to use with 'getConfig'.
-- useConfigFile :: Typeable a => a -> FilePath -> FilePath -> [FilePath] -> Rules (String -> Action (Maybe String))
-- useConfigFile a baseDir file deps = do
--   mp <- newCache $ \() -> do
--     need deps
--     liftIO $ do
--       oldwd <- getCurrentDirectory
--       setCurrentDirectory baseDir
--       readConfigFile file
--         `finally` setCurrentDirectory oldwd
--   f <- addOracle $ \(Config x :: Config a) -> Map.lookup x <$> mp ()
--   return $ \k -> f (configKey a k)

newtype Config = Config (FilePath, String) deriving (Show,Typeable,Eq,Hashable,Binary,NFData)

withConfig :: FilePath -> [FilePath] -> Rules (FilePath -> String -> Action (Maybe String))
withConfig baseDir deps = do
  fileCache <- newCache $ \file -> do
    need deps
    liftIO $ do
      oldwd <- getCurrentDirectory
      setCurrentDirectory baseDir
      readConfigFile file
        `finally` setCurrentDirectory oldwd
  query <- addOracle $ \(Config (file, key)) -> Map.lookup key <$> fileCache file
  return $ \file key -> query (Config (file, key))
