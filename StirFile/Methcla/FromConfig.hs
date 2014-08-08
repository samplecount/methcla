module Methcla.FromConfig (
  buildFlags
) where

import Control.Applicative
-- import Control.Arrow
import Data.Char (toLower)
-- import Data.List (intercalate)
-- import Data.List.Split (splitOn)
-- import qualified Data.HashMap.Strict as Map
import Development.Shake
-- import qualified Development.Shake.Config as Config
import Shakefile.C.BuildFlags as BuildFlags
import Shakefile.C.Util
-- import Shakefile.C.Language
import Shakefile.C.PkgConfig as PkgConfig
-- import Shakefile.Label

-- getConfig :: (String -> Action (Maybe String)) -> String -> Action (Maybe ([(String,String)], String))
-- getConfig f = fmap (fmap parse) . f
--   where
--     parse "" = ([],"")
--     parse s@('[':xs) = case splitOn "] " xs of
--                         (prefix:rest) -> (map (pair . splitOn "=") . splitOn " " $ prefix, intercalate "] " rest)
--                         _ -> ([], s)
--     parse xs = ([], xs)
--     pair [k,v] = (k,v)
--     pair _ = error "Invalid key value pair"
--
-- -- getConfigWithEnv :: (String -> Action (Maybe String)) -> Map.HashMap String String -> String -> Action (Maybe String)
-- -- getConfigWithEnv parse f env key = do
-- --   cfg <- getConfig f key
-- --   return $
-- --     case cfg of
-- --       Nothing -> return id
-- --       Just ((k,v),s) ->
-- --         case Map.lookup k env of
-- --           Nothing -> parse s
-- --           Just v' -> if v == v'
-- --                      then parse s
-- --                      else id
-- --
-- --
-- --   return $ do
-- --     ((k,v), str) <- input
-- --     v' <- Map.lookup k
-- --     fmap (maybe [] parser) . getConfig . ("BuildFlags."++)
--
-- filterEnv :: Map.HashMap String String -> [Maybe ([(String,String)],a)] -> [a]
-- filterEnv env xs =
--   let isMatch = all $ \(k,v) ->
--                   case Map.lookup k env of
--                     Nothing -> True
--                     Just v' -> v == v'
--   in   map snd
--      $ filter (isMatch . fst)
--      $ [ x | Just x <- xs ]
--
-- buildFlags :: (String -> Action (Maybe String)) -> Map.HashMap String String -> Action (BuildFlags -> BuildFlags)
-- buildFlags cfg env = do
--   let fields = [
--           ( "systemIncludes",       append systemIncludes    . paths                             )
--         , ( "userIncludes",         append userIncludes      . paths                             )
--         , ( "defines",              append defines           . defines'                          )
--         , ( "preprocessorFlags",    append preprocessorFlags . flags                             )
--         , ( "compilerFlags",        append compilerFlags     . ((:[]) . ((,)Nothing) . flags)    )
--         , ( "compilerFlags.c",      append compilerFlags     . ((:[]) . ((,)(Just C)) . flags)   )
--         , ( "compilerFlags.cxx",    append compilerFlags     . ((:[]) . ((,)(Just Cpp)) . flags) )
--         , ( "libraryPath",          append libraryPath       . paths                             )
--         , ( "libraries",            append libraries         . flags                             )
--         , ( "linkerFlags",          append linkerFlags       . flags                             )
--         , ( "localLibraries",       append localLibraries    . paths                             )
--         , ( "archiverFlags",        append archiverFlags     . flags                             )
--         ]
--   fs <- mapM (\(k, f) -> fmap (second f) <$> getConfig cfg ("BuildFlags." ++ k))
--              fields :: Action [Maybe ([(String,String)],BuildFlags -> BuildFlags)]
--   -- return id
--   return $ foldl (.) id
--          $ filterEnv env fs
--   where
--     flags = words'
--     paths = words'
--     define [] = error "Empty preprocessor definition"
--     define [k] = (k, Nothing)
--     define [k,v] = (k, Just v)
--     define (k:vs) = (k, Just (intercalate "=" vs))
--     defines' = map (define . splitOn "=") . flags
--
-- sources :: (String -> Action (Maybe String)) -> Map.HashMap String String -> Action [FilePath]
-- sources cfg env = do
--   deps <- fmap (second words') <$> getConfig cfg "Sources.deps"
--   need $ filterEnv env [deps]
--   sources <- fmap (second words') <$> getConfig cfg "Sources"
--   return $ filterEnv env [sources]

buildFlags :: (String -> Action (Maybe String)) -> Action (BuildFlags -> BuildFlags)
buildFlags cfg = do
  flags <- BuildFlags.fromConfig cfg
  pkgConfig_searchPath <- fmap words' <$> cfg "PkgConfig.options.searchPath"
  pkgConfig_static <- fmap (bool . words) <$> cfg "PkgConfig.options.static"
  pkgConfig_packages <- fmap words <$> cfg "PkgConfig.packages"
  let pkgConfig_options = PkgConfig.defaultOptions {
      PkgConfig.searchPath = pkgConfig_searchPath,
      PkgConfig.static = maybe False id pkgConfig_static
    }
  pkgConfig_flags <- mapM (pkgConfigWithOptions pkgConfig_options)
                          (maybe [] id pkgConfig_packages)
  return $ foldl (.) id $ pkgConfig_flags ++ [flags]
  where
    bool (x:_) = map toLower x == "true"
    bool _ = False
