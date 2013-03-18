-- Copyright 2012-2013 Samplecount S.L.
-- 
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
-- 
--     http://www.apache.org/licenses/LICENSE-2.0
-- 
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}

module Shakefile.C where

import           Control.Applicative ((<$>))
import           Control.Lens hiding (Action, (<.>))
import           Control.Monad
import           Data.Monoid
import           Development.Shake as Shake
import           Development.Shake.FilePath
import           Data.List (isSuffixOf)
import           Data.Maybe
import           System.Process (readProcess)

{-import Debug.Trace-}

under :: FilePath -> [FilePath] -> [FilePath]
under dir = map prependDir
    where prependDir ""   = dir
          prependDir "."  = dir
          prependDir ".." = takeDirectory dir
          prependDir x    = combine dir x

flag_ :: String -> String -> [String]
flag_ o x = [o, x]

flag :: String -> [String]
flag f = [f]

flags_ :: String -> [String] -> [String]
flags_ o = concat . map (flag_ o)

flags :: String -> [String] -> [String]
flags f = map (f++)

-- Lens utils
append :: forall s t a.
        Monoid a =>
        Setting (->) s t a a -> a -> s -> t
append l n = over l (`mappend` n)

prepend :: forall s t a.
         Monoid a =>
         Setting (->) s t a a -> a -> s -> t
prepend l n = over l (n `mappend`)

-- Shake utils
(?=>) :: FilePath -> (FilePath -> Shake.Action ()) -> Rules ()
f ?=> a = (equalFilePath f) ?> a

systemLoud :: FilePath -> [String] -> Shake.Action ()
systemLoud cmd args = do
    {-putQuiet $ unwords $ [cmd] ++ args-}
    {-system' cmd args-}
    system' cmd args

data Env = Env {
    _buildPrefix :: FilePath
  } deriving (Show)

makeLenses ''Env

defaultEnv :: Env
defaultEnv = Env "."

data CTarget = CTarget {
    _targetPlatform :: String
  , _targetArch :: String
  } deriving (Show)

makeLenses ''CTarget

mkCTarget :: String -> String -> CTarget
mkCTarget = CTarget

buildDir :: Env -> CTarget -> FilePath
{-buildDir env target =-}
      {-(env ^. buildPrefix)-}
  {-</> map toLower (env ^. buildConfiguration)-}
  {-</> (targetString (target ^. buildTarget))-}
  {-</> (target ^. targetArch)-}
buildDir env _ = env ^. buildPrefix

data CLanguage = C | Cpp | ObjC | ObjCpp
                 deriving (Enum, Eq, Show)

data CABI = CABI | CppABI

data LinkResult = Executable
                | SharedLibrary
                | DynamicLibrary
                deriving (Enum, Eq, Show)

defaultCLanguageMap :: [(String, CLanguage)]
defaultCLanguageMap = concatMap f [
    ([".c"], C)
  , ([".cc", ".cpp", ".C"], Cpp)
  , ([".m"], ObjC)
  , ([".mm"], ObjCpp)
  ]
  where f (es, l) = map (flip (,) l) es

languageOf :: FilePath -> Maybe CLanguage
languageOf = flip lookup defaultCLanguageMap . takeExtension

data CBuildFlags = CBuildFlags {
    _systemIncludes :: [FilePath]
  , _userIncludes :: [FilePath]
  , _defines :: [(String, Maybe String)]
  , _preprocessorFlags :: [String]
  , _compilerFlags :: [(Maybe CLanguage, [String])]
  , _libraryPath :: [FilePath]
  , _libraries :: [String]
  , _linkerFlags :: [String]
  , _archiverFlags :: [String]
  } deriving (Show)

makeLenses ''CBuildFlags

type Linker = CTarget -> CToolChain -> CBuildFlags -> [FilePath] -> FilePath -> Shake.Action ()
type Archiver = Linker

data CToolChain = CToolChain {
    _prefix :: Maybe FilePath
  , _compilerCmd :: String
  , _archiverCmd :: String
  , _archiver :: Archiver
  , _archiveFileName :: String -> FilePath
  , _linkerCmd :: String
  , _linker :: LinkResult -> Linker
  -- Not sure whether this should be someplace else
  , _linkResultFileName :: LinkResult -> String -> FilePath
  }

makeLenses ''CToolChain

defaultArchiver :: Archiver
defaultArchiver _ toolChain buildFlags inputs output = do
    need inputs
    systemLoud (tool archiverCmd toolChain)
        $ buildFlags ^. archiverFlags
        ++ [output]
        ++ inputs

defaultArchiveFileName :: String -> FilePath
defaultArchiveFileName = ("lib"++) . (<.> "a")

defaultLinker :: Linker
defaultLinker target toolChain buildFlags inputs output = do
    need inputs
    systemLoud (tool linkerCmd toolChain)
          $  buildFlags ^. linkerFlags
          ++ flag_ "-arch" (target ^. targetArch)
          ++ flags "-L" (buildFlags ^. libraryPath)
          ++ flags "-l" (buildFlags ^. libraries)
          ++ flag_ "-o" output
          ++ inputs

defaultLinkResultFileName :: LinkResult -> String -> FilePath
defaultLinkResultFileName Executable = id
defaultLinkResultFileName SharedLibrary = ("lib"++) . (<.> "so")
defaultLinkResultFileName DynamicLibrary =            (<.> "so")

defaultCToolChain :: CToolChain
defaultCToolChain =
    CToolChain {
        _prefix = Nothing
      , _compilerCmd = "gcc"
      , _archiverCmd = "ar"
      , _archiver = defaultArchiver
      , _archiveFileName = defaultArchiveFileName
      , _linkerCmd = "gcc"
      , _linker = \link target toolChain ->
            case link of
                Executable -> defaultLinker target toolChain
                _          -> defaultLinker target toolChain . append linkerFlags (flag "-shared")
      , _linkResultFileName = defaultLinkResultFileName
      }

tool :: (Getter CToolChain String) -> CToolChain -> FilePath
tool f toolChain = maybe cmd (flip combine ("bin" </> cmd))
                         (toolChain ^. prefix)
    where cmd = toolChain ^. f



defaultCBuildFlags :: CBuildFlags
defaultCBuildFlags =
    CBuildFlags {
        _systemIncludes = []
      , _userIncludes = []
      , _defines = []
      , _preprocessorFlags = []
      , _compilerFlags = []
      , _libraryPath = []
      , _libraries = []
      , _linkerFlags = []
      , _archiverFlags = []
      }

defineFlags :: CBuildFlags -> [String]
defineFlags = flags "-D" . map (\(a, b) -> maybe a (\b' -> a++"="++b') b) . flip (^.) defines

compilerFlagsFor :: Maybe CLanguage -> CBuildFlags -> [String]
compilerFlagsFor lang = concat
                      . maybe (map snd . filter (isNothing.fst))
                              (mapMaybe . f) lang
                      . flip (^.) compilerFlags
    where f _ (Nothing, x) = Just x
          f l (Just l', x) | l == l' = Just x
                           | otherwise = Nothing

type CBuildEnv = (CToolChain, CBuildFlags)


sed :: String -> FilePath -> FilePath -> Shake.Action ()
sed command input output = do
    need [input]
    (stdout, _) <- systemOutput "sed" ["-e", command, input]
    writeFile' output stdout

sourceTransform :: (FilePath -> FilePath) -> String -> FilePath -> Rules FilePath
sourceTransform f cmd input = do
    let output = f input
    output ?=> sed cmd input
    want [output]
    return output

dependencyFile :: CTarget -> CToolChain -> CBuildFlags -> FilePath -> FilePath -> Rules ()
dependencyFile target toolChain buildFlags input output = do
    output ?=> \_ -> do
        need [input]
        systemLoud (tool compilerCmd toolChain)
                $  flag_ "-arch" (target ^. targetArch)
                ++ flags "-I" (buildFlags ^. systemIncludes)
                ++ flags_ "-iquote" (buildFlags ^. userIncludes)
                ++ (defineFlags buildFlags)
                ++ (buildFlags ^. preprocessorFlags)
                ++ (compilerFlagsFor (languageOf input) buildFlags)
                ++ ["-MM", "-o", output, input]

parseDependencies :: String -> [FilePath]
parseDependencies = drop 2 . words . filter (/= '\\')

type ObjectRule = CTarget -> CToolChain -> CBuildFlags -> FilePath -> [FilePath] -> FilePath -> Rules ()

staticObject :: ObjectRule
staticObject target toolChain buildFlags input deps output = do
    let depFile = output <.> "d"
    dependencyFile target toolChain buildFlags input depFile
    output ?=> \_ ->  do
        deps' <- parseDependencies <$> readFile' depFile
        need $ [input] ++ deps ++ deps'
        systemLoud (tool compilerCmd toolChain)
                $  flag_ "-arch" (target ^. targetArch)
                ++ flags "-I" (buildFlags ^. systemIncludes)
                ++ flags_ "-iquote" (buildFlags ^. userIncludes)
                ++ (defineFlags buildFlags)
                ++ (buildFlags ^. preprocessorFlags)
                ++ (compilerFlagsFor (languageOf input) buildFlags)
                ++ ["-c", "-o", output, input]

sharedObject :: ObjectRule
sharedObject target toolChain = staticObject target toolChain . append compilerFlags [(Nothing, flag "-fPIC")]

data SourceTree b = SourceTree (b -> b) [(FilePath, [FilePath])]

sourceTree :: (b -> b) -> [(FilePath, [FilePath])] -> SourceTree b
sourceTree = SourceTree

sourceFiles :: [FilePath] -> [(FilePath, [FilePath])]
sourceFiles = map (flip (,) [])

data Library = Library {
    libName :: String
  , libSources :: [SourceTree CBuildFlags]
  }

libBuildDir :: Env -> CTarget -> FilePath -> FilePath
libBuildDir env target libFileName = buildDir env target </> map tr (libFileName)
    where tr '.' = '_'
          tr x   = x

libBuildPath :: Env -> CTarget -> FilePath -> FilePath
libBuildPath env target libFileName = buildDir env target </> libFileName

cLibrary :: ObjectRule -> Linker -> (Library -> FilePath)
         -> Env -> CTarget -> CToolChain -> CBuildFlags
         -> Library
         -> Rules FilePath
cLibrary object link libFileName env target toolChain buildFlags lib = do
    let libFile = libFileName lib
        libPath = libBuildPath env target libFile
        buildDir = libBuildDir env target libFile
    objects <- forM (libSources lib) $ \(SourceTree mapBuildFlags srcTree) -> do
        let src = map fst srcTree
            dep = map snd srcTree
            obj = map (combine buildDir . makeRelative buildDir . (<.> "o")) src
        zipWithM_ ($) (zipWith (object target toolChain (mapBuildFlags buildFlags)) src dep) obj
        return obj
    libPath ?=> link target toolChain buildFlags (concat objects)
    return libPath

-- | Rule for building a static library.
staticLibrary :: Env -> CTarget -> CToolChain -> CBuildFlags -> Library -> Rules FilePath
staticLibrary env target toolChain =
    cLibrary
        staticObject
        (toolChain ^. archiver)
        ((toolChain ^. archiveFileName) . libName)
        env target toolChain

-- | Rule for building a shared library.
sharedLibrary :: Env -> CTarget -> CToolChain -> CBuildFlags -> Library -> Rules FilePath
sharedLibrary env target toolChain =
    cLibrary
        sharedObject
        ((toolChain ^. linker) linkResult)
        ((toolChain ^. linkResultFileName) linkResult . libName)
        env target toolChain
    where linkResult = SharedLibrary

-- | Rule for building a dynamic library.
dynamicLibrary :: Env -> CTarget -> CToolChain -> CBuildFlags -> Library -> Rules FilePath
dynamicLibrary env target toolChain =
    cLibrary
        sharedObject
        ((toolChain ^. linker) linkResult)
        ((toolChain ^. linkResultFileName) linkResult . libName)
        env target toolChain
    where linkResult = DynamicLibrary

-- ====================================================================
-- Configurations

type Configuration = (String, CBuildFlags -> CBuildFlags)

applyConfiguration :: String -> [Configuration] -> CBuildFlags -> CBuildFlags
applyConfiguration c cs =
    case lookup c cs of
        Nothing -> id
        Just f  -> f

-- ====================================================================
-- PkgConfig

pkgConfig :: String -> IO (CBuildFlags -> CBuildFlags)
pkgConfig pkg = do
    cflags <- parseFlags <$> readProcess "pkg-config" ["--cflags", pkg] ""
    lflags <- parseFlags <$> readProcess "pkg-config" ["--libs", pkg] ""
    return $ append compilerFlags [(Nothing, cflags)] . append linkerFlags lflags
    where
        parseFlags = map (dropSuffix "\\") . words . head . lines
        dropSuffix s x = if s `isSuffixOf` x
                         then reverse (drop (length s) (reverse x))
                         else x

