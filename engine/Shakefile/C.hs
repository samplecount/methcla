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

module Shakefile.C (
    under
  , mapFlag
  , concatMapFlag
  , (?=>)
  , Env
  , defaultEnv
  , buildPrefix
  , Platform(..)
  , platformString
  , Arch(..)
  , archShortString
  , archString
  , ArmVersion(..)
  , X86Version(..)
  , Target
  , mkTarget
  , targetArch
  , targetVendor
  , targetOS
  , targetPlatform
  , targetString
  , Language(..)
  , Linkage(..)
  , LinkResult(..)
  , BuildFlags
  , defaultBuildFlags
  , systemIncludes
  , userIncludes
  , defines
  , preprocessorFlags
  , compilerFlags
  , libraryPath
  , libraries
  , linkerFlags
  , staticLibraries
  , archiverFlags
  , ToolChain
  , defaultToolChain
  , toolChainFromEnvironment
  , prefix
  , compilerCmd
  , archiverCmd
  , archiver
  , archiveFileName
  , linkerCmd
  , linker
  , linkResultFileName
  , tool
  , Archiver
  , defaultArchiver
  , Linker
  , defaultLinker
  , executable
  , staticLibrary
  , sharedLibrary
  , dynamicLibrary
) where

import           Control.Applicative ((<$>))
import           Control.Lens hiding (Action, (<.>), under)
import           Control.Monad
import           Data.Char (toLower)
import           Development.Shake as Shake
import           Development.Shake.FilePath
import           Data.Maybe
import           Data.Version
import           Shakefile.Lens (append)
import           Shakefile.SourceTree (SourceTree, applySourceTree)
import           System.Environment (getEnvironment)
import           System.FilePath (makeRelative, takeFileName)

{-import Debug.Trace-}

under :: FilePath -> [FilePath] -> [FilePath]
under dir = map prependDir
    where prependDir ""   = dir
          prependDir "."  = dir
          prependDir ".." = takeDirectory dir
          prependDir x    = combine dir x

mapFlag :: String -> [String] -> [String]
mapFlag f = concatMap (\x -> [f, x])

concatMapFlag :: String -> [String] -> [String]
concatMapFlag f = map (f++)

-- Shake utils
(?=>) :: FilePath -> (FilePath -> Shake.Action ()) -> Rules ()
f ?=> a = (equalFilePath f) ?> a

data Env = Env {
    _buildPrefix :: FilePath
  } deriving (Show)

makeLenses ''Env

defaultEnv :: Env
defaultEnv = Env "."

data Platform = Platform {
    platformName :: String
  , platformVersion :: Version
  } deriving (Eq, Show)

platformString :: Platform -> String
platformString = map toLower . platformName

data X86Version =
    I386
  | I686
  | X86_64
  deriving (Eq, Show)

data ArmVersion =
    Armv5
  | Armv6
  | Armv7
  | Armv7s
  deriving (Eq, Show)

data Arch =
    X86 X86Version
  | Arm ArmVersion
  deriving (Eq, Show)

archShortString :: Arch -> String
archShortString arch =
  case arch of
    X86 _ -> "x86"
    Arm _ -> "arm"

archString :: Arch -> String
archString arch =
  case arch of
    X86 I386 -> "i386"
    X86 I686 -> "i686"
    X86 X86_64 -> "x86_64"
    Arm Armv5 -> "armv5"
    Arm Armv6 -> "armv6"
    Arm Armv7 -> "armv7"
    Arm Armv7s -> "armv7s"

data Target = Target {
    _targetArch :: Arch
  , _targetVendor :: String
  , _targetOS :: String
  , _targetPlatform :: Platform
  } deriving (Show)

makeLenses ''Target

mkTarget :: Arch -> String -> String -> Platform -> Target
mkTarget = Target

targetString :: Target -> String
targetString target =
     archShortString (target ^. targetArch)
  ++ "-" ++ (target ^. targetVendor)
  ++ "-" ++ (target ^. targetOS)

buildDir :: Env -> Target -> FilePath
{-buildDir env target =-}
      {-(env ^. buildPrefix)-}
  {-</> map toLower (env ^. buildConfiguration)-}
  {-</> (targetString (target ^. buildTarget))-}
  {-</> (target ^. targetArch)-}
buildDir env _ = env ^. buildPrefix

data Language = C | Cpp | ObjC | ObjCpp
                 deriving (Enum, Eq, Show)

data Linkage = Static | Shared deriving (Enum, Eq, Show)

data LinkResult = Executable
                | SharedLibrary
                | DynamicLibrary
                deriving (Enum, Eq, Show)

defaultLanguageMap :: [(String, Language)]
defaultLanguageMap = concatMap f [
    (C, [".c"])
  , (Cpp, [".cc", ".CC", ".cpp", ".CPP", ".C", ".cxx", ".CXX"])
  , (ObjC, [".m"])
  , (ObjCpp, [".mm", ".M"])
  ]
  where f (lang, exts) = map (\ext -> (ext, lang)) exts

languageOf :: FilePath -> Maybe Language
languageOf = flip lookup defaultLanguageMap . takeExtension

data BuildFlags = BuildFlags {
    _systemIncludes :: [FilePath]
  , _userIncludes :: [FilePath]
  , _defines :: [(String, Maybe String)]
  , _preprocessorFlags :: [String]
  , _compilerFlags :: [(Maybe Language, [String])]
  , _libraryPath :: [FilePath]
  , _libraries :: [String]
  , _linkerFlags :: [String]
  -- This is needed for linking against local libraries built by shake (the linker `needs' its inputs).
  -- A better name maybe?
  , _staticLibraries :: [FilePath]
  , _archiverFlags :: [String]
  } deriving (Show)

makeLenses ''BuildFlags

type Linker = ToolChain -> BuildFlags -> [FilePath] -> FilePath -> Shake.Action ()
type Archiver = Linker

data ToolChain = ToolChain {
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

makeLenses ''ToolChain

defaultArchiver :: Archiver
defaultArchiver toolChain buildFlags inputs output = do
    need inputs
    system' (tool archiverCmd toolChain)
        $ buildFlags ^. archiverFlags
        ++ [output]
        ++ inputs

defaultArchiveFileName :: String -> FilePath
defaultArchiveFileName = ("lib"++) . (<.> "a")

defaultLinker :: Linker
defaultLinker toolChain buildFlags inputs output = do
    let staticLibs = buildFlags ^. staticLibraries
    need $ inputs ++ staticLibs
    system' (tool linkerCmd toolChain)
          $  inputs
          ++ buildFlags ^. linkerFlags
          ++ staticLibs
          ++ concatMapFlag "-L" (buildFlags ^. libraryPath)
          ++ concatMapFlag "-l" (buildFlags ^. libraries)
          ++ ["-o", output]

defaultLinkResultFileName :: LinkResult -> String -> FilePath
defaultLinkResultFileName Executable = id
defaultLinkResultFileName SharedLibrary = ("lib"++) . (<.> "so")
defaultLinkResultFileName DynamicLibrary =            (<.> "so")

defaultToolChain :: ToolChain
defaultToolChain =
    ToolChain {
        _prefix = Nothing
      , _compilerCmd = "gcc"
      , _archiverCmd = "ar"
      , _archiver = defaultArchiver
      , _archiveFileName = defaultArchiveFileName
      , _linkerCmd = "gcc"
      , _linker = \link toolChain ->
            case link of
                Executable -> defaultLinker toolChain
                _          -> defaultLinker toolChain . append linkerFlags ["-shared"]
      , _linkResultFileName = defaultLinkResultFileName
      }

tool :: (Getter ToolChain String) -> ToolChain -> FilePath
tool f toolChain = maybe cmd (flip combine ("bin" </> cmd))
                         (toolChain ^. prefix)
    where cmd = toolChain ^. f

toolChainFromEnvironment :: IO (ToolChain -> ToolChain)
toolChainFromEnvironment = do
  env <- getEnvironment
  return $ maybe id (\cc -> set compilerCmd cc) (lookup "CC" env)

defaultBuildFlags :: BuildFlags
defaultBuildFlags =
    BuildFlags {
        _systemIncludes = []
      , _userIncludes = []
      , _defines = []
      , _preprocessorFlags = []
      , _compilerFlags = []
      , _libraryPath = []
      , _libraries = []
      , _linkerFlags = []
      , _staticLibraries = []
      , _archiverFlags = []
      }

defineFlags :: BuildFlags -> [String]
defineFlags = concatMapFlag "-D" . map (\(a, b) -> maybe a (\b' -> a++"="++b') b) . flip (^.) defines

compilerFlagsFor :: Maybe Language -> BuildFlags -> [String]
compilerFlagsFor lang = concat
                      . maybe (map snd . filter (isNothing.fst))
                              (mapMaybe . f) lang
                      . flip (^.) compilerFlags
    where f _ (Nothing, x) = Just x
          f l (Just l', x) | l == l' = Just x
                           | otherwise = Nothing

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

dependencyFile :: ToolChain -> BuildFlags -> FilePath -> FilePath -> Rules ()
dependencyFile toolChain buildFlags input output = do
    output ?=> \_ -> do
        need [input]
        system' (tool compilerCmd toolChain)
                $  concatMapFlag "-I" (buildFlags ^. systemIncludes)
                ++ mapFlag "-iquote" (buildFlags ^. userIncludes)
                ++ (defineFlags buildFlags)
                ++ (buildFlags ^. preprocessorFlags)
                ++ (compilerFlagsFor (languageOf input) buildFlags)
                ++ ["-MM", "-o", output, input]

parseDependencies :: String -> [FilePath]
parseDependencies = drop 2 . words . filter (/= '\\')

type ObjectRule = ToolChain -> BuildFlags -> FilePath -> [FilePath] -> FilePath -> Rules ()

staticObject :: ObjectRule
staticObject toolChain buildFlags input deps output = do
    let depFile = output <.> "d"
    dependencyFile toolChain buildFlags input depFile
    output ?=> \_ -> do
        deps' <- parseDependencies <$> readFile' depFile
        need $ [input] ++ deps ++ deps'
        system' (tool compilerCmd toolChain)
                $  concatMapFlag "-I" (buildFlags ^. systemIncludes)
                ++ mapFlag "-iquote" (buildFlags ^. userIncludes)
                ++ (defineFlags buildFlags)
                ++ (buildFlags ^. preprocessorFlags)
                ++ (compilerFlagsFor (languageOf input) buildFlags)
                ++ ["-c", "-o", output, input]

sharedObject :: ObjectRule
sharedObject toolChain = staticObject toolChain -- Disable for now: . append compilerFlags [(Nothing, ["-fPIC"])]

mkObjectsDir :: Env -> Target -> FilePath -> FilePath
mkObjectsDir env target path = buildDir env target </> map tr (makeRelative "/" path) ++ "_obj"
    where tr '.' = '_'
          tr x   = x

mkBuildPath :: Env -> Target -> FilePath -> FilePath
mkBuildPath env target path = buildDir env target </> makeRelative "/" path

buildProduct :: ObjectRule -> Linker -> FilePath
             -> Env -> Target -> ToolChain -> BuildFlags
             -> SourceTree BuildFlags
             -> Rules FilePath
buildProduct object link fileName env target toolChain buildFlags sources = do
    let resultPath = mkBuildPath env target fileName
        objectsDir = mkObjectsDir env target fileName
    objects <- forM (buildFlags `applySourceTree` sources) $ \(buildFlags', (src, deps)) -> do
        let obj = objectsDir </> makeRelative "/" (src <.> "o")
        object toolChain buildFlags' src deps obj
        return obj
    resultPath ?=> link toolChain buildFlags objects
    return resultPath

-- | Rule for building an executable.
executable :: Env -> Target -> ToolChain -> BuildFlags -> String -> SourceTree BuildFlags -> Rules FilePath
executable env target toolChain buildFlags name sources =
    buildProduct
        staticObject
        ((toolChain ^. linker) Executable)
        ((toolChain ^. linkResultFileName) Executable name)
        env target toolChain buildFlags sources

-- | Rule for building a static library.
staticLibrary :: Env -> Target -> ToolChain -> BuildFlags -> String -> SourceTree BuildFlags -> Rules FilePath
staticLibrary env target toolChain buildFlags name sources =
    buildProduct
        staticObject
        (toolChain ^. archiver)
        ((toolChain ^. archiveFileName) name)
        env target toolChain buildFlags sources

-- | Rule for building a shared library.
sharedLibrary :: Env -> Target -> ToolChain -> BuildFlags -> String -> SourceTree BuildFlags -> Rules FilePath
sharedLibrary env target toolChain buildFlags name sources =
    buildProduct
        sharedObject
        ((toolChain ^. linker) SharedLibrary)
        ((toolChain ^. linkResultFileName) SharedLibrary name)
        env target toolChain buildFlags sources

-- | Rule for building a dynamic library.
dynamicLibrary :: Env -> Target -> ToolChain -> BuildFlags -> String -> SourceTree BuildFlags -> Rules FilePath
dynamicLibrary env target toolChain buildFlags name sources =
    buildProduct
        sharedObject
        ((toolChain ^. linker) DynamicLibrary)
        ((toolChain ^. linkResultFileName) DynamicLibrary name)
        env target toolChain buildFlags sources
