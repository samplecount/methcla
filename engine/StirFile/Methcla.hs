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

{-# LANGUAGE TypeOperators #-}

module Methcla (
    Variant(..)
  , Options
  , buildConfig
  , defaultOptions
  , optionDescrs
  , version
  , Config(..)
  , buildDir
  , commonRules
  , mkRules
  , libmethclaPNaCl
) where

import           Control.Arrow ((>>>), first, second)
import           Control.Exception as E
import           Data.Char (toLower)
import           Data.Version (Version(..), showVersion)
import           Development.Shake as Shake
import           Development.Shake.FilePath
import           Methcla.Util (under)
import qualified Paths_methcla_stirfile as Package
import           Shakefile.C hiding (under)
import qualified Shakefile.C.Android as Android
import qualified Shakefile.C.Host as Host
import qualified Shakefile.C.NaCl as NaCl
import qualified Shakefile.C.OSX as OSX
import           Shakefile.C.PkgConfig (pkgConfig, pkgConfigWithOptions)
import qualified Shakefile.C.PkgConfig as PkgConfig
import           Shakefile.Configuration
import           Shakefile.Label
import           Shakefile.SourceTree (SourceTree)
import qualified Shakefile.SourceTree as SourceTree
import           System.Console.GetOpt
import           System.Directory (removeFile)
import qualified System.Environment as Env
import           System.FilePath.Find

{-import Debug.Trace-}

-- ====================================================================
-- Utilities

lookupEnv :: String -> IO (Maybe String)
lookupEnv name = do
  e <- E.try $ Env.getEnv name :: IO (Either E.SomeException String)
  case e of
    Left _ -> return Nothing
    Right value -> return $ Just value

isDarwin :: Target -> Bool
isDarwin = isTargetOS (Just "apple") (Just "darwin10")

-- ====================================================================
-- Library variants

data Variant = Default | Pro deriving (Eq, Show)

isPro :: Variant -> Bool
isPro Pro = True
isPro _   = False

engineSources :: Variant -> FilePath -> Target -> SourceTree Action BuildFlags
engineSources Default _ _ =
  SourceTree.empty
engineSources Pro sourceDir _ =
  SourceTree.files $ under sourceDir [
    "src/Methcla/ProAPI.cpp"
  ]

pluginSources :: Variant -> FilePath -> Target -> SourceTree Action BuildFlags
pluginSources Default sourceDir _ =
  SourceTree.files $ under sourceDir [
    "plugins/disksampler_stub.cpp"
  ]
pluginSources Pro sourceDir target =
  SourceTree.list $ [
    SourceTree.files $ under sourceDir [ "plugins/pro/disksampler.cpp" ]
  ] ++ if isDarwin target
       then [ SourceTree.flags (append linkerFlags ["-framework", "AudioToolbox"])
            $ SourceTree.files [ sourceDir </> "plugins/pro/soundfile_api_extaudiofile.cpp" ] ]
       else []

testSources :: Variant -> FilePath -> Target -> SourceTree Action BuildFlags
testSources Default _ _ =
  SourceTree.empty
testSources Pro sourceDir _ =
  SourceTree.files $ under sourceDir [
    "tests/methcla_pro_engine_tests.cpp"
  ]

-- ====================================================================
-- Library

externalLibraries :: FilePath
externalLibraries = "external_libraries"

externalLibrary :: FilePath -> FilePath
externalLibrary = combine externalLibraries

-- boostBuildFlags :: BuildFlags -> BuildFlags
-- boostBuildFlags = append systemIncludes [ boostDir ]

-- | Build flags common to all targets
commonBuildFlags :: BuildFlags -> BuildFlags
commonBuildFlags = append compilerFlags [
    (Just C, ["-std=c99"])
  , (Just Cpp, ["-std=c++11"])
  , (Nothing, ["-Wall", "-Wextra"])
  , (Just Cpp, ["-fvisibility-inlines-hidden"])
  , (Nothing, ["-Werror=return-type"])
  , (Nothing, ["-fstrict-aliasing", "-Wstrict-aliasing"])
  ]

testBuildFlags :: Target -> BuildFlags -> BuildFlags
testBuildFlags target =
    append userIncludes ["tests", "src"]
  . (onlyIf (isDarwin target) $
      append linkerFlags ["-framework", "AudioToolbox", "-framework", "CoreFoundation"])
  . append systemIncludes
      [ externalLibrary "boost" ]

apiIncludes :: FilePath -> BuildFlags -> BuildFlags
apiIncludes sourceDir = append systemIncludes [ sourceDir </> "include" ]

local_libmethcla :: FilePath -> FilePath -> BuildFlags -> BuildFlags
local_libmethcla sourceDir libmethcla =
    apiIncludes sourceDir
  . append localLibraries [ libmethcla ]

-- vectorBuildFlags :: BuildFlags -> BuildFlags
-- vectorBuildFlags = append compilerFlags [
--     (Nothing, [ "-O3", "-save-temps" ])
--   ]

version :: Variant -> String
version variant = showVersion $ Package.version {
    versionTags = versionTags Package.version ++ tags
  }
  where tags = if isPro variant then ["pro"] else []

newtype VersionHeader = VersionHeader { versionHeaderPath :: FilePath }

mkVersionHeader :: Variant -> FilePath -> Rules VersionHeader
mkVersionHeader variant buildDir = do
  let output = buildDir </> "include/Methcla/Version.h"
  output *> \_ -> do
    alwaysRerun
    writeFileChanged output $ unlines [
        "#ifndef METHCLA_VERSION_H_INCLUDED"
      , "#define METHCLA_VERSION_H_INCLUDED"
      , ""
      , "static const char* kMethclaVersion = \"" ++ version variant ++ "\";"
      , ""
      , "#endif /* METHCLA_VERSION_H_INCLUDED */"
      ]
  return $ VersionHeader output

methclaSources :: Variant -> FilePath -> FilePath -> Target -> VersionHeader -> SourceTree Action BuildFlags -> SourceTree Action BuildFlags
methclaSources variant sourceDir buildDir target versionHeader platformSources =
  let tlsfDir = sourceDir </> externalLibrary "tlsf"
      boostDir = sourceDir </> externalLibrary "boost"
      engineBuildFlags =
          -- Library headers
          apiIncludes sourceDir
        . append userIncludes
            [ buildDir </> "include"
            , sourceDir </> "src"
            , sourceDir </> externalLibraries ]
        . append systemIncludes
            [ boostDir, tlsfDir ]
      pluginBuildFlags =
          apiIncludes sourceDir
        . append compilerFlags [(Nothing, ["-O3"])]
  in SourceTree.list [
        -- sourceFlags boostBuildFlags [ SourceTree.files $
        --   under (boostDir </> "libs") [
        --     --   "date_time/src/gregorian/date_generators.cpp"
        --     -- , "date_time/src/gregorian/greg_month.cpp"
        --     -- , "date_time/src/gregorian/greg_weekday.cpp"
        --     -- , "date_time/src/gregorian/gregorian_types.cpp"
        --     -- , "date_time/src/posix_time/posix_time_types.cpp"
        --     -- , "exception/src/clone_current_exception_non_intrusive.cpp"
        --     -- "system/src/error_code.cpp"
        --     ]
        -- ]
        -- TLSF
        SourceTree.files [ tlsfDir </> "tlsf.c" ]
        -- engine
      , SourceTree.flags engineBuildFlags $
          SourceTree.list [
              -- sourceFlags (append compilerFlags [ (Nothing, [ "-O0" ]) ])
              --   [ SourceTree.files [ "src/Methcla/Audio/Engine.cpp" ] ],
              SourceTree.files $ under sourceDir
                [ "src/Methcla/Audio/AudioBus.cpp"
                , "src/Methcla/Audio/Engine.cpp"
                , "src/Methcla/Audio/EngineImpl.cpp"
                , "src/Methcla/Audio/Group.cpp"
                , "src/Methcla/Audio/IO/Driver.cpp"
                , "src/Methcla/Audio/Node.cpp"
                -- , "src/Methcla/Audio/Resource.cpp"
                , "src/Methcla/Audio/Synth.cpp"
                , "src/Methcla/Audio/SynthDef.cpp"
                , "src/Methcla/Memory/Manager.cpp"
                , "src/Methcla/Memory.cpp"
                -- Disable for now
                -- , "src/Methcla/Plugin/Loader.cpp"
                , "src/Methcla/Utility/Semaphore.cpp"
                ]
            , SourceTree.filesWithDeps $ map (first (combine sourceDir))
                [ ("src/Methcla/API.cpp", [ versionHeaderPath versionHeader ]) ]
                -- ++ [ "external_libraries/zix/ring.c" ] -- Unused ATM
              -- platform dependent
            , platformSources
            , engineSources variant sourceDir target
          -- , sourceTree_ (vectorBuildFlags . engineBuildFlags) $ sourceFiles $
          --     under "src" [ "Methcla/Audio/DSP.c" ]
        ]
      , SourceTree.flags pluginBuildFlags $ SourceTree.list [
          SourceTree.files $ under sourceDir [
              "plugins/node-control.cpp"
            , "plugins/patch-cable.cpp"
            , "plugins/sampler.cpp"
            , "plugins/sine.c"
            , "plugins/soundfile_api_dummy.cpp" ]
        , pluginSources variant sourceDir target ]
      ]

-- plugins :: Platform -> [Library]
-- plugins platform = [
--     Library "sine" $ sourceFlags engineBuildFlags [ SourceTree.files [ "plugins/methc.la/plugins/sine/sine.c" ] ]
--   ]

-- ====================================================================
-- Additional libraries

rtAudio :: FilePath -> SourceTree Action BuildFlags
rtAudio sourceDir =
  SourceTree.flags flags $ SourceTree.files $ [
      platformDir </> "Methcla/Audio/IO/RtAudioDriver.cpp"
    , libDir </> "RTAudio.cpp"
    ]
  where
    platformDir = sourceDir </> "platform/rtaudio"
    libDir = sourceDir </> externalLibrary "rtaudio"
    flags =
          append userIncludes [ platformDir, libDir ]
        . Host.onlyOn [Host.OSX] (  append defines [ ("__MACOSX_CORE__", Nothing)
                                                   , ("HAVE_GETTIMEOFDAY", Nothing) ]
                                  . append linkerFlags [ "-framework", "CoreAudio"
                                                       , "-framework", "CoreFoundation" ] )

-- ====================================================================
-- Configurations

data Config = Debug | Release deriving (Eq, Show)

instance ToBuildPrefix Config where
  toBuildPrefix = map toLower . show

parseConfig :: String -> Either String Config
parseConfig x =
    case map toLower x of
        "debug" -> Right Debug
        "release" -> Right Release
        _ -> Left $ "Invalid configuration `" ++ x ++ "'"

configurations :: [Configuration Config BuildFlags]
configurations = [
    ( Release,
        append compilerFlags [
          (Nothing, ["-Os", "-g"])
        , (Nothing, ["-fvisibility=hidden"])
        ]
      . append defines [("NDEBUG", Nothing)]
    )
  , ( Debug,
        append compilerFlags [
            (Nothing, ["-O0", "-g"])
          , (Nothing, ["-fstack-protector", "-Wstack-protector"])
          ]
      . append defines [("DEBUG", Just "1")]
    )
  ]

-- ====================================================================
-- Commandline targets

data Options = Options {
    _buildConfig :: Config
  } deriving (Show)

buildConfig :: Options :-> Config
buildConfig = lens _buildConfig
                   (\g f -> f { _buildConfig = g (_buildConfig f) })

defaultOptions :: Options
defaultOptions = Options {
    _buildConfig = Debug
  }

optionDescrs :: [OptDescr (Either String (Options -> Options))]
optionDescrs = [ Option "c" ["config"]
                   (ReqArg (fmap (set buildConfig) . parseConfig) "CONFIG")
                   "Build configuration (debug, release)." ]

enable :: Bool -> String -> String -> String
enable on flag name = flag ++ (if on then "" else "no-") ++ name

-- | Standard math library.
libm :: BuildFlags -> BuildFlags
libm = append libraries ["m"]

-- | Pthread library.
libpthread :: BuildFlags -> BuildFlags
libpthread = append libraries ["pthread"]

-- | Pass -stdlib=libc++ (clang).
stdlib_libcpp :: ToolChain -> BuildFlags -> BuildFlags
stdlib_libcpp toolChain = onlyIf (get variant toolChain == LLVM) $
    append compilerFlags [(Just Cpp, flags)]
  . append linkerFlags flags
  where flags = ["-stdlib=libc++"]

rtti :: Bool -> BuildFlags -> BuildFlags
rtti on = append compilerFlags [(Just Cpp, [enable on "-f" "rtti"])]

exceptions :: Bool -> BuildFlags -> BuildFlags
exceptions on = append compilerFlags [(Just Cpp, [enable on "-f" "exceptions"])]

execStack :: Bool -> BuildFlags -> BuildFlags
execStack on =
    append compilerFlags [(Nothing, ["-Wa,--" ++ no ++ "execstack"])]
  . append linkerFlags ["-Wl,-z," ++ no ++ "execstack"]
  where no = if on then "" else "no"

withTarget :: Monad m => (Arch -> Target) -> [Arch] -> (Target -> m ()) -> m ()
withTarget mkTarget archs f = mapM_ (f . mkTarget) archs

mapTarget :: Monad m => (Arch -> Target) -> [Arch] -> (Target -> m a) -> m [a]
mapTarget mkTarget archs f = mapM (f . mkTarget) archs

androidTargetPlatform :: Platform
androidTargetPlatform = Android.platform 9

copyTo :: FilePath -> FilePath -> Rules FilePath
copyTo input output = do
  output ?=> \_ -> copyFile' input output
  return output

phonyFiles :: String -> [FilePath] -> Rules ()
phonyFiles target = phony target . need

phonyFile :: String -> FilePath -> Rules ()
phonyFile target input = phonyFiles target [input]

mkBuildPrefix :: FilePath -> Config -> String -> FilePath
mkBuildPrefix buildDir config platform =
      buildDir
  </> toBuildPrefix config
  </> platform

platformBuildPrefix :: FilePath -> Config -> Platform -> FilePath
platformBuildPrefix buildDir config platform =
  mkBuildPrefix buildDir config (toBuildPrefix platform)

targetBuildPrefix :: FilePath -> Config -> Target -> FilePath
targetBuildPrefix buildDir config target =
  mkBuildPrefix buildDir config (toBuildPrefix target)

libmethclaPNaCl :: Variant
                -> FilePath
                -> FilePath
                -> FilePath
                -> Config
                -> PkgConfig.Options
                -> Rules (FilePath, Action (BuildFlags -> BuildFlags))
libmethclaPNaCl variant sdk sourceDir buildDir config pkgConfigOptions = do
  let target = NaCl.target NaCl.canary
      naclConfig = case config of
                    Debug -> NaCl.Debug
                    Release -> NaCl.Release
      toolChain = NaCl.toolChain
                    sdk
                    naclConfig
                    target
      pkgConfig_libsndfile = pkgConfigWithOptions
                              (pkgConfigOptions { PkgConfig.static = True })
                              "sndfile"
      buildFlags = do
        libsndfile <- pkgConfig_libsndfile
        return $     applyConfiguration config configurations
                 >>> commonBuildFlags
                 -- Currently -std=c++11 produces compile errors with libc++
                 >>> append compilerFlags [(Just Cpp, ["-std=gnu++11"])]
                 >>> append userIncludes [ sourceDir </> "platform/pepper" ]
                 >>> stdlib_libcpp toolChain
                 >>> append linkerFlags [ "--pnacl-exceptions=sjlj" ]
                 >>> libsndfile
  versionHeader <- mkVersionHeader variant buildDir
  libmethcla <- staticLibrary toolChain (targetBuildPrefix buildDir config target </> "libmethcla.a")
                  $ SourceTree.flagsM buildFlags
                  $ methclaSources variant sourceDir buildDir target versionHeader
                  $ SourceTree.files $ under sourceDir [
                      "platform/pepper/Methcla/Audio/IO/PepperDriver.cpp"
                    , "plugins/soundfile_api_libsndfile.cpp"
                    -- , "plugins/soundfile_api_mpg123.cpp"
                    ]
  return (libmethcla, do
    libsndfile <- pkgConfig_libsndfile
    return $ local_libmethcla sourceDir libmethcla
              -- FIXME: How to select only linker flags?
              -- Maybe pkgConfig could return linker and other flags separately?
              -- TODO: Apply libsndfile to empty BuildFlags and append resulting libraries and linker flags here.
              >>> libsndfile
              -- >>> append libraries ["mpg123"]
    )

localSourceDir :: FilePath
localSourceDir = "."

buildDir :: FilePath
buildDir = "build"

commonRules :: Rules ()
commonRules = phony "clean" $ removeFilesAfter buildDir ["//*"]

mkRules :: Variant -> Options -> [([String], IO (Rules ()))]
mkRules variant options = do
    let config = get buildConfig options
        targetBuildPrefix' target = targetBuildPrefix buildDir config target
        platformAlias p = phony (platformString p) . need . (:[])
        targetAlias target = phony (platformString (get targetPlatform target) ++ "-" ++ archString (get targetArch target))
                                . need . (:[])
        mkVersionHeader' = mkVersionHeader variant buildDir
        methclaSources' = methclaSources variant localSourceDir buildDir
    [ (["iphoneos", "iphonesimulator", "iphone-universal"], do
        let iOS_SDK = Version [7,1] []
            iosBuildFlags toolChain =
                    OSX.iphoneos_version_min (Version [5,0] [])
                >>> append userIncludes [ "platform/ios"
                                        , externalLibrary "CoreAudioUtilityClasses/CoreAudio/PublicUtility" ]
                >>> stdlib_libcpp toolChain
            iosSources target versionHeader =
              methclaSources' target versionHeader $ SourceTree.files $
                [ "platform/ios/Methcla/Audio/IO/RemoteIODriver.cpp"
                , externalLibrary "CoreAudioUtilityClasses/CoreAudio/PublicUtility/CAHostTimeBase.cpp" ]
                ++ if isPro variant then [ "platform/ios/Methcla/ProAPI.cpp" ] else []

        applyEnv <- toolChainFromEnvironment
        developer <- OSX.getDeveloperPath
        return $ do
            versionHeader <- mkVersionHeader'

            let iphoneosPlatform = OSX.iPhoneOS iOS_SDK
            iphoneosLibs <- mapTarget (flip OSX.target iphoneosPlatform) [Arm Armv7, Arm Armv7s] $ \target -> do
                let toolChain = applyEnv $ OSX.toolChain developer target
                    buildFlags =   applyConfiguration config configurations
                               >>> commonBuildFlags
                               >>> iosBuildFlags toolChain
                lib <- staticLibrary toolChain (targetBuildPrefix' target </> "libmethcla.a")
                        $ SourceTree.flags buildFlags
                        $ iosSources target versionHeader
                return lib
            iphoneosLib <- OSX.universalBinary
                            iphoneosLibs
                            (platformBuildPrefix buildDir config iphoneosPlatform </> "libmethcla.a")
            phony "iphoneos" (need [iphoneosLib])

            let iphonesimulatorPlatform = OSX.iPhoneSimulator iOS_SDK
            iphonesimulatorLibI386 <- do
                let target = OSX.target (X86 I386) iphonesimulatorPlatform
                    toolChain = applyEnv $ OSX.toolChain developer target
                    buildFlags =   applyConfiguration config configurations
                               >>> commonBuildFlags
                               >>> iosBuildFlags toolChain
                lib <- staticLibrary toolChain (targetBuildPrefix' target </> "libmethcla.a")
                        $ SourceTree.flags buildFlags
                        $ iosSources target versionHeader
                return lib
            let iphonesimulatorLib = platformBuildPrefix buildDir config iphonesimulatorPlatform </> "libmethcla.a"
            iphonesimulatorLib *> copyFile' iphonesimulatorLibI386
            phony "iphonesimulator" (need [iphonesimulatorLib])

            let universalTarget = "iphone-universal"
            universalLib <- OSX.universalBinary
                                (iphoneosLibs ++ [iphonesimulatorLib])
                                (mkBuildPrefix buildDir config universalTarget </> "libmethcla.a")
            phony universalTarget (need [universalLib])
        )
      , (["android", "android-tests"], do
            applyEnv <- toolChainFromEnvironment
            ndk <- Env.getEnv "ANDROID_NDK"
            return $ do
                versionHeader <- mkVersionHeader'
                libs <- mapTarget (flip Android.target androidTargetPlatform) [Arm Armv5, Arm Armv7] $ \target -> do
                    let compiler = (LLVM, Version [3,4] [])
                        -- compiler = (GCC, Version [4,8] [])
                        abi = Android.abiString (get targetArch target)
                        toolChain = applyEnv $ Android.toolChain ndk compiler target
                        buildFlags =   applyConfiguration config configurations
                                   >>> commonBuildFlags
                                   >>> append userIncludes ["platform/android"]
                                   -- >>> Android.gnustl gccVersion Static ndk target
                                   >>> Android.libcxx Static ndk target
                                   >>> rtti True
                                   >>> exceptions True
                                   >>> execStack False
                    libmethcla <- staticLibrary toolChain (targetBuildPrefix' target </> "libmethcla.a")
                                    $ SourceTree.flags buildFlags
                                    $ methclaSources' target versionHeader
                                    $ SourceTree.files [
                                        "platform/android/opensl_io.c",
                                        "platform/android/Methcla/Audio/IO/OpenSLESDriver.cpp" ]
                    let androidTestBuildFlags =
                                       buildFlags
                                   >>> append systemIncludes [externalLibrary "catch/single_include"]
                                   >>> append linkerFlags ["-Wl,-soname,libmethcla-tests.so"]
                                   >>> append libraries ["android", "log", "OpenSLES"]
                                   >>> local_libmethcla localSourceDir libmethcla
                                   >>> testBuildFlags target
                    libmethcla_tests <- sharedLibrary toolChain (targetBuildPrefix' target </> "libmethcla-tests.so")
                                  -- TODO: Build static library for native_app_glue to link against.
                                  $ SourceTree.flags androidTestBuildFlags
                                  $ SourceTree.append (Android.native_app_glue ndk)
                                                      (SourceTree.files [
                                                          "src/Methcla/Audio/IO/DummyDriver.cpp"
                                                        , "tests/methcla_tests.cpp"
                                                        , "tests/android/main.cpp" ])

                    let installPath = mkBuildPrefix buildDir config "android"
                                        </> abi
                                        </> takeFileName libmethcla
                    installPath ?=> \_ -> copyFile' libmethcla installPath
                    targetAlias target installPath

                    let testInstallPath = "tests/android/libs" </> abi </> takeFileName libmethcla_tests
                    testInstallPath ?=> \_ -> copyFile' libmethcla_tests testInstallPath
                    return (installPath, testInstallPath)
                phony "android" $ need $ map fst libs
                phony "android-tests" $ need $ map snd libs
        )
      , (["pnacl", "pnacl-test", "pnacl-examples"], do
        sdk <- Env.getEnv "NACL_SDK"
        freesoundApiKey <- Env.getEnv "FREESOUND_API_KEY"
        return $ do
          let target = NaCl.target NaCl.canary
              naclConfig = case config of
                              Debug -> NaCl.Debug
                              Release -> NaCl.Release
              toolChain = NaCl.toolChain
                            sdk
                            naclConfig
                            target
              buildPrefix = targetBuildPrefix' target
              buildFlags =   applyConfiguration config configurations
                         >>> commonBuildFlags
                         -- Currently -std=c++11 produces compile errors with libc++
                         -- -std=c++11 defines __STRICT_ANSI__ and then newlib doesn't export fileno (needed by catch)
                         >>> append compilerFlags [(Just Cpp, ["-std=gnu++11"])]
                         >>> NaCl.libppapi_cpp
                         >>> NaCl.libppapi
                         >>> libpthread
          (libmethcla_a, get_libmethcla) <- libmethclaPNaCl variant sdk localSourceDir buildDir config PkgConfig.defaultOptions
          phony "pnacl" $ need [libmethcla_a]
          let pnaclTestBuildFlags = do
                libmethcla <- get_libmethcla
                return $     buildFlags
                         >>> append defines [ ("METHCLA_TEST_SOUNDFILE_API_HEADER", Just "<methcla/plugins/soundfile_api_dummy.h>")
                                   , ("METHCLA_TEST_SOUNDFILE_API_LIB", Just "methcla_soundfile_api_dummy") ]
                         >>> append systemIncludes [externalLibrary "catch/single_include"]
                         -- >>> NaCl.libppapi_simple
                         -- >>> NaCl.libnacl_io
                         >>> testBuildFlags target
                         >>> libmethcla
          pnacl_test_bc <- executable toolChain (buildPrefix </> "methcla-pnacl-tests.bc")
                           $ SourceTree.flagsM pnaclTestBuildFlags
                           $ SourceTree.list [
                              SourceTree.files [
                                  "src/Methcla/Audio/IO/DummyDriver.cpp"
                                , "tests/methcla_tests.cpp"
                                , "tests/methcla_engine_tests.cpp"
                                , "tests/test_runner_nacl.cpp"
                                ]
                              , testSources variant localSourceDir target ]
          pnacl_test <- NaCl.finalize toolChain pnacl_test_bc (pnacl_test_bc `replaceExtension` "pexe")
          pnacl_test_nmf <- NaCl.mk_nmf [(NaCl.PNaCl, pnacl_test)]
                                        (pnacl_test `replaceExtension` "nmf")
          pnacl_test' <- pnacl_test `copyTo` ("tests/pnacl" </> takeFileName pnacl_test)
          pnacl_test_nmf' <- pnacl_test_nmf `copyTo` ("tests/pnacl" </> takeFileName pnacl_test_nmf)
          phonyFiles "pnacl-test" [pnacl_test', pnacl_test_nmf']

          let examplesBuildFlags flags = do
                libmethcla <- get_libmethcla
                return $ buildFlags >>> libmethcla >>> flags
              examplesDir = buildDir </> "examples/pnacl"
              mkExample output flags sources files = do
                let outputDir = takeDirectory output
                bc <- executable toolChain (buildPrefix </> takeFileName output <.> "bc")
                        $ SourceTree.flagsM (examplesBuildFlags flags)
                        $ SourceTree.files sources
                pexe <- NaCl.finalize
                          toolChain
                          bc
                          (bc `replaceExtension` "pexe"
                              `replaceDirectory` (outputDir </> show naclConfig))
                nmf <- NaCl.mk_nmf
                          [(NaCl.PNaCl, pexe)]
                          (pexe `replaceExtension` "nmf")
                files' <- mapM (\old -> old `copyTo` (old `replaceDirectory` outputDir))
                               (["examples/common/common.js"] ++ files)
                return $ [pexe, nmf] ++ files'

          thaddeus <- mkExample ( examplesDir </> "thaddeus/methcla-thaddeus" )
                                ( append userIncludes [ "examples/thADDeus/src" ] )
                                [
                                  "examples/thADDeus/pnacl/main.cpp"
                                , "examples/thADDeus/src/synth.cpp"
                                ]
                                [
                                  "examples/thADDeus/pnacl/index.html"
                                , "examples/thADDeus/pnacl/manifest.json"
                                , "examples/thADDeus/pnacl/thaddeus.js"
                                ]
          sampler <- mkExample  ( examplesDir </> "sampler/methcla-sampler" )
                                ( append userIncludes   [ "examples/sampler/src" ]
                                . append systemIncludes [ "examples/sampler/libs/tinydir" ]
                                . NaCl.libnacl_io )
                                [
                                  "examples/sampler/pnacl/main.cpp"
                                , "examples/sampler/src/Engine.cpp"
                                ]
                                [
                                  "examples/sampler/pnacl/index.html"
                                , "examples/sampler/pnacl/manifest.json"
                                , "examples/sampler/pnacl/example.js"
                                ]

          let freesounds = map (takeFileName.takeDirectory) [
                  "http://freesound.org/people/adejabor/sounds/157965/"
                , "http://freesound.org/people/NOISE.INC/sounds/45394/"
                , "http://freesound.org/people/ThePriest909/sounds/209331/"
                ]

          (examplesDir </> "sampler/sounds/*") *> \output -> do
            cmd "curl" [    "http://www.freesound.org/api/sounds/"
                         ++ dropExtension (takeFileName output)
                         ++ "/serve?api_key=" ++ freesoundApiKey
                       , "-L", "-o", output ] :: Action ()

          -- Install warp-static web server if needed
          let server = ".cabal-sandbox/bin/warp"

          server *> \_ -> do
            cmd "cabal sandbox init" :: Action ()
            cmd "cabal install -j warp-static" :: Action ()

          phony "pnacl-examples" $ do
            need [server]
            need thaddeus
            need $ sampler ++ map (combine (examplesDir </> "sampler/sounds"))
                                  freesounds
            cmd (Cwd examplesDir) server :: Action ()
        )
      , (["desktop"], do
            applyEnv <- toolChainFromEnvironment
            (target, toolChain) <- fmap (second applyEnv) Host.getDefaultToolChain
            let buildFlags = do
                  libsndfile <- pkgConfig "sndfile"
                  libmpg123 <- pkgConfig "libmpg123"
                  return $     applyConfiguration config configurations
                           >>> commonBuildFlags
                           >>> append defines [("BUILDING_DLL", Nothing)]
                           >>> libsndfile
                           >>> libmpg123
                           >>> stdlib_libcpp toolChain
                           >>> libm
                build versionHeader f ext =
                  f toolChain (targetBuildPrefix' target </> "libmethcla" <.> ext)
                    $ SourceTree.flagsM buildFlags
                    $ methclaSources' target versionHeader
                    $ SourceTree.list [ rtAudio localSourceDir
                                      , SourceTree.files [
                                            "plugins/soundfile_api_libsndfile.cpp"
                                          , "plugins/soundfile_api_mpg123.cpp"
                                      ] ]
            return $ do
                versionHeader <- mkVersionHeader'

                staticLib <- build versionHeader staticLibrary "a"
                sharedLib <- build versionHeader sharedLibrary Host.sharedLibraryExtension

                phony "desktop" $ do
                  need [sharedLib]
                  if isDarwin target
	              -- Quick hack for setting install path of shared library
		      then system' "install_name_tool" ["-id", "@executable_path/../Resources/libmethcla.dylib", sharedLib]
		      else return ()
        )
      , (["test", "clean-test"], do -- tests
            applyEnv <- toolChainFromEnvironment
            (target, toolChain) <- fmap (second applyEnv) Host.getDefaultToolChain
            let buildFlags =   applyConfiguration config configurations
                           >>> commonBuildFlags
                           >>> stdlib_libcpp toolChain
                           >>> Host.onlyOn [Host.Linux] libpthread
                           >>> libm
                           >>> testBuildFlags target
            return $ do
                versionHeader <- mkVersionHeader'
                result <- executable toolChain (targetBuildPrefix' target </> "methcla-tests" <.> Host.executableExtension)
                          $ SourceTree.flags buildFlags
                          $ methclaSources' target versionHeader
                          $ SourceTree.list [
                                SourceTree.files [
                                    "src/Methcla/Audio/IO/DummyDriver.cpp"
                                  , "tests/methcla_tests.cpp"
                                  , "tests/methcla_engine_tests.cpp"
                                  , "tests/test_runner_console.cpp"
                                  , externalLibrary "gtest/gtest-all.cc" ]
                              , testSources variant localSourceDir target ]
                phony "test" $ do
                    need [result]
                    system' result []
                phony "clean-test" $ removeFilesAfter "tests/output" ["*.osc", "*.wav"]
        )
      , (["tags"], do -- tags
            let and_ a b = do { as <- a; bs <- b; return $! as ++ bs }
                files clause dir = find always clause dir
                sources = files (extension ~~? ".h*" ||? extension ~~? ".c*")
                tagFile = "tags"
                tagFiles = "tagfiles"
            return $ do
                tagFile ?=> \output -> flip actionFinally (removeFile tagFiles) $ do
                    fs <- liftIO $ find
                              (fileName /=? "typeof") (extension ==? ".hpp") (externalLibrary "boost" </> "boost")
                        `and_` sources "include"
                        `and_` sources "platform"
                        `and_` sources "plugins"
                        `and_` sources "src"
                    need fs
                    writeFileLines tagFiles fs
                    system' "ctags" $
                        (words "--sort=foldcase --c++-kinds=+p --fields=+iaS --extra=+q --tag-relative=yes")
                     ++ ["-f", output]
                     ++ ["-L", tagFiles]
        )
        ]
