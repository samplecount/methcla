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

{-# LANGUAGE TemplateHaskell #-}

import           Control.Arrow ((>>>), second)
import           Control.Lens hiding (Action, (<.>), under)
import           Data.Char (toLower)
import           Data.Version (Version(..), showVersion)
import           Development.Shake as Shake
import           Development.Shake.FilePath
import qualified MethclaPro as Pro
import qualified Paths_shakefile as Package
import           Shakefile.C
import qualified Shakefile.C.Android as Android
import           Shakefile.C.Host (getDefaultToolChain)
import qualified Shakefile.C.OSX as OSX
import           Shakefile.C.PkgConfig (pkgConfig)
import           Shakefile.Configuration
import           Shakefile.Lens
import           Shakefile.SourceTree (SourceTree)
import qualified Shakefile.SourceTree as SourceTree
import           System.Console.GetOpt
import           System.Directory (removeFile)
import           System.Environment (lookupEnv)
import           System.FilePath.Find

{-import Debug.Trace-}

-- ====================================================================
-- Library

externalLibraries :: FilePath
externalLibraries = "external_libraries"

externalLibrary :: FilePath -> FilePath
externalLibrary = combine externalLibraries

boostDir :: FilePath
boostDir = externalLibrary "boost"

-- boostBuildFlags :: BuildFlags -> BuildFlags
-- boostBuildFlags = append systemIncludes [ boostDir ]

tlsfDir :: FilePath
tlsfDir = externalLibrary "tlsf"

-- | Build flags common to all targets
commonBuildFlags :: BuildFlags -> BuildFlags
commonBuildFlags = append compilerFlags [
    (Just C, ["-std=c11"])
  , (Just Cpp, ["-std=c++11"])
  , (Nothing, ["-Wall", "-Wextra"])
  , (Just Cpp, ["-fvisibility-inlines-hidden"])
  , (Nothing, ["-fstrict-aliasing", "-Wstrict-aliasing"])
  ]

apiIncludes :: BuildFlags -> BuildFlags
apiIncludes = append systemIncludes [
    "include"
  , "external_libraries/oscpp/include" ]

engineBuildFlags :: BuildFlags -> BuildFlags
engineBuildFlags =
    append userIncludes
      ( -- Generated headers
        [ shakeBuildDir </> "include" ]
        -- Library headers
     ++ [ "src" ]
        -- External libraries
     ++ [ externalLibraries ] )
  . append systemIncludes
       ( -- API headers
         [ "include" ]
         -- Boost
      ++ [ boostDir ]
         -- oscpp
      ++ [ externalLibrary "oscpp/include" ]
         -- TLSF
      ++ [ tlsfDir ] )

pluginBuildFlags :: BuildFlags -> BuildFlags
pluginBuildFlags =
    append systemIncludes [ "include", externalLibrary "oscpp/include" ]
  . append compilerFlags [(Nothing, ["-O3"])]

pluginSources :: SourceTree BuildFlags
pluginSources = SourceTree.files [
    "plugins/patch-cable.cpp"
  , "plugins/sampler.cpp"
  , "plugins/sine.c"
  , "plugins/soundfile_api_dummy.cpp"
  ]

-- vectorBuildFlags :: BuildFlags -> BuildFlags
-- vectorBuildFlags = append compilerFlags [
--     (Nothing, [ "-O3", "-save-temps" ])
--   ]

methclaSources :: SourceTree BuildFlags -> SourceTree BuildFlags
methclaSources platformSources =
    SourceTree.list [
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
              SourceTree.files
                [ "src/Methcla/Audio/AudioBus.cpp"
                , "src/Methcla/Audio/Engine.cpp"
                , "src/Methcla/Audio/Group.cpp"
                , "src/Methcla/Audio/IO/Driver.cpp"
                , "src/Methcla/Audio/Node.cpp"
                -- , "src/Methcla/Audio/Resource.cpp"
                , "src/Methcla/Audio/Synth.cpp"
                , "src/Methcla/Audio/SynthDef.cpp"
                , "src/Methcla/Memory/Manager.cpp"
                , "src/Methcla/Memory.cpp"
                , "src/Methcla/Plugin/Loader.cpp"
                , "src/Methcla/Utility/Semaphore.cpp"
                ]
            , SourceTree.filesWithDeps
                [ ("src/Methcla/API.cpp", [ fst versionHeader ]) ]
                -- ++ [ "external_libraries/zix/ring.c" ] -- Unused ATM
              -- platform dependent
            , platformSources
            , Pro.engineSources
          -- , sourceTree_ (vectorBuildFlags . engineBuildFlags) $ sourceFiles $
          --     under "src" [ "Methcla/Audio/DSP.c" ]
        ]
      , SourceTree.flags pluginBuildFlags $ SourceTree.list $ [pluginSources] ++ map snd Pro.pluginSources
      ]

methcla :: String
methcla = "methcla"

versionHeader :: (FilePath, String)
versionHeader = (shakeBuildDir </> "include/Methcla/Version.h", unlines [
    "#ifndef METHCLA_VERSION_H_INCLUDED"
  , "#define METHCLA_VERSION_H_INCLUDED"
  , ""
  , "static const char* kMethclaVersion = \"" ++ showVersion (Package.version) ++ "\";"
  , ""
  , "#endif /* METHCLA_VERSION_H_INCLUDED */"
  ])

-- plugins :: Platform -> [Library]
-- plugins platform = [
--     Library "sine" $ sourceFlags engineBuildFlags [ SourceTree.files [ "plugins/methc.la/plugins/sine/sine.c" ] ]
--   ]

-- ====================================================================
-- Configurations

data Config = Debug | Release deriving (Eq, Show)

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
          (Nothing, ["-Os", "-gdwarf-2"])
        , (Nothing, ["-fvisibility=hidden"])
        ]
      . append defines [("NDEBUG", Nothing)]
    )
  , ( Debug,
        append compilerFlags [
            (Nothing, ["-O0", "-gdwarf-2"])
          , (Nothing, ["-fstack-protector", "-Wstack-protector"])
          ]
      . append defines [("DEBUG", Just "1")]
    )
  ]

-- ====================================================================
-- Commandline targets

shakeBuildDir :: String
shakeBuildDir = "build"

data Options = Options {
    _buildConfig :: Config
  } deriving (Show)

makeLenses ''Options

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

-- | Build with specific C++ standard library (clang).
stdlib :: String -> BuildFlags -> BuildFlags
stdlib libcpp = append compilerFlags [(Just Cpp, ["-stdlib="++libcpp])]

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

mkBuildPrefix :: Show a => a -> FilePath -> FilePath
mkBuildPrefix config target = shakeBuildDir </> map toLower (show config) </> target

commonRules :: Rules ()
commonRules =
    fst versionHeader *> \file -> do
        alwaysRerun
        writeFileChanged file (snd versionHeader)

mkRules :: Options -> IO (Rules ())
mkRules options = do
    let config = options ^. buildConfig
        mkEnv target = set buildPrefix
                            (defaultBuildPrefix target (show config))
                            -- (mkBuildPrefix config (platformString $ target ^. targetPlatform))
                            defaultEnv
        platformAlias p = phony (platformString p) . need . (:[])
        targetAlias target = phony (platformString (target ^. targetPlatform) ++ "-" ++ archString (target ^. targetArch))
                                . need . (:[])
    applyEnv <- toolChainFromEnvironment
    fmap sequence_ $ sequence [
        do
            return $ phony "clean" $ removeFilesAfter shakeBuildDir ["//*"]
      , do -- iphone
            let iOS_SDK = Version [6,1] []
                iosBuildFlags =
                        append userIncludes [ "platform/ios"
                                            , externalLibrary "CoreAudioUtilityClasses/CoreAudio/PublicUtility" ]
                    >>> stdlib "libc++"
                iosSources = methclaSources $ SourceTree.files $
                                [ "platform/ios/Methcla/Audio/IO/RemoteIODriver.cpp"
                                , externalLibrary "CoreAudioUtilityClasses/CoreAudio/PublicUtility/CAHostTimeBase.cpp" ]
                                ++ if Pro.isPresent then [ "platform/ios/Methcla/ProAPI.cpp" ] else []
            developer <- OSX.getDeveloperPath
            return $ do
                iphoneosLibs <- mapTarget (flip OSX.target (OSX.iPhoneOS iOS_SDK)) [Arm Armv7, Arm Armv7s] $ \target -> do
                    let toolChain = applyEnv $ OSX.toolChain_IOS target developer
                        env = mkEnv target
                        buildFlags =   applyConfiguration config configurations
                                   >>> commonBuildFlags
                                   >>> iosBuildFlags
                    lib <- staticLibrary env target toolChain methcla (SourceTree.flags buildFlags iosSources)
                    return lib
                iphoneosLib <- OSX.universalBinary
                                iphoneosLibs
                                (mkBuildPrefix config "iphoneos" </> "libmethcla.a")
                phony "iphoneos" (need [iphoneosLib])

                iphonesimulatorLibI386 <- do
                    let platform = OSX.iPhoneSimulator iOS_SDK
                        target = OSX.target (X86 I386) platform
                        toolChain = applyEnv $ OSX.toolChain_IOS_Simulator target developer
                        env = mkEnv target
                        buildFlags =   applyConfiguration config configurations
                                   >>> commonBuildFlags
                                   >>> iosBuildFlags
                    lib <- staticLibrary env target toolChain methcla (SourceTree.flags buildFlags iosSources)
                    return lib
                let iphonesimulatorLib = mkBuildPrefix config "iphonesimulator" </> "libmethcla.a"
                iphonesimulatorLib *> copyFile' iphonesimulatorLibI386
                phony "iphonesimulator" (need [iphonesimulatorLib])

                let universalTarget = "iphone-universal"
                universalLib <- OSX.universalBinary
                                    (iphoneosLibs ++ [iphonesimulatorLib])
                                    (mkBuildPrefix config universalTarget </> "libmethcla.a")
                phony universalTarget (need [universalLib])
      , do -- android
            ndk <- maybe "." id `fmap` lookupEnv "ANDROID_NDK"
            return $ do
                libs <- mapTarget (flip Android.target androidTargetPlatform) [Arm Armv5, Arm Armv7] $ \target -> do
                    let abi = Android.abiString (target ^. targetArch)
                        toolChain = applyEnv $ Android.toolChain ndk Android.GCC_4_7 target
                        buildFlags =   applyConfiguration config configurations
                                   >>> commonBuildFlags
                                   >>> append userIncludes ["platform/android"]
                                   >>> Android.gnustl Static ndk target
                                   >>> rtti True
                                   >>> exceptions True
                                   >>> execStack False
                    libmethcla <- staticLibrary (mkEnv target) target toolChain methcla $
                                    SourceTree.flags buildFlags $ methclaSources $
                                        SourceTree.files [
                                          "platform/android/opensl_io.c",
                                          "platform/android/Methcla/Audio/IO/OpenSLESDriver.cpp" ]
                    let testBuildFlags =
                                       engineBuildFlags
                                   >>> append systemIncludes [externalLibrary "catch/single_include"]
                                   >>> append linkerFlags ["-Wl,-soname,libmethcla-tests.so"]
                                   >>> append staticLibraries [libmethcla]
                                   >>> append libraries ["android", "log", "OpenSLES"]
                                   >>> buildFlags
                    libmethcla_tests <- sharedLibrary (mkEnv target) target toolChain
                                  "methcla-tests"
                                  -- TODO: Build static library for native_app_glue to link against.
                                  $ SourceTree.flags testBuildFlags
                                  $ SourceTree.append (Android.native_app_glue ndk)
                                                      (SourceTree.files [ "tests/methcla_tests.cpp"
                                                                       , "tests/android/main.cpp" ])

                    let installPath = "libs/android" </> abi </> takeFileName libmethcla
                    installPath ?=> \_ -> copyFile' libmethcla installPath
                    targetAlias target installPath

                    let testInstallPath = "tests/android/libs" </> Android.abiString (target ^. targetArch) </> takeFileName libmethcla_tests
                    testInstallPath ?=> \_ -> copyFile' libmethcla_tests testInstallPath
                    return (installPath, testInstallPath)
                phony "android" $ need $ map fst libs
                phony "android-tests" $ need $ map snd libs
      , do -- macosx-icecast
            developer <- OSX.getDeveloperPath
            sdkVersion <- OSX.getSystemVersion
            libshout <- pkgConfig "shout"
            let platform = OSX.macOSX sdkVersion
                target = OSX.target (X86 X86_64) platform
                toolChain = applyEnv $ OSX.toolChain_MacOSX target developer
                env = mkEnv target
                liblame = append systemIncludes ["/usr/local/include"]
                        . append libraryPath ["/usr/local/lib"]
                        . append libraries ["mp3lame"]
                buildFlags =   applyConfiguration config configurations
                           >>> commonBuildFlags
                           >>> append userIncludes ["platform/icecast"]
                           >>> libshout
                           >>> liblame
                           >>> stdlib "libc++"
                           >>> append libraries ["c++"]
            return $ do
                lib <- staticLibrary env target toolChain
                            "methcla-icecast"
                          $ methclaSources
                          $ SourceTree.flags buildFlags
                          $ SourceTree.files ["platform/icecast/Methcla/Audio/IO/IcecastDriver.cpp"]
                example <- executable env target toolChain
                            "methcla-icecast-example"
                            $ SourceTree.flags (    apiIncludes
                                                >>> append userIncludes ["examples/thADDeus/src"]
                                                >>> append staticLibraries [lib]
                                                >>> buildFlags )
                            $ SourceTree.files [ "examples/icecast/main.cpp"
                                               , "examples/thADDeus/src/synth.cpp" ]
                phony "macosx-icecast" $ need [lib]
                phony "macosx-icecast-example" $ need [example]
      , do -- macosx-jack
            developer <- OSX.getDeveloperPath
            sdkVersion <- OSX.getSystemVersion
            jackBuildFlags <- pkgConfig "jack"
            let platform = OSX.macOSX sdkVersion
                target = OSX.target (X86 X86_64) platform
                toolChain = applyEnv $ OSX.toolChain_MacOSX target developer
                env = mkEnv target
                buildFlags =   applyConfiguration config configurations
                           >>> commonBuildFlags
                           >>> append userIncludes ["platform/jack"]
                           >>> jackBuildFlags
                           >>> stdlib "libc++"
                           >>> append libraries ["c++", "m"]
                build f = f env target toolChain "methcla-jack"
                            $ SourceTree.flags buildFlags
                            $ methclaSources $
                                SourceTree.files ["platform/jack/Methcla/Audio/IO/JackDriver.cpp"]
            return $ do
                staticLib <- build staticLibrary
                sharedLib <- build sharedLibrary
                phony "macosx-jack" $ need [staticLib]
                phony "macosx-jack-shared" $ need [sharedLib]
      , do -- tests
            (target, toolChain) <- fmap (second applyEnv) getDefaultToolChain
            let env = mkEnv target
                buildFlags =   applyConfiguration config configurations
                           >>> commonBuildFlags
                           >>> append defines [("METHCLA_USE_DUMMY_DRIVER", Nothing)]
                           >>> append systemIncludes [externalLibrary "catch/single_include"]
                           >>> stdlib "libc++"
                           >>> append libraries ["c++", "m"]
                           >>> Pro.testBuildFlags
            return $ do
                result <- executable env target toolChain "methcla-tests"
                            $ SourceTree.flags buildFlags
                            $ methclaSources $ SourceTree.list [
                                SourceTree.files [
                                    "src/Methcla/Audio/IO/DummyDriver.cpp"
                                  , "tests/methcla_tests.cpp"
                                  , "tests/methcla_engine_tests.cpp" ]
                              , Pro.testSources ]
                phony "test" $ do
                    need [result]
                    system' result []
      , do -- tags
            let and_ a b = do { as <- a; bs <- b; return $! as ++ bs }
                files clause dir = find always clause dir
                sources = files (extension ~~? ".h*" ||? extension ~~? ".c*")
                tagFile = "tags"
                tagFiles = "tagfiles"
            return $ do
                tagFile ?=> \output -> flip actionFinally (removeFile tagFiles) $ do
                    fs <- liftIO $ find
                              (fileName /=? "typeof") (extension ==? ".hpp") (boostDir </> "boost")
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
        ]

-- Bump this in order to force a rebuild
buildSystemVersion :: String
buildSystemVersion = "3"

main :: IO ()
main = do
    let shakeOptions' = shakeOptions {
                        shakeFiles = shakeBuildDir ++ "/"
                      , shakeVersion = showVersion Package.version ++ "-shake-" ++ buildSystemVersion
                      , shakeVerbosity = Normal }
        f xs ts = do
            let os = foldl (.) id xs $ defaultOptions
            rules <- mkRules os
            return $ Just $ commonRules >> rules >> want ts
    shakeArgsWith shakeOptions' optionDescrs f

