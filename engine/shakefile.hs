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

import           Control.Arrow ((>>>))
import           Control.Lens hiding (Action, (<.>), under)
import           Data.Char (toLower)
import           Data.Version (Version(..))
import           Development.Shake as Shake
import           Development.Shake.FilePath
import           Shakefile.C
import qualified Shakefile.C.Android as Android
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
  , "external_libraries/oscpp/include"
  , "plugins" ]

engineBuildFlags :: BuildFlags -> BuildFlags
engineBuildFlags =
    append userIncludes
      ( -- Library headers
        [ "src" ]
        -- External libs and plugins
     ++ [ "external_libraries", "plugins" ] )
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
    append userIncludes [ "plugins" ]
  . append systemIncludes [ "include", externalLibrary "oscpp/include" ]
  . append compilerFlags [(Nothing, ["-O3"])]

pluginSources :: [FilePath]
pluginSources = [
    "plugins/methc.la/plugins/disksampler/disksampler.cpp"
  , "plugins/methc.la/plugins/sampler/sampler.cpp"
  , "plugins/methc.la/plugins/sine/sine.c" ]

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
                [ "src/Methcla/API.cpp"
                , "src/Methcla/Audio/AudioBus.cpp"
                , "src/Methcla/Audio/Engine.cpp"
                , "src/Methcla/Audio/Group.cpp"
                , "src/Methcla/Audio/IO/Driver.cpp"
                , "src/Methcla/Audio/IO/DummyDriver.cpp"
                , "src/Methcla/Audio/Node.cpp"
                -- , "src/Methcla/Audio/Resource.cpp"
                , "src/Methcla/Audio/Synth.cpp"
                , "src/Methcla/Audio/SynthDef.cpp"
                , "src/Methcla/Memory/Manager.cpp"
                , "src/Methcla/Memory.cpp"
                , "src/Methcla/Plugin/Loader.cpp"
                , "src/Methcla/Utility/Semaphore.cpp"
                ]
                -- ++ [ "external_libraries/zix/ring.c" ] -- Unused ATM
              -- platform dependent
            , platformSources
          -- , sourceTree_ (vectorBuildFlags . engineBuildFlags) $ sourceFiles $
          --     under "src" [ "Methcla/Audio/DSP.c" ]
        ]
      , SourceTree.flags pluginBuildFlags $ SourceTree.files pluginSources
      ]

methcla :: String
methcla = "methcla"

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

mkRules :: Options -> IO (Rules ())
mkRules options = do
    let config = options ^. buildConfig
        mkEnv target = set buildPrefix
                            (defaultBuildPrefix target (show config))
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
            developer <- liftIO OSX.getDeveloperPath
            return $ do
                iphoneosLib <- do
                    let platform = OSX.iPhoneOS iOS_SDK
                        cTarget = OSX.target (Arm Armv7) platform
                        toolChain = applyEnv $ OSX.toolChain_IOS developer
                        env = mkEnv cTarget
                        buildFlags =   applyConfiguration config configurations
                                   >>> commonBuildFlags
                                   >>> append userIncludes ["platform/ios"]
                                   >>> stdlib "libc++"
                                   $   OSX.buildFlags_IOS cTarget developer
                    lib <- staticLibrary env cTarget toolChain buildFlags
                            methcla (methclaSources $
                                        SourceTree.files ["platform/ios/Methcla/Audio/IO/RemoteIODriver.cpp"])
                    platformAlias platform lib
                    return lib
                iphonesimulatorLib <- do
                    let platform = OSX.iPhoneSimulator iOS_SDK
                        cTarget = OSX.target (X86 I386) platform
                        toolChain = applyEnv $ OSX.toolChain_IOS_Simulator developer
                        env = mkEnv cTarget
                        buildFlags =   applyConfiguration config configurations
                                   >>> commonBuildFlags
                                   >>> append userIncludes ["platform/ios"]
                                   >>> stdlib "libc++"
                                   $   OSX.buildFlags_IOS_Simulator cTarget developer
                    lib <- staticLibrary env cTarget toolChain buildFlags
                            methcla (methclaSources $
                                        SourceTree.files ["platform/ios/Methcla/Audio/IO/RemoteIODriver.cpp"])
                    platformAlias platform lib
                    return lib
                let universalTarget = "iphone-universal"
                universalLib <- OSX.universalBinary
                                    [iphoneosLib, iphonesimulatorLib]
                                    (shakeBuildDir
                                      </> map toLower (show config)
                                      </> universalTarget
                                      </> "libmethcla.a")
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
                                   $   Android.buildFlags ndk target
                    libmethcla <- staticLibrary (mkEnv target) target toolChain buildFlags
                            methcla (methclaSources $
                                        SourceTree.files [
                                          "platform/android/opensl_io.c",
                                          "platform/android/Methcla/Audio/IO/OpenSLESDriver.cpp" ])
                    let testBuildFlags =
                                       engineBuildFlags
                                   >>> append systemIncludes [externalLibrary "catch/single_include"]
                                   >>> append linkerFlags ["-Wl,-soname,libmethcla-tests.so"]
                                   >>> append staticLibraries [libmethcla]
                                   >>> append libraries ["android", "log", "OpenSLES"]
                                   $   buildFlags
                    libmethcla_tests <- sharedLibrary (mkEnv target) target toolChain testBuildFlags
                                  "methcla-tests"
                                  -- TODO: Build static library for native_app_glue to link against.
                                  (SourceTree.append (Android.native_app_glue ndk)
                                                     (SourceTree.files [ "tests/methcla_tests.cpp"
                                                                       , "tests/android/main.cpp" ]))

                    let installPath = "libs/android" </> abi </> takeFileName libmethcla
                    installPath ?=> \_ -> copyFile' libmethcla installPath
                    targetAlias target installPath

                    let testInstallPath = "tests/android/libs" </> Android.abiString (target ^. targetArch) </> takeFileName libmethcla_tests
                    testInstallPath ?=> \_ -> copyFile' libmethcla_tests testInstallPath
                    return (installPath, testInstallPath)
                phony "android" $ need $ map fst libs
                phony "android-tests" $ need $ map snd libs
      , do -- macosx-icecast
            developer <- liftIO OSX.getDeveloperPath
            sdkVersion <- liftIO OSX.getSystemVersion
            libshout <- liftIO $ pkgConfig "shout"
            let platform = OSX.macOSX sdkVersion
                cTarget = OSX.target (X86 X86_64) platform
                toolChain = applyEnv $ OSX.toolChain_MacOSX developer
                env = mkEnv cTarget
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
                           $   OSX.buildFlags_MacOSX cTarget developer
            return $ do
                lib <- staticLibrary env cTarget toolChain buildFlags
                            "methcla-icecast"
                          $ methclaSources
                          $ SourceTree.files ["platform/icecast/Methcla/Audio/IO/IcecastDriver.cpp"]
                example <- executable env cTarget toolChain
                            (  apiIncludes
                             . append userIncludes ["examples/thADDeus/src"]
                             . append staticLibraries [lib]
                             $ buildFlags)
                            "methcla-icecast-example"
                            (SourceTree.files [ "examples/icecast/main.cpp"
                                              , "examples/thADDeus/src/synth.cpp" ])
                phony "macosx-icecast" $ need [lib]
                phony "macosx-icecast-example" $ need [example]
      , do -- macosx-jack
            developer <- liftIO OSX.getDeveloperPath
            sdkVersion <- liftIO OSX.getSystemVersion
            jackBuildFlags <- liftIO $ pkgConfig "jack"
            let platform = OSX.macOSX sdkVersion
                cTarget = OSX.target (X86 X86_64) platform
                toolChain = applyEnv $ OSX.toolChain_MacOSX developer
                env = mkEnv cTarget
                buildFlags =   applyConfiguration config configurations
                           >>> commonBuildFlags
                           >>> append userIncludes ["platform/jack"]
                           >>> jackBuildFlags
                           >>> stdlib "libc++"
                           >>> append libraries ["c++", "m"]
                           $   OSX.buildFlags_MacOSX cTarget developer
            return $ do
                result <- sharedLibrary env cTarget toolChain buildFlags
                            "methcla-jack" (methclaSources $
                                    SourceTree.files ["platform/jack/Methcla/Audio/IO/JackDriver.cpp"])
                phony "macosx-jack" $ need [result]
      , do -- tests (macosx)
            developer <- liftIO OSX.getDeveloperPath
            sdkVersion <- liftIO OSX.getSystemVersion
            let platform = OSX.macOSX sdkVersion
                cTarget = OSX.target (X86 I386) platform
                toolChain = applyEnv $ OSX.toolChain_MacOSX developer
                env = mkEnv cTarget
                buildFlags =   applyConfiguration config configurations
                           >>> commonBuildFlags
                           >>> append defines [("METHCLA_USE_DUMMY_DRIVER", Nothing)]
                           >>> append systemIncludes [externalLibrary "catch/single_include"]
                           >>> stdlib "libc++"
                           >>> append libraries ["c++", "m"]
                           $   OSX.buildFlags_MacOSX cTarget developer
            return $ do
                result <- executable env cTarget toolChain buildFlags
                            "methcla-tests"
                            (methclaSources $ SourceTree.files ["tests/methcla_tests.cpp"])
                phony "macosx-tests" $ do
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

main :: IO ()
main = do
    let shakeOptions' = shakeOptions {
                        shakeFiles = shakeBuildDir ++ "/"
                      , shakeVerbosity = Normal }
        f xs ts = do
            let os = foldl (.) id xs $ defaultOptions
            rules <- mkRules os
            return $ Just $ rules >> want ts
    shakeArgsWith shakeOptions' optionDescrs f

