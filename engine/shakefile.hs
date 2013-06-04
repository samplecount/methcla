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

import           Control.Lens hiding (Action, (<.>), under)
import           Data.Char (toLower)
import           Data.Version (Version(..))
import           Development.Shake as Shake
import           Development.Shake.FilePath
import           Shakefile.C
import qualified Shakefile.C.Android as Android
import           Shakefile.C.OSX as OSX
import           Shakefile.C.PkgConfig (pkgConfig)
import           Shakefile.Configuration
import           Shakefile.Lens
import           System.Console.GetOpt
import           System.Directory (removeFile)
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

boostBuildFlags :: CBuildFlags -> CBuildFlags
boostBuildFlags = append systemIncludes [ boostDir ]

tlsfDir :: FilePath
tlsfDir = externalLibrary "tlsf"

engineBuildFlags :: CBuildFlags -> CBuildFlags
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
      ++ [ "external_libraries/oscpp" ]
         -- TLSF
      ++ [ tlsfDir ] )

-- | Build flags common to all targets
commonBuildFlags :: CBuildFlags -> CBuildFlags
commonBuildFlags = append compilerFlags [
    (Just C, ["-std=c11"])
  , (Just Cpp, ["-std=c++11"])
  , (Nothing, ["-Wall", "-Wextra"])
  , (Just Cpp, ["-fvisibility-inlines-hidden"])
  , (Nothing, ["-fstrict-aliasing", "-Wstrict-aliasing"])
  , (Nothing, ["-fstack-protector", "-Wstack-protector"])
  ]

-- | Build flags for static library
staticBuildFlags :: CBuildFlags -> CBuildFlags
staticBuildFlags = id

-- | Build flags for shared library
sharedBuildFlags :: CBuildFlags -> CBuildFlags
sharedBuildFlags = append libraries ["c++", "m"]

-- | Build flags for building with clang.
clangBuildFlags :: String -> CBuildFlags -> CBuildFlags
clangBuildFlags libcpp = append compilerFlags [(Just Cpp, ["-stdlib="++libcpp])]

pluginSources :: [FilePath]
pluginSources = [
    "plugins/methc.la/plugins/disksampler/disksampler.cpp"
  , "plugins/methc.la/plugins/sampler/sampler.cpp"
  , "plugins/methc.la/plugins/sine/sine.c" ]

-- vectorBuildFlags :: CBuildFlags -> CBuildFlags
-- vectorBuildFlags = append compilerFlags [
--     (Nothing, [ "-O3", "-save-temps" ])
--   ]

methclaLib :: SourceTree CBuildFlags -> Library
methclaLib platformSources =
    Library "methcla" $ sourceFlags commonBuildFlags [
        sourceFlags boostBuildFlags [ sourceFiles_ $
          under (boostDir </> "libs") [
            --   "date_time/src/gregorian/date_generators.cpp"
            -- , "date_time/src/gregorian/greg_month.cpp"
            -- , "date_time/src/gregorian/greg_weekday.cpp"
            -- , "date_time/src/gregorian/gregorian_types.cpp"
            -- , "date_time/src/posix_time/posix_time_types.cpp"
            -- , "exception/src/clone_current_exception_non_intrusive.cpp"
            "system/src/error_code.cpp"
            ]
        ]
        -- TLSF
      , sourceFiles_ [ tlsfDir </> "tlsf.c" ]
        -- engine
      , sourceFlags engineBuildFlags [
          -- sourceFlags (append compilerFlags [ (Nothing, [ "-O0" ]) ])
          --   [ sourceFiles_ [ "src/Methcla/Audio/Engine.cpp" ] ],
          sourceFiles_ $
            [ "src/Methcla/API.cpp"
            , "src/Methcla/Audio/AudioBus.cpp"
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
            -- ++ [ "external_libraries/zix/ring.c" ] -- Unused ATM
            -- plugins
            ++ pluginSources
          -- platform dependent
        , platformSources
      -- , sourceTree_ (vectorBuildFlags . engineBuildFlags) $ sourceFiles $
      --     under "src" [ "Methcla/Audio/DSP.c" ]
        ]
      ]

plugins :: Platform -> [Library]
plugins platform = [
    Library "sine" $ sourceFlags engineBuildFlags [ sourceFiles_ [ "plugins/methc.la/plugins/sine/sine.c" ] ]
  ]

-- ====================================================================
-- Configurations

data Config = Debug | Release deriving (Eq, Show)

parseConfig :: String -> Either String Config
parseConfig x =
    case map toLower x of
        "debug" -> Right Debug
        "release" -> Right Release
        _ -> Left $ "Invalid configuration `" ++ x ++ "'"

configurations :: [Configuration Config CBuildFlags]
configurations = [
    ( Release,
        append compilerFlags [
          (Nothing, ["-O1", "-gdwarf-2", "-fvisibility=hidden"])
        , (Nothing, ["-Wno-unused-parameter"])
        ]
      . append defines [("NDEBUG", Nothing)]
    )
  , ( Debug,
        append compilerFlags [(Nothing, ["-O0", "-gdwarf-2"])]
      . append defines [("DEBUG", Just "1")]
    )
  ]

-- ====================================================================
-- Commandline targets

shakeBuildDir :: String
shakeBuildDir = "build"

mkBuildPrefix :: CTarget -> Config -> FilePath
mkBuildPrefix cTarget config =
      shakeBuildDir
  </> map toLower (show config)
  </> (platformString $ cTarget ^. targetPlatform)
  </> (archString $ cTarget ^. targetArch)

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

mkRules :: Options -> IO (Rules ())
mkRules options = do
    let config = options ^. buildConfig
        mkEnv cTarget = set buildPrefix
                            (mkBuildPrefix cTarget config)
                            defaultEnv
        platformAlias p = phony (platformString p) . need . (:[])
    applyEnv <- toolChainFromEnvironment
    fmap sequence_ $ sequence [
        do
            return $ phony "clean" $ removeFilesAfter shakeBuildDir ["//*"]
      , do -- iphone
            let iOS_SDK = Version [6,1] []
            developer <- liftIO getDeveloperPath
            return $ do
                iphoneosLib <- do
                    let platform = iPhoneOS iOS_SDK
                        cTarget = OSX.target (Arm Armv7) platform
                        toolChain = applyEnv $ cToolChain_IOS developer
                        env = mkEnv cTarget
                        buildFlags = applyConfiguration config configurations
                                   . append userIncludes ["platform/ios"]
                                   . clangBuildFlags "libc++"
                                   . staticBuildFlags
                                   $ cBuildFlags_IOS cTarget developer
                    lib <- staticLibrary env cTarget toolChain buildFlags
                            (methclaLib (sourceFiles_ ["platform/ios/Methcla/Audio/IO/RemoteIODriver.cpp"]))
                    platformAlias platform lib
                    return lib
                iphonesimulatorLib <- do
                    let platform = iPhoneSimulator iOS_SDK
                        cTarget = OSX.target (X86 I386) platform
                        toolChain = applyEnv $ cToolChain_IOS_Simulator developer
                        env = mkEnv cTarget
                        buildFlags = applyConfiguration config configurations
                                   . append userIncludes ["platform/ios"]
                                   . clangBuildFlags "libc++"
                                   . staticBuildFlags
                                   $ cBuildFlags_IOS_Simulator cTarget developer
                    lib <- staticLibrary env cTarget toolChain buildFlags
                            (methclaLib (sourceFiles_ ["platform/ios/Methcla/Audio/IO/RemoteIODriver.cpp"]))
                    platformAlias platform lib
                    return lib
                let universalTarget = "iphone-universal"
                universalLib <- universalBinary
                                    [iphoneosLib, iphonesimulatorLib]
                                    (shakeBuildDir
                                      </> map toLower (show config)
                                      </> universalTarget
                                      </> "libmethcla.a")
                phony universalTarget (need [universalLib])
      , do -- android
          return $ do
            let platform = Android.platform 9
                cTarget = Android.target (Arm Armv5) platform
                toolChainPath = "/Users/skersten/dev/android-toolchain-arm-linux-androideabi-4.7-9"
                toolChain = applyEnv $ Android.standaloneToolChain toolChainPath cTarget
                env = mkEnv cTarget
                buildFlags = applyConfiguration config configurations
                           . append userIncludes ["platform/android"]
                           . staticBuildFlags
                           . append compilerFlags [ (Nothing, ["-fpic"])
                                                  , (Just Cpp, ["-frtti", "-fexceptions"]) ]
                           $ Android.buildFlags cTarget
            lib <- staticLibrary env cTarget toolChain buildFlags
                    (methclaLib (sourceFiles_ [
                      "platform/android/opensl_io.c",
                      "platform/android/Methcla/Audio/IO/OpenSLESDriver.cpp" ]))
            platformAlias platform lib
      , do -- macosx
            developer <- liftIO getDeveloperPath
            sdkVersion <- liftIO getSystemVersion
            jackBuildFlags <- liftIO $ pkgConfig "jack"
            let platform = macOSX sdkVersion
                cTarget = OSX.target (X86 X86_64) platform
                toolChain = applyEnv $ cToolChain_MacOSX developer
                env = mkEnv cTarget
                buildFlags = applyConfiguration config configurations
                           . append userIncludes ["platform/jack"]
                           . jackBuildFlags
                           . clangBuildFlags "libc++"
                           . sharedBuildFlags
                           $ cBuildFlags_MacOSX cTarget developer
            return $ sharedLibrary env cTarget toolChain buildFlags
                        (methclaLib (sourceFiles_ ["platform/jack/Methcla/Audio/IO/JackDriver.cpp"]))
                        >>= platformAlias platform
      , do -- tags
            let and a b = do { as <- a; bs <- b; return $! as ++ bs }
                files clause dir = find always clause dir
                sources = files (extension ~~? ".h*" ||? extension ~~? ".c*")
                tagFile = "tags"
                tagFiles = "tagfiles"
            return $ do
                tagFile ?=> \output -> flip actionFinally (removeFile tagFiles) $ do
                    fs <- liftIO $ find
                              (fileName /=? "typeof") (extension ==? ".hpp") (boostDir </> "boost")
                        `and` sources "include"
                        `and` sources "platform"
                        `and` sources "plugins"
                        `and` sources "src"
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

