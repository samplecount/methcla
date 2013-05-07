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
import           Development.Shake as Shake
import           Development.Shake.FilePath
import           Shakefile.C
import           Shakefile.C.OSX
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

lv2Dir :: FilePath
lv2Dir = externalLibrary "lv2"

serdDir :: FilePath
serdDir = externalLibrary "serd"

serdBuildFlags :: CBuildFlags -> CBuildFlags
serdBuildFlags = append userIncludes
                    [ serdDir, serdDir </> "src"
                    , externalLibraries ]

sordDir :: FilePath
sordDir = externalLibrary "sord"

sordBuildFlags :: CBuildFlags -> CBuildFlags
sordBuildFlags = append userIncludes
                    [ sordDir, sordDir </> "src"
                    , serdDir
                    , externalLibraries ]

sratomDir :: FilePath
sratomDir = externalLibrary "sratom"

sratomBuildFlags :: CBuildFlags -> CBuildFlags
sratomBuildFlags = append userIncludes
                    [ sratomDir
                    , serdDir
                    , sordDir
                    , externalLibraries
                    , lv2Dir ]

lilvDir :: FilePath
lilvDir = externalLibrary "lilv"

lilvBuildFlags :: CBuildFlags -> CBuildFlags
lilvBuildFlags = append userIncludes
                    [ lilvDir, lilvDir </> "src"
                    , serdDir
                    , sordDir
                    , sratomDir
                    , externalLibraries
                    , lv2Dir ]

boostDir :: FilePath
boostDir = externalLibrary "boost"

boostBuildFlags :: CBuildFlags -> CBuildFlags
boostBuildFlags = append systemIncludes [ boostDir ]

tlsfDir :: FilePath
tlsfDir = externalLibrary "tlsf"

engineBuildFlags :: Platform -> CBuildFlags -> CBuildFlags
engineBuildFlags platform =
    append userIncludes
      ( -- Library headers
        [ ".", "src" ]
        -- Platform specific modules
     ++ (if      platform == iPhoneOS then[ "platform/ios" ]
         else if platform == iPhoneSimulator then [ "platform/ios" ]
         else if platform == macOSX then [ "platform/jack" ]
         else [])
        -- LV2 libraries
     ++ [ "external_libraries", "external_libraries/lv2" ]
     ++ [ serdDir, sordDir, lilvDir ] )
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
    (Just C, flag "-std=c11")
  , (Just Cpp, flag "-std=c++11" ++ flag "-stdlib=libc++")
  , (Nothing, flag "-Wall")
  , (Nothing, flag "-fvisibility=hidden")
  , (Just Cpp, flag "-fvisibility-inlines-hidden")
  ]

-- | Build flags for static library
staticBuidFlags :: CBuildFlags -> CBuildFlags
staticBuidFlags = id

-- | Build flags for shared library
sharedBuildFlags :: CBuildFlags -> CBuildFlags
sharedBuildFlags = append libraries [ "c++", "m" ]

methclaLib :: Platform -> Library
methclaLib platform =
    Library "methcla" $ sourceFlags commonBuildFlags [
        -- serd
        sourceTree_ serdBuildFlags $ sourceFiles $
            under (serdDir </> "src") [
                "env.c"
              , "node.c"
              , "reader.c"
              , "string.c"
              , "uri.c"
              , "writer.c"
              ]
        -- sord
      , sourceTree_ sordBuildFlags $ sourceFiles $
            under (sordDir </> "src") [
                "sord.c"
              , "syntax.c"
              , "zix/digest.c"
              , "zix/hash.c"
              , "zix/tree.c"
              ]
        -- sratom
      , sourceTree_ sratomBuildFlags $ sourceFiles $
            under (sratomDir </> "src") [
                "sratom.c"
              ]
        -- lilv
      , sourceTree_ lilvBuildFlags $ sourceFiles $
            under (lilvDir </> "src") [
                "collections.c"
              , "instance.c"
              , "lib.c"
              , "node.c"
              , "plugin.c"
              , "pluginclass.c"
              , "port.c"
              , "query.c"
              , "scalepoint.c"
              , "state.c"
              , "ui.c"
              , "util.c"
              , "world.c"
              -- FIXME: Make sure during compilation that this is actually the same as sord/src/zix/tree.c?
              --, "zix/tree.c"
              ]
        -- boost
      , sourceTree_ boostBuildFlags $ sourceFiles $
            under (boostDir </> "libs") [
                "date_time/src/gregorian/date_generators.cpp"
              , "date_time/src/gregorian/greg_month.cpp"
              , "date_time/src/gregorian/greg_weekday.cpp"
              , "date_time/src/gregorian/gregorian_types.cpp"
              , "date_time/src/posix_time/posix_time_types.cpp"
              , "exception/src/clone_current_exception_non_intrusive.cpp"
              , "system/src/error_code.cpp"
              ]
        -- TLSF
      , sourceTree_ id $ sourceFiles $ [
            tlsfDir </> "tlsf.c" ]
        -- engine
      , sourceTree_ (engineBuildFlags platform) $ sourceFiles $
            under "src" [
                "Methcla/API.cpp"
              , "Methcla/Audio/AudioBus.cpp"
              , "Methcla/Audio/Engine.cpp"
              , "Methcla/Audio/Group.cpp"
              , "Methcla/Audio/IO/Driver.cpp"
              , "Methcla/Audio/Node.cpp"
              , "Methcla/Audio/Resource.cpp"
              , "Methcla/Audio/Synth.cpp"
              , "Methcla/Audio/SynthDef.cpp"
              , "Methcla/LV2/URIDMap.cpp"
              , "Methcla/Memory/Manager.cpp"
              , "Methcla/Memory.cpp"
              , "Methcla/Plugin/Loader.cpp"
              ]
            ++ [ "external_libraries/zix/ring.c" ]
            -- plugins
            ++ [ "lv2/methc.la/plugins/sine.lv2/sine.c" ]
            -- platform dependent
            ++ (if platform `elem` [iPhoneOS, iPhoneSimulator]
                then under "platform/ios" [ "Methcla/Audio/IO/RemoteIODriver.cpp" ]
                else if platform == macOSX
                     then under "platform/jack" [ "Methcla/Audio/IO/JackDriver.cpp" ]
                     else [])
        ]

plugins :: Platform -> [Library]
plugins platform = [
    -- TODO: Provide more SourceTree combinators
    Library "sine" $ sourceTree_ (engineBuildFlags platform) $ sourceFiles [ "lv2/methc.la/plugins/sine.lv2/sine.c" ]
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
        append compilerFlags [(Nothing, flag "-O2")]
      . append defines [("NDEBUG", Nothing)]
    )
  , ( Debug,
        append compilerFlags [(Nothing, flag "-O0" ++ flag "-gdwarf-2")]
    )
  ]

-- ====================================================================
-- Commandline targets

iOS_SDK :: SDKVersion
iOS_SDK = SDKVersion "6.1"

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
    fmap sequence_ $ sequence [
        do
            return $ phony "clean" $ removeFilesAfter shakeBuildDir ["//*"]
      , do -- iphone
            developer <- liftIO getDeveloperPath
            return $ do
                iphoneosLib <- do
                    let platform = iPhoneOS
                        cTarget = mkCTarget platform Armv7
                        toolChain = cToolChain_IOS developer
                        env = mkEnv cTarget
                        buildFlags = applyConfiguration config configurations
                                   . staticBuidFlags
                                   $ cBuildFlags_IOS developer iOS_SDK
                    lib <- staticLibrary env cTarget toolChain buildFlags (methclaLib platform)
                    platformAlias platform lib
                    return lib
                iphonesimulatorLib <- do
                    let platform = iPhoneSimulator
                        cTarget = mkCTarget platform I386
                        toolChain = cToolChain_IOS_Simulator developer
                        env = mkEnv cTarget
                        buildFlags = applyConfiguration config configurations
                                   . staticBuidFlags
                                   $ cBuildFlags_IOS_Simulator developer iOS_SDK
                    lib <- staticLibrary env cTarget toolChain buildFlags (methclaLib platform)
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

      , do -- macosx
            developer <- liftIO getDeveloperPath
            sdkVersion <- liftIO getSystemVersion
            jackBuildFlags <- liftIO $ pkgConfig "jack"
            let platform = macOSX
                cTarget = mkCTarget platform X86_64
                toolChain = cToolChain_MacOSX developer
                env = mkEnv cTarget
                buildFlags = applyConfiguration config configurations
                           . jackBuildFlags
                           . sharedBuildFlags
                           $ cBuildFlags_MacOSX developer sdkVersion
            return $ sharedLibrary env cTarget toolChain buildFlags (methclaLib platform)
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
                        `and` files (extension ==? ".h") (lilvDir </> "lilv")
                        `and` files (extension ==? ".h") (lv2Dir </> "lv2")
                        `and` files (extension ==? ".h") (serdDir </> "serd")
                        `and` files (extension ==? ".h") (sordDir </> "sord")
                        `and` sources "include"
                        `and` sources "lv2"
                        `and` sources "platform"
                        `and` sources "src"
                    need fs
                    writeFileLines tagFiles fs
                    system' "ctags" $
                        (words "--sort=foldcase --c++-kinds=+p --fields=+iaS --extra=+q --tag-relative=yes")
                     ++ flag_ "-f" output
                     ++ flag_ "-L" tagFiles
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

