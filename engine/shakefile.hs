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

import           Control.Lens hiding (Action, (<.>), under)
import           Control.Monad
import           Development.Shake as Shake
import           Development.Shake.FilePath
import           Shakefile.C
import           System.Console.CmdArgs.Explicit
import qualified System.Directory as Dir
import           System.FilePath.Find
import           System.IO

import Debug.Trace

-- ====================================================================
-- Library

externalLibraries :: FilePath
externalLibraries = "external_libraries"

externalLibrary :: FilePath -> FilePath
externalLibrary = combine externalLibraries

lv2Dir :: FilePath
lv2Dir = externalLibrary "lv2"

boostDir :: FilePath
boostDir = externalLibrary "boost"

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

boostBuildFlags :: CBuildFlags -> CBuildFlags
boostBuildFlags = append systemIncludes [ boostDir ]

engineBuildFlags :: CTarget -> CBuildFlags -> CBuildFlags
engineBuildFlags target =
    append userIncludes
      ( -- Library headers
        [ ".", "src" ]
        -- Platform specific modules
     ++ case buildTarget ^$ target of
            IOS           -> [ "platform/ios" ]
            IOS_Simulator -> [ "platform/ios" ]
            MacOSX        -> [ "platform/jack" ]
        -- LV2 libraries
     ++ [ "external_libraries", "external_libraries/lv2" ]
     ++ [ serdDir, sordDir, lilvDir ] )
  . append systemIncludes
       ( -- API headers
         [ "include" ]
         -- Boost
      ++ [ boostDir
         , "external_libraries/boost_lockfree" ] )

-- | Build flags common to all targets
methclaCommonBuildFlags :: CBuildFlags -> CBuildFlags
methclaCommonBuildFlags = append compilerFlags [
    (Just C, flag "-std=c11")
  , (Just Cpp, flag "-std=c++11" ++ flag "-stdlib=libc++")
  , (Nothing, flag "-Wall")
  , (Nothing, flag "-fvisibility=hidden")
  , (Just Cpp, flag "-fvisibility-inlines-hidden")
  ]

-- | Build flags for static library
methclaStaticBuidFlags :: CBuildFlags -> CBuildFlags
methclaStaticBuidFlags = id

-- | Build flags for shared library
methclaSharedBuildFlags :: CBuildFlags -> CBuildFlags
methclaSharedBuildFlags = libraries .~ [ "m" ]

methclaLib :: CTarget -> IO Library
methclaLib target = do
    --boostSrc <- find always
    --                (    extension ==? ".cpp"
    --                 &&? (not . isSuffixOf "win32") <$> directory
    --                 &&? (not . isSuffixOf "test/src") <$> directory
    --                 -- FIXME: linking `filesystem' into a shared library fails with:
    --                 --     Undefined symbols for architecture x86_64:
    --                 --       "vtable for boost::filesystem::detail::utf8_codecvt_facet", referenced from:
    --                 --           boost::filesystem::detail::utf8_codecvt_facet::utf8_codecvt_facet(unsigned long) in v2_path.cpp.o
    --                 --           boost::filesystem::detail::utf8_codecvt_facet::utf8_codecvt_facet(unsigned long) in path.cpp.o
    --                 --       NOTE: a missing vtable usually means the first non-inline virtual member function has no definition.
    --                 &&? (not . elem "filesystem/" . splitPath) <$> filePath
    --                 &&? (not . elem "thread/" . splitPath) <$> filePath
    --                 &&? (fileName /=? "utf8_codecvt_facet.cpp")
    --                )
    --                boostDir
    return $ Library "methcla" $ [
        -- serd
        sourceTree serdBuildFlags $ sourceFiles $
            under (serdDir </> "src") [
                "env.c"
              , "node.c"
              , "reader.c"
              , "string.c"
              , "uri.c"
              , "writer.c"
              ]
        -- sord
      , sourceTree sordBuildFlags $ sourceFiles $
            under (sordDir </> "src") [
                "sord.c"
              , "syntax.c"
              , "zix/digest.c"
              , "zix/hash.c"
              , "zix/tree.c"
              ]
        -- sratom
      , sourceTree sratomBuildFlags $ sourceFiles $
            under (sratomDir </> "src") [
                "sratom.c"
              ]
        -- lilv
      , sourceTree lilvBuildFlags $ sourceFiles $
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
      , sourceTree boostBuildFlags $ sourceFiles $
            under (boostDir </> "libs") [
                "date_time/src/gregorian/date_generators.cpp"
              , "date_time/src/gregorian/greg_month.cpp"
              , "date_time/src/gregorian/greg_weekday.cpp"
              , "date_time/src/gregorian/gregorian_types.cpp"
              , "date_time/src/posix_time/posix_time_types.cpp"
              , "exception/src/clone_current_exception_non_intrusive.cpp"
              , "filesystem/src/codecvt_error_category.cpp"
              , "filesystem/src/operations.cpp"
              , "filesystem/src/path.cpp"
              , "filesystem/src/path_traits.cpp"
              , "filesystem/src/portability.cpp"
              , "filesystem/src/unique_path.cpp"
              --, "filesystem/src/utf8_codecvt_facet.cpp"
              , "filesystem/src/windows_file_codecvt.cpp"
              , "system/src/error_code.cpp"
              ]
        -- engine
      , sourceTree (engineBuildFlags target) $ sourceFiles $
            under "src" [
                "Methcla/API.cpp"
              , "Methcla/Audio/AudioBus.cpp"
              , "Methcla/Audio/Client.cpp"
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
            ++ [ "lv2/methc.la/plugins/sine.lv2/sine.cpp" ]
            -- platform dependent
            ++ (if (buildTarget ^$ target) `elem` [IOS, IOS_Simulator]
                then under "platform/ios" [ "Methcla/Audio/IO/RemoteIODriver.cpp" ]
                else if (buildTarget ^$ target) `elem` [MacOSX]
                     then under "platform/jack" [ "Methcla/Audio/IO/JackDriver.cpp" ]
                     else [])
        ]

-- ====================================================================
-- Configurations

applyBuildConfiguration :: Env -> [Configuration] -> CBuildFlags -> CBuildFlags
applyBuildConfiguration env = applyConfiguration (buildConfiguration ^$ env)

configurations :: [Configuration]
configurations = [
    ( "release",
        append compilerFlags [(Nothing, flag "-O2")]
      . append defines [("NDEBUG", Nothing)]
    )
  , ( "debug",
        append compilerFlags [(Nothing, flag "-O0" ++ flag "-gdwarf-2")]
    )
  ]

-- ====================================================================
-- Commandline targets

iOS_SDK :: String
iOS_SDK = "6.1"

maybeRemoveDirectoryRecursive :: FilePath -> IO ()
maybeRemoveDirectoryRecursive d =
	Dir.doesDirectoryExist d >>= flip when (Dir.removeDirectoryRecursive d)

targetSpecs :: [(String, (Rules () -> IO ()) -> Env -> IO ())]
targetSpecs = [
    ( "clean", const (maybeRemoveDirectoryRecursive . flip (^.) buildPrefix) )
  , ( "ios",
    \shake env -> do
        developer <- getDeveloperPath
        let target = mkCTarget IOS "armv7"
            toolChain = cToolChain_IOS developer
            buildFlags = applyBuildConfiguration env configurations
                       . methclaStaticBuidFlags
                       . methclaCommonBuildFlags
                       $ cBuildFlags_IOS developer iOS_SDK
        libmethcla <- methclaLib target
        shake $ do
            let libs = [ libmethcla ]
                lib = staticLibrary env target toolChain buildFlags
                libFile = libBuildPath env target
            want =<< mapM lib libs
    )
  , ( "ios-simulator",
    \shake env -> do
        developer <- getDeveloperPath
        let target = mkCTarget IOS_Simulator "i386"
            toolChain = cToolChain_IOS_Simulator developer
            buildFlags = applyBuildConfiguration env configurations
                       . methclaStaticBuidFlags
                       . methclaCommonBuildFlags
                       $ cBuildFlags_IOS_Simulator developer iOS_SDK
        libmethcla <- methclaLib target
        shake $ do
            let libs = [ libmethcla ]
                lib = staticLibrary env target toolChain buildFlags
                libFile = libBuildPath env target
            want =<< mapM lib libs
    )
  , ( "macosx",
    \shake env -> do
        developer <- getDeveloperPath
        sdkVersion <- getSystemVersion
        jackBuildFlags <- pkgConfig "jack"
        let target = mkCTarget MacOSX "x86_64"
            toolChain = cToolChain_MacOSX developer
            buildFlags = applyBuildConfiguration env configurations
                       . jackBuildFlags
                       . methclaSharedBuildFlags
                       . methclaCommonBuildFlags
                       $ cBuildFlags_MacOSX developer sdkVersion
        libmethcla <- methclaLib target
        shake $ do
            let libs = [ libmethcla ]
                lib = sharedLibrary env target toolChain buildFlags
                libFile = libBuildPath env target
            want =<< mapM lib libs
    )
  , ( "tags",
    \shake env -> do
        let and a b = do { as <- a; bs <- b; return $! as ++ bs }
            files clause dir = find always clause dir
            sourceFiles = files (extension ~~? ".h*" ||? extension ~~? ".c*")
            tagFile = "../tags"
            tagFiles = "../tagfiles"
        shake $ do
            tagFile ?=> \output -> do
                fs <- liftIO $
                    find (fileName /=? "typeof") (extension ==? ".hpp") (boostDir </> "boost")
                        `and`
                    files (extension ==? ".hpp") (externalLibraries </> "boost_lockfree")
                        `and`
                    files (extension ==? ".h") (lilvDir </> "lilv")
                        `and`
                    files (extension ==? ".h") (lv2Dir </> "lv2")
                        `and`
                    files (extension ==? ".h") (serdDir </> "serd")
                        `and`
                    files (extension ==? ".h") (sordDir </> "sord")
                        `and`
                    sourceFiles "lv2" `and` sourceFiles "platform" `and` sourceFiles "src"
                need fs
                writeFileLines tagFiles fs
                systemLoud "ctags" $
                    (words "--sort=foldcase --c++-kinds=+p --fields=+iaS --extra=+q --tag-relative=yes")
                 ++ flag_ "-f" output
                 ++ flag_ "-L" tagFiles
                -- FIXME: How to use bracket in the Action monad?
                liftIO $ Dir.removeFile tagFiles
            want [ tagFile ]
    )
  ]

processTargets :: (Rules () -> IO ()) -> Env -> [String] -> IO ()
processTargets shake env = mapM_ processTarget where
    processTarget t =
        case lookup t targetSpecs of
            Nothing -> putStrLn $ "Warning: Target " ++ t ++ " not found"
            Just f -> f shake env

setLineBuffering :: IO ()
setLineBuffering = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering

main :: IO ()
main = do
    let args = arguments (map fst configurations) (map fst targetSpecs)
    opts <- processArgs args
    let shakeIt = shake (optionsToShake opts)
        env = mkEnv (configuration ^$ opts) (output ^$ opts)
    if help ^$ opts
        then print $ helpText [] HelpFormatDefault args
        else case targets ^$ opts of
                [] -> do
                    -- TODO: integrate this with option processing
                    putStrLn $ "Please chose a target:"
                    mapM_ (putStrLn . ("\t"++) . fst) targetSpecs
                ts -> setLineBuffering >> processTargets shakeIt env ts
