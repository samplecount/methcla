-- Copyright 2012-2014 Samplecount S.L.
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
  , mkRules
  , libmethclaPepper
) where

import           Control.Applicative hiding ((*>))
import           Control.Arrow
import           Control.Exception as E
import           Control.Monad
import           Data.Char (toLower)
import           Data.Version (Version(..), showVersion)
import           Development.Shake as Shake
import           Development.Shake.FilePath
import qualified Paths_methcla_stirfile as Package
import           Shakefile.C
import qualified Shakefile.C.Android as Android
import qualified Shakefile.C.BuildFlags as BuildFlags
import qualified Shakefile.C.Host as Host
import qualified Shakefile.C.NaCl as NaCl
import qualified Shakefile.C.OSX as OSX
import qualified Shakefile.C.PkgConfig as PkgConfig
import qualified Shakefile.C.ToolChain as ToolChain
import qualified Shakefile.C.Util as Util
import qualified Shakefile.Config as Config
import           Shakefile.Label
import           System.Console.GetOpt
import           System.Directory hiding (executable)
-- import qualified System.Environment as Env
import           System.FilePath.Find

{-import Debug.Trace-}

-- ====================================================================
-- Utilities

-- lookupEnv :: String -> IO (Maybe String)
-- lookupEnv name = do
--   e <- E.try $ Env.getEnv name :: IO (Either E.SomeException String)
--   case e of
--     Left _ -> return Nothing
--     Right value -> return $ Just value
--
-- getEnv_ :: String -> IO String
-- getEnv_ = fmap (maybe "" id) . lookupEnv

getEnv' :: String -> Action String
getEnv' name =
  getEnvWithDefault
    (error $ "Environment variable " ++ name ++ " is undefined")
    name

isDarwin :: Target -> Bool
isDarwin = isTargetOS (Just "apple") (Just "darwin10")

-- ====================================================================
-- Library variants

data Variant = Default | Pro deriving (Eq, Show)

isPro :: Variant -> Bool
isPro Pro = True
isPro _   = False

version :: Variant -> String
version variant = showVersion $ Package.version {
    versionTags = versionTags Package.version ++ tags
  }
  where tags = if isPro variant then ["pro"] else []

newtype VersionHeader = VersionHeader { versionHeaderPath :: FilePath }

mkVersionHeader :: FilePath -> VersionHeader
mkVersionHeader buildDir = VersionHeader $ buildDir </> "include/Methcla/Version.h"

versionHeaderRule :: Variant -> FilePath -> Rules VersionHeader
versionHeaderRule variant buildDir = do
  let output = mkVersionHeader buildDir
  versionHeaderPath output *> \path -> do
    alwaysRerun
    writeFileChanged path $ unlines [
        "#ifndef METHCLA_VERSION_H_INCLUDED"
      , "#define METHCLA_VERSION_H_INCLUDED"
      , ""
      , "static const char* kMethclaVersion = \"" ++ version variant ++ "\";"
      , ""
      , "#endif /* METHCLA_VERSION_H_INCLUDED */"
      ]
  return output

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

mapTarget :: Monad m => (Arch -> Target) -> [Arch] -> (Target -> m a) -> m [a]
mapTarget mkTarget archs f = mapM (f . mkTarget) archs

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

(>->) :: Monad m => m (a -> b) -> m (b -> c) -> m (a -> c)
(>->) = liftM2 (>>>)

mkObjectsDir :: FilePath -> FilePath
mkObjectsDir path = takeDirectory path </> map tr (takeFileName path) ++ "_obj"
    where tr '.' = '_'
          tr x   = x

configureBuild :: FilePath -> FilePath -> Config -> Rules ()
configureBuild sourceDir buildDir config = do
  (buildDir </> "config/build.cfg") *> \out -> do
    alwaysRerun
    writeFileChanged out $ unlines [
        "la.methc.sourceDir = " ++ sourceDir
      , "la.methc.buildDir = " ++ buildDir
      , "include config/" ++ map toLower (show config) ++ ".cfg"
      ]

withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory dir action = do
  oldwd <- getCurrentDirectory
  setCurrentDirectory dir
  action `finally` setCurrentDirectory oldwd

libmethclaPepper :: Variant -> Config -> FilePath -> FilePath -> Maybe PkgConfig.Options -> Rules (Action (BuildFlags -> BuildFlags))
libmethclaPepper variant config sourceDir buildDir pkgConfigOptions = do
  let target = NaCl.target NaCl.canary
      result = targetBuildPrefix buildDir config target </> "libmethcla.a"
  result *> \_ -> do
    currentShakeOptions <- getShakeOptions
    alwaysRerun
    liftIO $ do
      cwd <- getCurrentDirectory
      let fullBuildDir = cwd </> buildDir
          options = currentShakeOptions {
                        shakeFiles = addTrailingPathSeparator (takeDirectory result)
                      , shakeVersion = version variant
                      , shakeAbbreviations = [
                          (fullBuildDir, buildDir) ]
                      }
      rules <- mkRules variant
                       "."
                       fullBuildDir
                       (set buildConfig config defaultOptions)
                       pkgConfigOptions
      withCurrentDirectory sourceDir $ shake options $ rules >> want [cwd </> result]
  return $ pure ( append systemIncludes [sourceDir </> "include"]
                 . append localLibraries [result])
           -- FIXME: Read this from config file
           >-> PkgConfig.pkgConfigWithOptions (maybe PkgConfig.defaultOptions id pkgConfigOptions) "sndfile"

mkRules :: Variant -> FilePath -> FilePath -> Options -> Maybe PkgConfig.Options -> IO (Rules ())
mkRules variant sourceDir buildDir options pkgConfigOptions = do
  let config = get buildConfig options
      targetBuildPrefix' target = targetBuildPrefix buildDir config target
      targetAlias target = phony (platformString (get targetPlatform target) ++ "-" ++ archString (get targetArch target))
                              . need . (:[])

  hostToolChain <- fmap (second ToolChain.applyEnv) Host.getDefaultToolChain

  return $ do
    -- Common rules
    phony "clean" $ removeFilesAfter buildDir ["//*"]

    _ <- versionHeaderRule variant buildDir

    configureBuild sourceDir buildDir config

    getConfigFrom <- Config.withConfig [buildDir </> "config/build.cfg"]
    let getBuildFlags cfg =
              BuildFlags.fromConfig cfg
          >-> PkgConfig.fromConfigWithOptions
                (maybe PkgConfig.defaultOptions id pkgConfigOptions) cfg
        getSources cfg = do
          need =<< Config.getList cfg [ "Sources.deps"
                                      , if isPro variant
                                        then "Sources.pro.deps"
                                        else "Sources.default.deps"]
          Config.getList cfg [ "Sources"
                             , if isPro variant
                               then "Sources.pro"
                               else "Sources.default"]

    -- iOS
    do
      let iOS_SDK = Version [7,1] []
          getConfig = getConfigFrom $ sourceDir </> "config/ios.cfg"

      let iphoneosPlatform = OSX.iPhoneOS iOS_SDK
      iphoneosLibs <- mapTarget (flip OSX.target iphoneosPlatform) [Arm Armv7, Arm Armv7s] $ \target -> do
          staticLibrary
            (OSX.toolChain <$> liftIO OSX.getDeveloperPath <*> pure target
             >>= ToolChain.applyEnv)
            (targetBuildPrefix' target </> "libmethcla.a")
            (getBuildFlags getConfig)
            (getSources getConfig)
      iphoneosLib <- OSX.universalBinary
                      iphoneosLibs
                      (platformBuildPrefix buildDir config iphoneosPlatform </> "libmethcla.a")
      phony "iphoneos" (need [iphoneosLib])

      let iphonesimulatorPlatform = OSX.iPhoneSimulator iOS_SDK
      iphonesimulatorLibI386 <- do
          let target = OSX.target (X86 I386) iphonesimulatorPlatform
          staticLibrary
            (OSX.toolChain <$> liftIO OSX.getDeveloperPath <*> pure target
             >>= ToolChain.applyEnv)
            (targetBuildPrefix' target </> "libmethcla.a")
            (getBuildFlags getConfig)
            (getSources getConfig)
      let iphonesimulatorLib = platformBuildPrefix buildDir config iphonesimulatorPlatform </> "libmethcla.a"
      iphonesimulatorLib *> copyFile' iphonesimulatorLibI386
      phony "iphonesimulator" (need [iphonesimulatorLib])

      let universalTarget = "iphone-universal"
      universalLib <- OSX.universalBinary
                          (iphoneosLibs ++ [iphonesimulatorLib])
                          (mkBuildPrefix buildDir config universalTarget </> "libmethcla.a")
      phony universalTarget (need [universalLib])
    -- Android
    do
      let getConfig = getConfigFrom $ sourceDir </> "config/android.cfg"
          getConfigTests = getConfigFrom $ sourceDir </> "config/android_tests.cfg"

      libs <- mapTarget (flip Android.target (Android.platform 9)) [Arm Armv5, Arm Armv7] $ \target -> do
        let compiler = (LLVM, Version [3,4] [])
            abi = Android.abiString (get targetArch target)
            ndk = getEnv' "ANDROID_NDK"
            toolChain = Android.toolChain
                          <$> ndk
                          <*> pure compiler
                          <*> pure target
            buildFlags =     getBuildFlags getConfig
                         >-> (Android.libcxx Static <$> ndk <*> pure target)
        libmethcla <- staticLibrary toolChain
                        (targetBuildPrefix' target </> "libmethcla.a")
                        buildFlags
                        (getSources getConfig)

        libmethcla_tests <- sharedLibrary toolChain
                              (targetBuildPrefix' target </> "libmethcla-tests.so")
                              (buildFlags >-> pure (append localLibraries [libmethcla]))
                              (getSources getConfigTests)

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
    -- Pepper/PNaCl
    do
      let target = NaCl.target NaCl.canary
          naclConfig = case config of
                          Debug -> NaCl.Debug
                          Release -> NaCl.Release
          toolChain = NaCl.toolChain
                        <$> getEnv' "NACL_SDK"
                        <*> pure naclConfig
                        <*> pure target
          buildPrefix = targetBuildPrefix' target
          getConfig = getConfigFrom $ sourceDir </> "config/pepper.cfg"
      libmethcla <- staticLibrary toolChain
                      (targetBuildPrefix' target </> "libmethcla.a")
                      (getBuildFlags getConfig)
                      (getSources getConfig)
      phony "pnacl" $ need [libmethcla]

      let getConfigTests = getConfigFrom $ sourceDir </> "config/pepper_tests.cfg"
      pnacl_test_bc <- executable toolChain
                        (buildPrefix </> "methcla-pnacl-tests.bc")
                        (getBuildFlags getConfigTests
                          >-> pure (append localLibraries [libmethcla]))
                        (getSources getConfigTests)
      let pnacl_test = (pnacl_test_bc `replaceExtension` "pexe")
      pnacl_test *> \out -> join $ NaCl.finalize <$> toolChain <*> pure pnacl_test_bc <*> pure out
      let pnacl_test_nmf = pnacl_test `replaceExtension` "nmf"
      pnacl_test_nmf *> NaCl.mk_nmf [(NaCl.PNaCl, pnacl_test)]

      let pnacl_test' = "tests/pnacl" </> takeFileName pnacl_test
      pnacl_test' *> copyFile' pnacl_test
      let pnacl_test_nmf' = "tests/pnacl" </> takeFileName pnacl_test_nmf
      pnacl_test_nmf' *> copyFile' pnacl_test_nmf
      phony "pnacl-test" $ need [pnacl_test', pnacl_test_nmf']

      -- let examplesBuildFlags flags = do
      --       libmethcla <- get_libmethcla
      --       return $ buildFlags >>> libmethcla >>> flags
      --     examplesDir = buildDir </> "examples/pnacl"
      --     mkExample output flags sources files = do
      --       let outputDir = takeDirectory output
      --       bc <- executable toolChain (buildPrefix </> takeFileName output <.> "bc")
      --               $ SourceTree.flagsM (examplesBuildFlags flags)
      --               $ SourceTree.files sources
      --       let pexe = bc `replaceExtension` "pexe"
      --                     `replaceDirectory` (outputDir </> show naclConfig)
      --       pexe *> NaCl.finalize toolChain bc
      --       let nmf = pexe `replaceExtension` "nmf"
      --       nmf *> NaCl.mk_nmf [(NaCl.PNaCl, pexe)]
      --       let allFiles = ["examples/common/common.js"] ++ files
      --           allFiles' = map (`replaceDirectory` outputDir) allFiles
      --       mapM_ (\(old, new) -> new *> copyFile' old) (zip allFiles allFiles')
      --
      --       return $ [pexe, nmf] ++ allFiles'
      --
      -- thaddeus <- mkExample ( examplesDir </> "thaddeus/methcla-thaddeus" )
      --                       ( append userIncludes [ "examples/thADDeus/src" ] )
      --                       [
      --                         "examples/thADDeus/pnacl/main.cpp"
      --                       , "examples/thADDeus/src/synth.cpp"
      --                       ]
      --                       [
      --                         "examples/thADDeus/pnacl/index.html"
      --                       , "examples/thADDeus/pnacl/manifest.json"
      --                       , "examples/thADDeus/pnacl/thaddeus.js"
      --                       ]
      -- sampler <- mkExample  ( examplesDir </> "sampler/methcla-sampler" )
      --                       ( append userIncludes   [ "examples/sampler/src" ]
      --                       . append systemIncludes [ "examples/sampler/libs/tinydir" ]
      --                       . NaCl.libnacl_io )
      --                       [
      --                         "examples/sampler/pnacl/main.cpp"
      --                       , "examples/sampler/src/Engine.cpp"
      --                       ]
      --                       [
      --                         "examples/sampler/pnacl/index.html"
      --                       , "examples/sampler/pnacl/manifest.json"
      --                       , "examples/sampler/pnacl/example.js"
      --                       ]
      --
      -- let freesounds = map (takeFileName.takeDirectory) [
      --         "http://freesound.org/people/adejabor/sounds/157965/"
      --       , "http://freesound.org/people/NOISE.INC/sounds/45394/"
      --       , "http://freesound.org/people/ThePriest909/sounds/209331/"
      --       ]
      --
      -- (examplesDir </> "sampler/sounds/*") *> \output -> do
      --   freesoundApiKey <- getEnv'
      --                       "FREESOUND_API_KEY"
      --   cmd "curl" [    "http://www.freesound.org/api/sounds/"
      --                ++ dropExtension (takeFileName output)
      --                ++ "/serve?api_key=" ++ freesoundApiKey
      --              , "-L", "-o", output ] :: Action ()
      --
      -- -- Install warp-static web server if needed
      -- let server = ".cabal-sandbox/bin/warp"
      --
      -- server *> \_ -> do
      --   cmd "cabal sandbox init" :: Action ()
      --   cmd "cabal install -j warp-static" :: Action ()
      --
      -- phony "pnacl-examples" $ do
      --   need [server]
      --   need thaddeus
      --   need $ sampler ++ map (combine (examplesDir </> "sampler/sounds"))
      --                         freesounds
      --   cmd (Cwd examplesDir) server :: Action ()
    -- Desktop
    do
      let (target, toolChain) = hostToolChain
          getConfig = getConfigFrom $ sourceDir </> "config/desktop.cfg"
          build f ext =
            f toolChain (targetBuildPrefix' target </> "libmethcla" <.> ext)
              (getBuildFlags getConfig)
              (getSources getConfig)
      staticLib <- build staticLibrary "a"
      sharedLib <- build sharedLibrary Host.sharedLibraryExtension

      -- Quick hack for setting install path of shared library
      let installedSharedLib = joinPath $ ["install"] ++ tail (splitPath sharedLib)
      phony installedSharedLib $
        if isDarwin target
        then do
          need [sharedLib]
          command_ [] "install_name_tool"
                      ["-id", "@executable_path/../Resources/libmethcla.dylib", sharedLib]
        else return ()

      phony "desktop" $ need [staticLib, installedSharedLib]
    -- tests
    do
      let (target, toolChain) = hostToolChain
          getConfig = getConfigFrom $ sourceDir </> "config/host_tests.cfg"
      result <- executable toolChain
                  (targetBuildPrefix' target </> "methcla-tests" <.> Host.executableExtension)
                  (getBuildFlags getConfig)
                  (getSources getConfig)
      phony "host-tests" $ need [result]
      phony "test" $ do
          need [result]
          command_ [] result []
      phony "clean-test" $ removeFilesAfter "tests/output" ["*.osc", "*.wav"]
    --tags
    do
      let and_ a b = do { as <- a; bs <- b; return $! as ++ bs }
          files clause dir = find always clause dir
          sources = files (extension ~~? ".h*" ||? extension ~~? ".c*")
          tagFile = "tags"
          tagFiles = "tagfiles"
      tagFile ?=> \output -> flip actionFinally (removeFile tagFiles) $ do
          fs <- liftIO $ find
                    (fileName /=? "typeof") (extension ==? ".hpp") ("external_libraries/boost/boost")
              `and_` sources "include"
              `and_` sources "platform"
              `and_` sources "plugins"
              `and_` sources "src"
          need fs
          writeFileLines tagFiles fs
          command_ [] "ctags" $
              (words "--sort=foldcase --c++-kinds=+p --fields=+iaS --extra=+q --tag-relative=yes")
           ++ ["-f", output]
           ++ ["-L", tagFiles]
