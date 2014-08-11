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

module Shake_Methcla (
    Variant(..)
  , Options
  , buildConfig
  , defaultOptions
  , optionDescrs
  , version
  , Config(..)
  , mkRules
  , LibraryTarget(..)
  , libmethcla
) where

import           Control.Applicative hiding ((*>))
import           Control.Arrow
import           Control.Exception as E
import           Control.Monad
import           Data.Char (toLower)
-- import           Data.Monoid
import           Data.Version (Version(..), showVersion)
import           Development.Shake as Shake
import           Development.Shake.FilePath
import           Development.Shake.Language.C
import qualified Development.Shake.Language.C.BuildFlags as BuildFlags
import qualified Development.Shake.Language.C.Host as Host
import           Development.Shake.Language.C.Label
import qualified Development.Shake.Language.C.PkgConfig as PkgConfig
import qualified Development.Shake.Language.C.Target.Android as Android
import qualified Development.Shake.Language.C.Target.NaCl as NaCl
import qualified Development.Shake.Language.C.Target.OSX as OSX
import qualified Development.Shake.Language.C.Config as Config
-- import           Development.Shake.Language.C.Label
-- import qualified Paths_methcla_stirfile as Package
import           System.Console.GetOpt
import           System.Directory hiding (executable)
-- import qualified System.Environment as Env
-- import           System.FilePath.Find

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

-- ====================================================================
-- Library variants

data Variant = Default | Pro deriving (Eq, Show)

isPro :: Variant -> Bool
isPro Pro = True
isPro _   = False

version :: Variant -> String
version variant = showVersion (Version [0,3,0] tags)-- $ Package.version {
  --   versionTags = versionTags Package.version ++ tags
  -- }
  where tags = if isPro variant then ["pro"] else []

newtype VersionHeader = VersionHeader { versionHeaderPath :: FilePath }

mkVersionHeader :: FilePath -> VersionHeader
mkVersionHeader buildDir = VersionHeader $ buildDir </> "include/Methcla/Version.h"

versionHeaderRule :: Variant -> FilePath -> FilePath -> Rules VersionHeader
versionHeaderRule variant sourceDir buildDir = do
  let output = mkVersionHeader buildDir
  versionHeaderPath output *> \path -> do
    version <- head <$> readFileLines (sourceDir </> "VERSION")
    let tag = if isPro variant then "-pro" else ""
    writeFileChanged path $ unlines [
        "#ifndef METHCLA_VERSION_H_INCLUDED"
      , "#define METHCLA_VERSION_H_INCLUDED"
      , ""
      , "static const char* kMethclaVersion = \"" ++ version ++ tag ++ "\";"
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

-- configureBuild :: FilePath -> FilePath -> Config -> Rules ()
-- configureBuild sourceDir buildDir config = do
--   ("build/methcla-build.cfg") *> \out -> do
--     alwaysRerun
--     writeFileChanged out $ unlines [
--         "la.methc.sourceDir = " ++ sourceDir
--       , "la.methc.buildDir = " ++ buildDir
--       , "include config/" ++ map toLower (show config) ++ ".cfg"
--       ]

withCurrentDirectory :: FilePath -> IO a -> IO a
-- withCurrentDirectory dir action = do
--   oldwd <- getCurrentDirectory
--   setCurrentDirectory dir
--   action `finally` setCurrentDirectory oldwd
withCurrentDirectory _ action = action

data LibraryTarget =
    Lib_Pepper
  | Lib_Android Arch
  | Lib_iOS
  deriving (Eq, Show)

-- selectLibraryFlags :: (BuildFlags -> BuildFlags) -> (BuildFlags -> BuildFlags)
-- selectLibraryFlags f =
--   let b = f mempty
--   in   set libraries (get libraries b)
--      . set linkerFlags (get linkerFlags b)
--      $ mempty

libmethcla :: LibraryTarget -> Variant -> Config -> FilePath -> FilePath -> Maybe PkgConfig.Options -> Rules (FilePath, Action (BuildFlags -> BuildFlags))
libmethcla libTarget variant config sourceDir buildDir pkgConfigOptions = do
  let (result, exportFlags) =
        case libTarget of
          Lib_Pepper ->
            ( targetBuildPrefix buildDir config NaCl.target </> "libmethcla.a"
            , -- FIXME: Read this from config file
              PkgConfig.pkgConfigWithOptions (maybe PkgConfig.defaultOptions id pkgConfigOptions) "sndfile" )
          Lib_iOS ->
            ( mkBuildPrefix buildDir config "iphone-universal" </> "libmethcla.a"
            , return id )
          Lib_Android arch ->
            ( mkBuildPrefix buildDir config "android"
              </> Android.abiString arch
              </> "libmethcla.a"
            , -- FIXME: Read this from config file
              PkgConfig.pkgConfigWithOptions (maybe PkgConfig.defaultOptions id pkgConfigOptions) "sndfile" )
          -- _ -> error $ "Library target " ++ show libTarget ++ " not supported yet"
  result *> \_ -> do
    currentShakeOptions <- getShakeOptions
    alwaysRerun
    liftIO $ do
      cwd <- getCurrentDirectory
      let fullBuildDir = cwd </> buildDir
          options = currentShakeOptions {
                        shakeFiles = addTrailingPathSeparator fullBuildDir
                      , shakeVersion = version variant
                      , shakeAbbreviations = [
                          (fullBuildDir, buildDir) ]
                      , shakeReport = map (combine fullBuildDir . takeFileName)
                                          (shakeReport currentShakeOptions)
                      }
          rules = mkRules variant
                     ""
                     fullBuildDir
                     (set buildConfig config defaultOptions)
                     pkgConfigOptions
      withCurrentDirectory sourceDir $ shake options $
        rules >> want [cwd </> result]
  return (result, pure ( append systemIncludes [sourceDir </> "include"]
                        . append localLibraries [result])
                  >>>= exportFlags)

mkRules :: Variant -> FilePath -> FilePath -> Options -> Maybe PkgConfig.Options -> Rules ()
mkRules variant sourceDir buildDir options pkgConfigOptions = do
  let config = get buildConfig options
      targetBuildPrefix' target = targetBuildPrefix buildDir config target

  -- Common rules
  phony "clean" $ removeFilesAfter buildDir ["//*"]

  _ <- versionHeaderRule variant sourceDir buildDir

  -- configureBuild sourceDir buildDir config

  let configEnv = [
          ("la.methc.sourceDir", sourceDir)
        , ("la.methc.buildDir", buildDir)
        , ("la.methc.buildConfig", map toLower (show config)) ]

  getConfigFrom <- Config.withConfig [] >>= \f -> return $ f configEnv

  let getBuildFlags cfg =
             BuildFlags.fromConfig cfg
        >>>= PkgConfig.fromConfigWithOptions
              (maybe PkgConfig.defaultOptions id pkgConfigOptions) cfg
      getSources cfg = do
        need =<< Config.getPaths cfg [ "Sources.deps"
                                     , if isPro variant
                                       then "Sources.pro.deps"
                                       else "Sources.default.deps"]
        Config.getPaths cfg [ "Sources"
                            , if isPro variant
                              then "Sources.pro"
                              else "Sources.default"]

  -- iOS
  do
    let sdkVersion platform = maximum <$> OSX.getPlatformVersions platform
        getConfig = getConfigFrom $ sourceDir </> "config/ios.cfg"

    iphoneosLibs <- mapTarget (OSX.target OSX.iPhoneOS) [Arm Armv7, Arm Armv7s] $ \target -> do
        staticLibrary
          (OSX.toolChain
            <$> OSX.getSDKRoot
            <*> sdkVersion OSX.iPhoneOS
            <*> pure target
           >>= applyEnv)
          (targetBuildPrefix' target </> "libmethcla.a")
          (getBuildFlags getConfig)
          (getSources getConfig)

    let iphoneosLib = platformBuildPrefix buildDir config OSX.iPhoneOS </> "libmethcla.a"
    iphoneosLib *> OSX.universalBinary iphoneosLibs
    phony "iphoneos" (need [iphoneosLib])

    iphonesimulatorLibI386 <- do
        let target = OSX.target OSX.iPhoneSimulator (X86 I386)
        staticLibrary
          (OSX.toolChain
            <$> OSX.getSDKRoot
            <*> sdkVersion OSX.iPhoneSimulator
            <*> pure target
           >>= applyEnv)
          (targetBuildPrefix' target </> "libmethcla.a")
          (getBuildFlags getConfig)
          (getSources getConfig)
    let iphonesimulatorLib = platformBuildPrefix buildDir config OSX.iPhoneSimulator </> "libmethcla.a"
    iphonesimulatorLib *> copyFile' iphonesimulatorLibI386
    phony "iphonesimulator" (need [iphonesimulatorLib])

    let universalTarget = "iphone-universal"
        universalLib = mkBuildPrefix buildDir config universalTarget </> "libmethcla.a"
    universalLib *> OSX.universalBinary (iphoneosLibs ++ [iphonesimulatorLib])
    phony universalTarget (need [universalLib])
  -- Android
  do
    let getConfig = getConfigFrom $ sourceDir </> "config/android.cfg"
        getConfigTests = getConfigFrom $ sourceDir </> "config/android_tests.cfg"

    libs <- mapTarget Android.target [Arm Armv5, Arm Armv7] $ \target -> do
      let compiler = (LLVM, Version [3,4] [])
          abi = Android.abiString (targetArch target)
          ndk = getEnv' "ANDROID_NDK"
          toolChain = Android.toolChain
                        <$> ndk
                        <*> pure (Android.sdkVersion 9)
                        <*> pure compiler
                        <*> pure target
          buildFlags =      getBuildFlags getConfig
                       >>>= (Android.libcxx Static <$> ndk <*> pure target)
      libmethcla <- staticLibrary toolChain
                      (targetBuildPrefix' target </> "libmethcla.a")
                      buildFlags
                      (getSources getConfig)

      libmethcla_tests <- sharedLibrary toolChain
                            (targetBuildPrefix' target </> "libmethcla-tests.so")
                            (buildFlags >>>= pure (append localLibraries [libmethcla]))
                            (getSources getConfigTests)

      let installPath = mkBuildPrefix buildDir config "android"
                          </> abi
                          </> takeFileName libmethcla
      installPath *> copyFile' libmethcla

      let testInstallPath = "tests/android/libs" </> abi </> takeFileName libmethcla_tests
      testInstallPath *> copyFile' libmethcla_tests
      return (installPath, testInstallPath)
    phony "android" $ need $ map fst libs
    phony "android-tests" $ need $ map snd libs
  -- Pepper/PNaCl
  do
    let target = NaCl.target
        naclConfig = case config of
                        Debug -> NaCl.Debug
                        Release -> NaCl.Release
        toolChain = NaCl.toolChain
                      <$> getEnv' "NACL_SDK"
                      <*> pure NaCl.canary
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
                        >>>= pure (append localLibraries [libmethcla]))
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
    let (target, toolChain) = second ((=<<) applyEnv) Host.defaultToolChain
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
      case targetOS target of
        OSX -> do
          need [sharedLib]
          command_ [] "install_name_tool"
                      ["-id", "@executable_path/../Resources/libmethcla.dylib", sharedLib]
        _ -> return ()

    phony "desktop" $ need [staticLib, installedSharedLib]
  -- tests
  do
    let (target, toolChain) = second ((=<<) applyEnv) Host.defaultToolChain
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
  -- do
  --   let and_ a b = do { as <- a; bs <- b; return $! as ++ bs }
  --       files clause dir = find always clause dir
  --       sources = files (extension ~~? ".h*" ||? extension ~~? ".c*")
  --       tagFile = "tags"
  --       tagFiles = "tagfiles"
  --   tagFile *> \output -> flip actionFinally (removeFile tagFiles) $ do
  --       fs <- liftIO $ find
  --                 (fileName /=? "typeof") (extension ==? ".hpp") ("external_libraries/boost/boost")
  --           `and_` sources "include"
  --           `and_` sources "platform"
  --           `and_` sources "plugins"
  --           `and_` sources "src"
  --       need fs
  --       writeFileLines tagFiles fs
  --       command_ [] "ctags" $
  --           (words "--sort=foldcase --c++-kinds=+p --fields=+iaS --extra=+q --tag-relative=yes")
  --        ++ ["-f", output]
  --        ++ ["-L", tagFiles]
