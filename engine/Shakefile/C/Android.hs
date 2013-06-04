module Shakefile.C.Android where

import           Control.Applicative ((<$>))
import           Control.Lens hiding ((<.>))
import           Development.Shake as Shake
import           Development.Shake.FilePath
import           Data.List (intercalate)
import           Data.List.Split (splitOn)
import           Data.Version (Version(..))
import           Shakefile.C
import           Shakefile.Lens (append, prepend)
import           System.Process (readProcess)
import qualified System.Info as System

platform :: Int -> Platform
platform apiVersion = Platform "android" (Version [apiVersion] [])

target :: Arch -> Platform -> CTarget
target arch = mkCTarget arch "linux" "androideabi"

standaloneToolChain :: FilePath -> CTarget -> CToolChain
standaloneToolChain path target =
    prefix .~ Just path
  $ compilerCmd .~ mkTool "gcc"
  $ archiverCmd .~ mkTool "ar"
  -- $ archiver .~ osxArchiver
  $ linkerCmd .~ mkTool "g++"
  -- $ linker .~ osxLinker
  -- $ linkResultFileName .~ osxLinkResultFileName 
  $ defaultCToolChain
  where mkTool x = targetString target ++ "-" ++ x

androidArchString :: Arch -> String
androidArchString (Arm Armv5) = "armv5te"
androidArchString (Arm Armv6) = "armv5te"
androidArchString (Arm Armv7) = "armv7-a"
androidArchString arch = archString arch

archCompilerFlags :: Arch -> [(Maybe CLanguage, [String])]
archCompilerFlags (Arm Armv5) = [(Nothing, ["-mfloat-abi=soft"])]
archCompilerFlags (Arm Armv6) = [(Nothing, ["-mfloat-abi=soft"])]
archCompilerFlags (Arm Armv7) = [(Nothing, ["-mfloat-abi=softfp", "-mfpu=neon"])]
archCompilerFlags _ = []

archLinkerFlags :: Arch -> [String]
archLinkerFlags (Arm Armv7) = ["-Wl,--fix-cortex-a8"]
archLinkerFlags _ = []

buildFlags :: CTarget -> CBuildFlags
buildFlags target =
    append compilerFlags ([(Nothing, march)] ++ archCompilerFlags arch)
  . append linkerFlags (march ++ archLinkerFlags arch)
  . append archiverFlags ["-rs"]
  $ defaultCBuildFlags
  where
    arch = target ^. targetArch
    march = ["-march=" ++ androidArchString arch]

-- toolChain :: FilePath -> String -> Int -> CToolChain
-- toolChain ndkRoot name apiLevel =
--     prefix .~ Just (ndkRoot </> "toolchains" </> name </> "prebuilt" </> System.os ++ "-" ++ System.arch)
--   $ compilerCmd .~ "clang"
--   $ archiverCmd .~ "libtool"
--   $ archiver .~ osxArchiver
--   $ linkerCmd .~ "clang++"
--   $ linker .~ osxLinker
--   $ linkResultFileName .~ osxLinkResultFileName 
--   $ defaultCToolChain
