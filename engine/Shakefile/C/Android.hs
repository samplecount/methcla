module Shakefile.C.Android where

import           Control.Applicative ((<$>))
import           Control.Lens hiding ((<.>))
import           Development.Shake as Shake
import           Development.Shake.FilePath
import           Data.List (intercalate)
import           Data.List.Split (splitOn)
import           Shakefile.C
import           Shakefile.Lens (append, prepend)
import           System.Process (readProcess)
import qualified System.Info as System

standaloneToolChain :: FilePath -> String -> CToolChain
standaloneToolChain path name =
    prefix .~ Just path
  $ compilerCmd .~ mkTool "gcc"
  $ archiverCmd .~ mkTool "ar"
  -- $ archiver .~ osxArchiver
  $ linkerCmd .~ mkTool "g++"
  -- $ linker .~ osxLinker
  -- $ linkResultFileName .~ osxLinkResultFileName 
  $ defaultCToolChain
  where mkTool x = name ++ "-" ++ x

androidArchString :: Arch -> String
androidArchString arch =
  case arch of
    I386 -> "x86"
    Armv7 -> "armv7-a"
    _     -> archString arch

buildFlags :: CTarget -> CBuildFlags
buildFlags target =
    append compilerFlags [(Nothing, ["-march="++arch, "-mfloat-abi=softfp", "-mfpu=neon"])]
  . append linkerFlags ["-march="++arch, "-Wl,--fix-cortex-a8"]
  . append archiverFlags ["-rs"]
  $ defaultCBuildFlags
  where arch = androidArchString $ target ^. targetArch

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
