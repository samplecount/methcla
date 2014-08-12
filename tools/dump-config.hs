#!/usr/bin/env runhaskell

import           Control.Monad
import qualified Data.HashMap.Strict as Map
import           Data.List.Split (splitOn)
import           Data.Monoid
import qualified Development.Shake.Config as Config
import           System.Environment
import           System.Directory

import qualified Development.Shake.Language.C.BuildFlags as BuildFlags

toPair :: [a] -> Maybe (a, a)
toPair [a1,a2] = Just (a1, a2)
toPair _ = Nothing

main :: IO ()
main = do
  (file:args) <- getArgs
  isPro <- doesFileExist "pro/LICENSE"
  let env = [ ("la.methc.sourceDir", ".")
            , ("la.methc.buildDir", "./build")
            , ("la.methc.buildConfig", "debug")
            , ("la.methc.variantDir", if isPro then "pro/config" else "config/default") ]
            ++ [ x | Just x <- map (toPair . splitOn "=") args ]
  cfg <- Map.toList `fmap` Config.readConfigFileWithEnv env file
  let maxKey = maximum . fmap (length . fst) $ cfg
  forM_ cfg $ \(k,v) -> do
      putStrLn $ show k ++ replicate (4 + maxKey - length k) ' ' ++ show v

  flags <- BuildFlags.fromConfig (return . flip lookup cfg)
  print $ flags mempty
