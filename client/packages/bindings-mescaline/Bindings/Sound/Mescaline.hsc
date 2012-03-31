#include <bindings.dsl.h>
#include <Mescaline/API.h>

module Bindings.Sound.Mescaline where

import Data.Word (Word32)

#strict_import

-- | Abstract engine handle.
#opaque_t Mescaline_Engine

-- #cinline kDefaultWorldOptions , IO (Ptr <struct WorldOptions>)

-- | Create new engine.
#ccall Mescaline_Engine_new             , IO (Ptr <Mescaline_Engine>)

-- | Free engine.
#ccall Mescaline_Engine_free            , Ptr <Mescaline_Engine> -> IO ()

-- | Start engine.
#ccall Mescaline_Engine_start           , Ptr <Mescaline_Engine> -> IO ()

-- | Stop engine.
#ccall Mescaline_Engine_stop            , Ptr <Mescaline_Engine> -> IO ()

-- | LV2 mapped URI.
#integral_t LV2_URID

-- | LV2 atom structure.
#starttype LV2_Atom
  #field size , Word32
  #field type , Word32
#stoptype

-- | Map URI.
#ccall Mescaline_Engine_mapUri          , Ptr <Mescaline_Engine> -> Ptr CChar -> IO <LV2_URID>

-- | Unmap mapped URI.
#ccall Mescaline_Engine_unmapUri        , Ptr <Mescaline_Engine> -> <LV2_URID> -> IO (Ptr CChar)

-- | Response callback function.
#callback Mescaline_HandleResponse      , Ptr <LV2_Atom> -> Ptr () -> IO ()

-- | Send request.
#ccall Mescaline_Engine_request         , Ptr <Mescaline_Engine> -> Ptr <LV2_Atom> -> <Mescaline_HandleResponse> -> Ptr () -> IO ()

-- | Haskell print function.
-- #callback HaskellPrintFunc , Ptr CChar -> IO ()

-- | Set the global Haskell print function.
-- #ccall SetHaskellPrintFunc , <HaskellPrintFunc> -> IO ()
