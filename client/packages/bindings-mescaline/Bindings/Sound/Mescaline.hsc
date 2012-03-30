#include <bindings.dsl.h>
#include <Mescaline/API.h>

module Bindings.Sound.Mescaline where

import Data.Word (Word32)

#strict_import

#opaque_t Mescaline_Engine

-- #cinline kDefaultWorldOptions , IO (Ptr <struct WorldOptions>)

-- #opaque_t struct ReplyAddress

-- #callback ReplyFunc , Ptr <ReplyAddress> -> Ptr CChar -> CInt -> IO ()

-- | Access the data pointer in reply address.
-- #ccall ReplyAddress_ReplyData       , Ptr <ReplyAddress> -> IO (Ptr ())

-- | Create a new world.
#ccall Mescaline_Engine_new             , IO (Ptr <Mescaline_Engine>)
#ccall Mescaline_Engine_free            , Ptr <Mescaline_Engine> -> IO ()
#ccall Mescaline_Engine_start           , Ptr <Mescaline_Engine> -> IO ()
#ccall Mescaline_Engine_stop            , Ptr <Mescaline_Engine> -> IO ()

#integral_t LV2_URID

#starttype LV2_Atom
  #field size , Word32
  #field type , Word32
#stoptype

#ccall Mescaline_Engine_mapUri          , Ptr <Mescaline_Engine> -> Ptr CChar -> IO <LV2_URID>
#ccall Mescaline_Engine_unmapUri        , Ptr <Mescaline_Engine> -> <LV2_URID> -> IO (Ptr CChar)

-- #callback ReplyFunc , Ptr <ReplyAddress> -> Ptr CChar -> CInt -> IO ()

#callback Mescaline_HandleResponse      , Ptr <LV2_Atom> -> Ptr () -> IO ()
#ccall Mescaline_Engine_request         , Ptr <Mescaline_Engine> -> Ptr <LV2_Atom> -> <Mescaline_HandleResponse> -> Ptr () -> IO ()

-- #ccall World_NonRealTimeSynthesis   , Ptr <World> -> Ptr <WorldOptions> -> IO ()
-- #ccall World_OpenUDP                , Ptr <World> -> CInt -> IO CInt
-- #ccall World_OpenTCP                , Ptr <World> -> CInt -> CInt -> CInt -> IO CInt
-- #ccall World_WaitForQuit            , Ptr <World> -> IO ()
-- #ccall World_SendPacket             , Ptr <World> -> CInt -> Ptr CChar -> <ReplyFunc> -> IO Bool
-- #ccall World_SendPacketWithContext  , Ptr <World> -> CInt -> Ptr CChar -> <ReplyFunc> -> Ptr () -> IO Bool

-- | Haskell print function.
-- #callback HaskellPrintFunc , Ptr CChar -> IO ()

-- | Set the global Haskell print function.
-- #ccall SetHaskellPrintFunc , <HaskellPrintFunc> -> IO ()
