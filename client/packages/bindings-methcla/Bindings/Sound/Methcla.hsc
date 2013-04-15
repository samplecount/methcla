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

#include <bindings.dsl.h>
#include <methcla/engine.h>

module Bindings.Sound.Methcla where

import Data.Word (Word32)

#strict_import

-- | Abstract engine handle.
#opaque_t Methcla_Engine

#starttype Methcla_Option
#field key , Ptr CChar
#field value , Ptr ()
#stoptype

#integral_t enum Methcla_Error

#ccall methcla_engine_error             , Ptr <Methcla_Engine> -> IO <Methcla_Error>

#ccall methcla_engine_error_message     , Ptr <Methcla_Engine> -> IO (Ptr CChar)

#integral_t Methcla_RequestId
#num kMethcla_Notification

-- | Response callback function.
#callback Methcla_PacketHandler         , Ptr () -> <Methcla_RequestId> -> Ptr () -> CSize -> IO ()

-- | Create new engine.
#ccall methcla_engine_new               , <Methcla_PacketHandler> -> Ptr () -> IO (Ptr <Methcla_Engine>)

-- | Free engine.
#ccall methcla_engine_free              , Ptr <Methcla_Engine> -> IO ()

-- | Start engine.
#ccall methcla_engine_start             , Ptr <Methcla_Engine> -> IO ()

-- | Stop engine.
#ccall methcla_engine_stop              , Ptr <Methcla_Engine> -> IO ()

-- | Send request.
#ccall methcla_engine_send              , Ptr <Methcla_Engine> -> Ptr () -> CSize -> IO ()

