/*
    Copyright 2012-2013 Samplecount S.L.

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

        http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.
*/

#ifndef METHCLA_ENGINE_H_INCLUDED
#define METHCLA_ENGINE_H_INCLUDED

#include <methcla/common.h>
#include <methcla/file.h>
#include <stdint.h>
#include <stdlib.h>

#if defined(__cplusplus)
extern "C" {
#endif

//* Return library version string.
const char* methcla_version();

//* An integral type for uniquely identifying requests sent to the engine.
typedef int32_t Methcla_RequestId;

enum
{
    //* Request id reserved for asynchronous notifications.
    //  Clients should not use this id when sending requests to the engine.
    kMethcla_Notification = 0
};

//* Callback function type for handling OSC packets coming from the engine.
//  Packets can be either responses to previously issued requests, or, if request_id is equal to kMethcla_Notification, an asynchronous notification.
typedef void (*Methcla_PacketHandler)(void* handler_data, Methcla_RequestId request_id, const void* packet, size_t size);

//* Abstract type for the sound engine.
typedef struct Methcla_Engine Methcla_Engine;

//* Create a new engine with the given packet handling closure and options.
// @handler Packet handler (may be NULL).
// @handler_data Pointer passed to the packet handler callback.
// @options OSC packet with engine options (may be NULL).
// @engine Output parameter for the newly created engine.
METHCLA_EXPORT Methcla_Error methcla_engine_new(
    Methcla_PacketHandler handler,
    void* handler_data,
    const Methcla_OSCPacket* options,
    Methcla_Engine** engine
    );

//* Free the resources associated with engine.
//
//  Dereferencing engine after this function returns results in undefined behavior.
METHCLA_EXPORT void methcla_engine_free(Methcla_Engine* engine);

//* Return the last error code.
// METHCLA_EXPORT Methcla_Error methcla_engine_error(const Methcla_Engine* engine);

//* Start the engine.
METHCLA_EXPORT Methcla_Error methcla_engine_start(Methcla_Engine* engine);

//* Stop the engine.
METHCLA_EXPORT Methcla_Error methcla_engine_stop(Methcla_Engine* engine);

enum Methcla_EngineLogFlags
{
    kMethcla_EngineLogDefault   = 0x00,
    kMethcla_EngineLogDebug     = 0x01,
    kMethcla_EngineLogRequests  = 0x02
};

//* Set flags for debug logging.
METHCLA_EXPORT void methcla_engine_set_log_flags(Methcla_Engine* engine, Methcla_EngineLogFlags flags);

//* Time in seconds.
typedef double Methcla_Time;

//* Encode a Methcla_Time value as a 64 bit unsigned integer.
METHCLA_EXPORT uint64_t methcla_time_to_uint64(Methcla_Time time);

//* Decode a Methcla_Time value from a 64 bit unsigned integer.
METHCLA_EXPORT Methcla_Time methcla_time_from_uint64(uint64_t time);

//* Get the current time.
METHCLA_EXPORT Methcla_Time methcla_engine_current_time(const Methcla_Engine* engine);

//* Send an OSC packet to the engine.
METHCLA_EXPORT Methcla_Error methcla_engine_send(Methcla_Engine* engine, const void* packet, size_t size);

enum Methcla_NodePlacement
{
    kMethcla_NodePlacementHeadOfGroup,
    kMethcla_NodePlacementTailOfGroup,
    kMethcla_NodePlacementBeforeNode,
    kMethcla_NodePlacementAfterNode
};

enum Methcla_BusMappingFlags
{
    kMethcla_BusMappingInternal = 0x00
  , kMethcla_BusMappingExternal = 0x01
  , kMethcla_BusMappingFeedback = 0x02
  , kMethcla_BusMappingReplace  = 0x04
};

//* Open a sound file.
METHCLA_EXPORT Methcla_Error methcla_engine_soundfile_open(const Methcla_Engine* engine, const char* path, Methcla_FileMode mode, Methcla_SoundFile** file, Methcla_SoundFileInfo* info);

#if defined(__cplusplus)
}
#endif

#endif /* METHCLA_ENGINE_H_INCLUDED */
