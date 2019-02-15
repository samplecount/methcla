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
#include <methcla/log.h>
#include <methcla/plugin.h>

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#if defined(__cplusplus)
extern "C" {
#endif

//* Return library version string.
const char* methcla_version();

//* Return true if using the pro version of methcla.
static inline bool methcla_version_is_pro()
{
    return strstr(methcla_version(), "pro") != NULL;
}

//* Common audio driver options.
typedef struct Methcla_AudioDriverOptions Methcla_AudioDriverOptions;

METHCLA_EXPORT Methcla_Error methcla_audio_driver_options_new(
    Methcla_AudioDriverOptions** audio_driver_options);

METHCLA_EXPORT void methcla_audio_driver_options_free(
    Methcla_AudioDriverOptions* audio_driver_options);

METHCLA_EXPORT void methcla_audio_driver_options_set_sample_rate(
    Methcla_AudioDriverOptions* audio_driver_options, size_t sample_rate);

METHCLA_EXPORT void methcla_audio_driver_options_set_buffer_size(
    Methcla_AudioDriverOptions* audio_driver_options, size_t buffer_size);

METHCLA_EXPORT void methcla_audio_driver_options_set_num_inputs(
    Methcla_AudioDriverOptions* audio_driver_options, size_t num_inputs);

METHCLA_EXPORT void methcla_audio_driver_options_set_num_outputs(
    Methcla_AudioDriverOptions* audio_driver_options, size_t num_outputs);

//* Abstract audio driver type.
typedef struct Methcla_AudioDriver Methcla_AudioDriver;

//* Return default audio driver for this platform.
METHCLA_EXPORT Methcla_Error methcla_default_audio_driver(
    const Methcla_AudioDriverOptions* options, Methcla_AudioDriver** outDriver);

//* An integral type for uniquely identifying requests sent to the engine.
typedef int32_t Methcla_RequestId;

enum
{
    //* Request id reserved for asynchronous notifications.
    //  Clients should not use this id when sending requests to the engine.
    kMethcla_Notification = 0
};

//* Callback closure type for handling OSC packets coming from the engine.
//  Packets can be either responses to previously issued requests, or, if
//  request_id is equal to kMethcla_Notification, an asynchronous notification.
typedef struct Methcla_PacketHandler
{
    void* handle;
    void (*handle_packet)(void* handle, Methcla_RequestId request_id,
                          const void* packet, size_t size);
} Methcla_PacketHandler;

typedef struct Methcla_EngineOptions Methcla_EngineOptions;

METHCLA_EXPORT Methcla_Error
               methcla_engine_options_new(Methcla_EngineOptions** engine_options);

METHCLA_EXPORT void
methcla_engine_options_free(Methcla_EngineOptions* engine_options);

METHCLA_EXPORT void
methcla_engine_options_set_log_handler(Methcla_EngineOptions* engine_options,
                                       Methcla_LogHandler     log_handler);

METHCLA_EXPORT void
methcla_engine_options_set_packet_handler(Methcla_EngineOptions* engine_options,
                                          Methcla_PacketHandler packet_handler);

METHCLA_EXPORT void
methcla_engine_options_set_sample_rate(Methcla_EngineOptions* engine_options,
                                       size_t                 sample_rate);

METHCLA_EXPORT void
methcla_engine_options_set_block_size(Methcla_EngineOptions* engine_options,
                                      size_t                 block_size);

METHCLA_EXPORT void methcla_engine_options_set_realtime_memory_size(
    Methcla_EngineOptions* engine_options, size_t realtime_memory_size);

METHCLA_EXPORT void
methcla_engine_options_set_max_num_nodes(Methcla_EngineOptions* engine_options,
                                         size_t                 max_num_nodes);

METHCLA_EXPORT void methcla_engine_options_set_max_num_audio_buses(
    Methcla_EngineOptions* engine_options, size_t max_num_audio_buses);

METHCLA_EXPORT void
methcla_engine_options_set_log_level(Methcla_EngineOptions* engine_options,
                                     Methcla_LogLevel       log_level);

//* NULL terminated array of plugin library functions.
METHCLA_EXPORT void methcla_engine_options_set_plugin_libraries(
    Methcla_EngineOptions*   engine_options,
    Methcla_LibraryFunction* plugin_libraries);

//* NULL terminated array of plugin directories.
METHCLA_EXPORT void methcla_engine_options_set_plugin_directories(
    Methcla_EngineOptions* engine_options,
    const char* const*     plugin_directories);

//* Abstract type for the sound engine.
typedef struct Methcla_Engine Methcla_Engine;

//* Create a new engine with the given options and an audio driver.
METHCLA_EXPORT Methcla_Error methcla_engine_new_with_driver(
    const Methcla_EngineOptions* options, Methcla_AudioDriver* driver,
    Methcla_Engine** engine);

//* Free the resources associated with engine.
//
//  Dereferencing engine after this function returns results in undefined
//  behavior.
METHCLA_EXPORT void methcla_engine_free(Methcla_Engine* engine);

//* Return the last error code.
// METHCLA_EXPORT Methcla_Error methcla_engine_error(const Methcla_Engine*
// engine);

//* Start the engine.
METHCLA_EXPORT Methcla_Error methcla_engine_start(Methcla_Engine* engine);

//* Stop the engine.
METHCLA_EXPORT Methcla_Error methcla_engine_stop(Methcla_Engine* engine);

enum Methcla_EngineLogFlags
{
    kMethcla_EngineLogDefault = 0x00,
    kMethcla_EngineLogRequests = 0x01
};

//* Set flags for debug logging.
METHCLA_EXPORT void
methcla_engine_set_log_flags(Methcla_Engine*             engine,
                             enum Methcla_EngineLogFlags flags);

//* Log a line using the registered log handler.
METHCLA_EXPORT void methcla_engine_log_line(Methcla_Engine*  engine,
                                            Methcla_LogLevel level,
                                            const char*      message);

//* Encode a Methcla_Time value as a 64 bit unsigned integer.
METHCLA_EXPORT uint64_t methcla_time_to_uint64(Methcla_Time time);

//* Decode a Methcla_Time value from a 64 bit unsigned integer.
METHCLA_EXPORT Methcla_Time methcla_time_from_uint64(uint64_t time);

//* Get the current time.
METHCLA_EXPORT Methcla_Time methcla_engine_current_time(Methcla_Engine* engine);

//* Send an OSC packet to the engine.
METHCLA_EXPORT Methcla_Error
               methcla_engine_send(Methcla_Engine* engine, const Methcla_OSCPacket* packet);

//* Open a sound file.
METHCLA_EXPORT Methcla_Error methcla_engine_soundfile_open(
    const Methcla_Engine* engine, const char* path, Methcla_FileMode mode,
    Methcla_SoundFile** file, Methcla_SoundFileInfo* info);

#if defined(__cplusplus)
}
#endif

#endif /* METHCLA_ENGINE_H_INCLUDED */
