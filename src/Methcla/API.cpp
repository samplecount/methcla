// Copyright 2012-2013 Samplecount S.L.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

#include "Methcla/API.hpp"
#include "Methcla/Audio/Engine.hpp"
#include "Methcla/Audio/IO/Driver.hpp"
#include "Methcla/Audio/SynthDef.hpp"
#include "Methcla/Exception.hpp"
#include "Methcla/Platform.hpp"
#include "Methcla/Version.h"

#include <methcla/engine.h>
#include <methcla/plugin.h>
#include <methcla/plugins/sine.h>

#include <cstdlib>
#include <functional>
#include <iostream>
#include <new>
#include <stdexcept>
#include <string>

#include <oscpp/server.hpp>

struct Methcla_AudioDriver
{
public:
    Methcla_AudioDriver(Methcla::Audio::IO::Driver* driver)
    : m_driver(driver)
    {}

    ~Methcla_AudioDriver()
    {
        delete m_driver;
    }

    Methcla::Audio::IO::Driver* driver()
    {
        return m_driver;
    }

private:
    Methcla::Audio::IO::Driver* m_driver;
};

Methcla_AudioDriver*
Methcla::API::wrapAudioDriver(Methcla::Audio::IO::Driver* driver)
{
    return new Methcla_AudioDriver(driver);
}

Methcla::Audio::Environment::Options
Methcla::API::convertOptions(const Methcla_EngineOptions* options)
{
    Methcla::Audio::Environment::Options result;

    result.sampleRate = options->sample_rate;
    result.blockSize = options->block_size;
    result.realtimeMemorySize = options->realtime_memory_size;
    result.maxNumNodes = options->max_num_nodes;
    result.maxNumAudioBuses = options->max_num_audio_buses;

    if (options->plugin_libraries != nullptr)
    {
        Methcla_LibraryFunction* it = options->plugin_libraries;
        while (*it != nullptr)
        {
            result.pluginLibraries.push_back(*it);
            it++;
        }
    }

    if (options->plugin_directories != nullptr)
    {
        const char* const* it = options->plugin_directories;
        while (*it != nullptr)
        {
            result.pluginDirectories.push_back(*it);
            it++;
        }
    }

    return result;
}

Methcla::Audio::IO::Driver::Options
Methcla::API::convertOptions(const Methcla_AudioDriverOptions* options)
{
    Methcla::Audio::IO::Driver::Options result;
    result.sampleRate = options->sample_rate;
    result.numInputs = options->num_inputs;
    result.numOutputs = options->num_outputs;
    result.bufferSize = options->buffer_size;
    return result;
}

struct Methcla_Engine
{
public:
    Methcla_Engine(const Methcla_EngineOptions* options,
                   Methcla_AudioDriver*         driver)
    {
        Methcla::Audio::Environment::Options engineOptions =
            Methcla::API::convertOptions(options);

        m_driver = std::unique_ptr<Methcla_AudioDriver>(driver);
        m_driver->driver()->setProcessCallback(processCallback, this);

        engineOptions.sampleRate = m_driver->driver()->sampleRate();
        engineOptions.blockSize = m_driver->driver()->bufferSize();
        engineOptions.numHardwareInputChannels =
            m_driver->driver()->numInputs();
        engineOptions.numHardwareOutputChannels =
            m_driver->driver()->numOutputs();

        using namespace std::placeholders;
        m_env = std::unique_ptr<Methcla::Audio::Environment>(
            new Methcla::Audio::Environment(
                std::bind(options->log_handler.log_line,
                          options->log_handler.handle, _1, _2),
                std::bind(options->packet_handler.handle_packet,
                          options->packet_handler.handle, _1, _2, _3),
                engineOptions));
    }

    const Methcla::Audio::Environment& env() const
    {
        return *m_env;
    }
    Methcla::Audio::Environment& env()
    {
        return *m_env;
    }

    const Methcla::Audio::IO::Driver* driver() const
    {
        return m_driver->driver();
    }
    Methcla::Audio::IO::Driver* driver()
    {
        return m_driver->driver();
    }

    void start()
    {
        driver()->start();
    }
    void stop()
    {
        driver()->stop();
    }

    ~Methcla_Engine()
    {
        stop();
    }

private:
    static void processCallback(void* data, Methcla_Time currentTime,
                                size_t                            numFrames,
                                const Methcla_AudioSample* const* inputs,
                                Methcla_AudioSample* const*       outputs)
    {
        static_cast<Methcla_Engine*>(data)->m_env->process(
            currentTime, numFrames, inputs, outputs);
    }

private:
    std::unique_ptr<Methcla::Audio::Environment> m_env;
    std::unique_ptr<Methcla_AudioDriver>         m_driver;
};

Methcla::Audio::IO::Driver* Methcla::API::getDriver(Methcla_Engine* engine)
{
    return engine->driver();
}

#define METHCLA_API_TRY try

#define METHCLA_API_CATCH                                                     \
    catch (Methcla::Error & e)                                                \
    {                                                                         \
        return methcla_error_new_with_message(e.errorCode(), e.what());       \
    }                                                                         \
    catch (std::bad_alloc)                                                    \
    {                                                                         \
        return methcla_error_new(kMethcla_MemoryError);                       \
    }                                                                         \
    catch (std::invalid_argument & e)                                         \
    {                                                                         \
        return methcla_error_new_with_message(kMethcla_ArgumentError,         \
                                              e.what());                      \
    }                                                                         \
    catch (std::logic_error & e)                                              \
    {                                                                         \
        return methcla_error_new_with_message(kMethcla_LogicError, e.what()); \
    }                                                                         \
    catch (std::exception & e)                                                \
    {                                                                         \
        return methcla_error_new_with_message(kMethcla_UnspecifiedError,      \
                                              e.what());                      \
    }                                                                         \
    catch (...)                                                               \
    {                                                                         \
        return methcla_error_new(kMethcla_UnspecifiedError);                  \
    }

const char* methcla_version()
{
    return kMethclaVersion;
}

static void nullPacketHandler(void*, Methcla_RequestId, const void*, size_t)
{}

METHCLA_EXPORT void
methcla_audio_driver_options_init(Methcla_AudioDriverOptions* options)
{
    options->sample_rate = -1;
    options->num_inputs = -1;
    options->num_outputs = -1;
    options->buffer_size = -1;
}

METHCLA_EXPORT Methcla_Error methcla_default_audio_driver(
    const Methcla_AudioDriverOptions* options, Methcla_AudioDriver** outDriver)
{
    if (options == nullptr || outDriver == nullptr)
        return methcla_error_new(kMethcla_ArgumentError);
    METHCLA_API_TRY
    {
        Methcla::Audio::IO::Driver* driver =
            Methcla::Platform::defaultAudioDriver(
                Methcla::API::convertOptions(options));
        if (driver == nullptr)
            throw std::runtime_error("Couldn't create default audio driver");
        *outDriver = Methcla::API::wrapAudioDriver(driver);
    }
    METHCLA_API_CATCH;
    return methcla_no_error();
}

static Methcla_PacketHandler defaultPacketHandler()
{
    Methcla_PacketHandler handler;
    handler.handle = nullptr;
    handler.handle_packet = nullPacketHandler;
    return handler;
}

METHCLA_EXPORT void methcla_engine_options_init(Methcla_EngineOptions* options)
{
    memset(options, 0, sizeof(Methcla_EngineOptions));
    options->log_handler = Methcla::Platform::defaultLogHandler();
    options->packet_handler = defaultPacketHandler();
    options->log_level = kMethcla_LogWarn;
}

METHCLA_EXPORT Methcla_Error methcla_engine_new_with_driver(
    const Methcla_EngineOptions* options, Methcla_AudioDriver* driver,
    Methcla_Engine** engine)
{
    if (driver == nullptr)
        return methcla_error_new(kMethcla_ArgumentError);
    if (options == nullptr)
    {
        delete driver;
        return methcla_error_new(kMethcla_ArgumentError);
    }
    if (engine == nullptr)
    {
        delete driver;
        return methcla_error_new(kMethcla_ArgumentError);
    }
    METHCLA_API_TRY
    {
        *engine = new Methcla_Engine(options, driver);
    }
    METHCLA_API_CATCH;
    return methcla_no_error();
}

METHCLA_EXPORT void methcla_engine_free(Methcla_Engine* engine)
{
    // cout << "Methcla_Engine_free" << endl;
    methcla_engine_stop(engine);
    try
    {
        delete engine;
    }
    catch (...)
    {}
}

METHCLA_EXPORT Methcla_Error methcla_engine_start(Methcla_Engine* engine)
{
    // cout << "Methcla_Engine_start" << endl;
    if (engine == nullptr)
        return methcla_error_new(kMethcla_ArgumentError);
    METHCLA_API_TRY
    {
        engine->start();
    }
    METHCLA_API_CATCH;
    return methcla_no_error();
}

METHCLA_EXPORT Methcla_Error methcla_engine_stop(Methcla_Engine* engine)
{
    // cout << "Methcla_Engine_stop" << endl;
    if (engine == nullptr)
        return methcla_error_new(kMethcla_ArgumentError);
    METHCLA_API_TRY
    {
        engine->stop();
    }
    METHCLA_API_CATCH;
    return methcla_no_error();
}

METHCLA_EXPORT void methcla_engine_set_log_flags(Methcla_Engine*        engine,
                                                 Methcla_EngineLogFlags flags)
{
    engine->env().setLogFlags(flags);
}

METHCLA_EXPORT void methcla_engine_log_line(Methcla_Engine*  engine,
                                            Methcla_LogLevel level,
                                            const char*      message)
{
    engine->env().logLineNRT(level, message);
}

METHCLA_EXPORT uint64_t methcla_time_to_uint64(Methcla_Time time)
{
    static_assert(sizeof(uint64_t) == sizeof(Methcla_Time),
                  "sizeof(uint64_t) != sizeof(Methcla_Time)");
    uint64_t result;
    std::memcpy(&result, &time, sizeof(result));
    return result;
}

METHCLA_EXPORT Methcla_Time methcla_time_from_uint64(uint64_t time)
{
    static_assert(sizeof(uint64_t) == sizeof(Methcla_Time),
                  "sizeof(uint64_t) != sizeof(Methcla_Time)");
    double result;
    std::memcpy(&result, &time, sizeof(result));
    return result;
}

METHCLA_EXPORT Methcla_Time methcla_engine_current_time(Methcla_Engine* engine)
{
    return engine == nullptr ? 0. : engine->driver()->currentTime();
}

METHCLA_EXPORT Methcla_Error
               methcla_engine_send(Methcla_Engine* engine, const Methcla_OSCPacket* packet)
{
    if (engine == nullptr)
        return methcla_error_new(kMethcla_ArgumentError);
    if (packet == nullptr)
        return methcla_error_new(kMethcla_ArgumentError);
    if (packet->size == 0)
        return methcla_error_new(kMethcla_ArgumentError);
    METHCLA_API_TRY
    {
        engine->env().send(*packet);
    }
    METHCLA_API_CATCH;
    return methcla_no_error();
}

METHCLA_EXPORT Methcla_Error methcla_engine_soundfile_open(
    const Methcla_Engine* engine, const char* path, Methcla_FileMode mode,
    Methcla_SoundFile** file, Methcla_SoundFileInfo* info)
{
    if (engine == nullptr)
        return methcla_error_new(kMethcla_ArgumentError);
    if (path == nullptr || *path == '\0')
        return methcla_error_new(kMethcla_ArgumentError);
    if (mode < kMethcla_FileModeRead || mode > kMethcla_FileModeWrite)
        return methcla_error_new(kMethcla_ArgumentError);
    if (file == nullptr)
        return methcla_error_new(kMethcla_ArgumentError);
    if (info == nullptr)
        return methcla_error_new(kMethcla_ArgumentError);
    Methcla_Host host(engine->env());
    return methcla_host_soundfile_open(&host, path, mode, file, info);
}
