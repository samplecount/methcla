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

#include <methcla/engine.h>
#include <methcla/plugin.h>

#include "Methcla/API.hpp"
#include "Methcla/Audio/Engine.hpp"
#include "Methcla/Audio/SynthDef.hpp"
#include "Methcla/Exception.hpp"

#include <cstdlib>
#include <cstring>
#include <functional>
#include <iostream>
#include <new>
#include <oscpp/server.hpp>
#include <stdexcept>
#include <string>

void Methcla::API::parseOptions(
    const Methcla_OSCPacket* inOptions,
    Methcla::Audio::Environment::Options* engineOptions,
    Methcla::Audio::IO::Driver::Options* driverOptions)
{
    OSCPP::Server::Packet packet(inOptions->data, inOptions->size);

    if (packet.isBundle())
    {
        auto packets = OSCPP::Server::Bundle(packet).packets();
        while (!packets.atEnd())
        {
            auto optionPacket = packets.next();
            if (optionPacket.isMessage())
            {
                OSCPP::Server::Message option(optionPacket);
                if (option == "/engine/option/plugin-library")
                {
                    OSCPP::Blob x = option.args().blob();
                    if (x.size() == sizeof(Methcla_LibraryFunction))
                    {
                        Methcla_LibraryFunction f;
                        memcpy(&f, x.data(), x.size());
                        engineOptions->pluginLibraries.push_back(f);
                    }
                }
                else if (option == "/engine/option/driver/buffer-size")
                {
                    driverOptions->bufferSize = option.args().int32();
                }
            }
        }
    }
}

struct Methcla_Engine
{
    Methcla_Engine(Methcla_PacketHandler handler, void* handlerData, const Methcla_OSCPacket* inOptions)
    {
        Methcla::Audio::Environment::Options engineOptions;
        Methcla::Audio::IO::Driver::Options driverOptions;

        Methcla::API::parseOptions(inOptions, &engineOptions, &driverOptions);

        using namespace std::placeholders;

        m_engine = new Methcla::Audio::Engine(
            std::bind(handler, handlerData, _1, _2, _3),
            engineOptions,
            driverOptions
        );
    }

    ~Methcla_Engine()
    {
        delete m_engine;
    }

    Methcla::Audio::Engine* m_engine;
};

#define METHCLA_ENGINE_TRY \
    try

#define METHCLA_ENGINE_CATCH \
    catch (Methcla::Error& e) { \
        return e.errorCode(); \
    } catch (std::bad_alloc) { \
        return kMethcla_MemoryError; \
    } catch (std::invalid_argument) { \
        return kMethcla_ArgumentError; \
    } catch (std::logic_error) { \
        return kMethcla_LogicError; \
    } catch (...) { \
        return kMethcla_UnspecifiedError; \
    }

METHCLA_EXPORT Methcla_Error methcla_engine_new(Methcla_PacketHandler handler, void* handler_data, const Methcla_OSCPacket* options, Methcla_Engine** engine)
{
    // cout << "Methcla_Engine_new" << endl;
    if (handler == nullptr)
        return kMethcla_ArgumentError;
    if (options == nullptr)
        return kMethcla_ArgumentError;
    if (engine == nullptr)
        return kMethcla_ArgumentError;
    METHCLA_ENGINE_TRY {
        *engine = new Methcla_Engine(handler, handler_data, options);
    } METHCLA_ENGINE_CATCH;
    return kMethcla_NoError;
}

METHCLA_EXPORT void methcla_engine_free(Methcla_Engine* engine)
{
    // cout << "Methcla_Engine_free" << endl;
    methcla_engine_stop(engine);
    try {
        delete engine;
    } catch(...) { }
}

METHCLA_EXPORT const char* methcla_error_message(Methcla_Error error)
{
    switch (error)
    {
        case kMethcla_NoError: return "No error";

        /* Generic error codes */
        case kMethcla_UnspecifiedError: return "Unspecified error";
        case kMethcla_LogicError: return "Logic error";
        case kMethcla_ArgumentError: return "Invalid argument";
        case kMethcla_MemoryError: return "Out of memory";
        case kMethcla_UnimplementedError: return "Operation not implemented";
        case kMethcla_SystemError: return "Generic operating system error";

        /* Engine errors */
        case kMethcla_SynthDefNotFoundError: return "SynthDef not found";
        case kMethcla_NodeIdError: return "Invalid node id";
        case kMethcla_NodeTypeError: return "Invalid node type";

        /* File errors */
        case kMethcla_FileNotFoundError: return "File not found";
        case kMethcla_FileExistsError: return "File already exists";
        case kMethcla_PermissionsError: return "Insufficient file permissions";
        case kMethcla_UnsupportedFileTypeError: return "Unsupported file type";
        case kMethcla_UnsupportedDataFormatError: return "Unsupported data format";
        case kMethcla_InvalidFileError: return "Malformed file contents";

        /* Audio driver errors */
        case kMethcla_DeviceUnavailableError: return "Audio device not available";
    }

    return "Unknown Methcla_Error code";
}

METHCLA_EXPORT Methcla_Error methcla_engine_start(Methcla_Engine* engine)
{
    // cout << "Methcla_Engine_start" << endl;
    if (engine == nullptr)
        return kMethcla_ArgumentError;
    METHCLA_ENGINE_TRY {
        engine->m_engine->start();
    } METHCLA_ENGINE_CATCH;
    return kMethcla_NoError;
}

METHCLA_EXPORT Methcla_Error methcla_engine_stop(Methcla_Engine* engine)
{
    // cout << "Methcla_Engine_stop" << endl;
    if (engine == nullptr)
        return kMethcla_ArgumentError;
    METHCLA_ENGINE_TRY {
        engine->m_engine->stop();
    } METHCLA_ENGINE_CATCH;
    return kMethcla_NoError;
}

METHCLA_EXPORT uint64_t methcla_time_to_uint64(Methcla_Time time)
{
    static_assert(sizeof(uint64_t) == sizeof(Methcla_Time), "sizeof(uint64_t) != sizeof(Methcla_Time)");
    uint64_t result;
    std::memcpy(&result, &time, sizeof(result));
    return result;
}

METHCLA_EXPORT Methcla_Time methcla_time_from_uint64(uint64_t time)
{
    static_assert(sizeof(uint64_t) == sizeof(Methcla_Time), "sizeof(uint64_t) != sizeof(Methcla_Time)");
    double result;
    std::memcpy(&result, &time, sizeof(result));
    return result;
}

METHCLA_EXPORT Methcla_Time methcla_engine_current_time(const Methcla_Engine* engine)
{
    if (engine == nullptr)
        return kMethcla_ArgumentError;
    return engine->m_engine->driver()->currentTime();
}

METHCLA_EXPORT Methcla_Error methcla_engine_send(Methcla_Engine* engine, const void* packet, size_t size)
{
    if (engine == nullptr)
        return kMethcla_ArgumentError;
    if (packet == nullptr)
        return kMethcla_ArgumentError;
    if (size == 0)
        return kMethcla_ArgumentError;
    METHCLA_ENGINE_TRY {
        engine->m_engine->env().send(packet, size);
    } METHCLA_ENGINE_CATCH;
    return kMethcla_NoError;
}
