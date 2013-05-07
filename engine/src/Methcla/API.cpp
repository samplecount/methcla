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
#include "Methcla/Audio/Engine.hpp"
#include "Methcla/Audio/SynthDef.hpp"

#include <cstdlib>
#include <cstring>
#include <iostream>
#include <new>
#include <stdexcept>
#include <string>
#include <unordered_map>

class Options
{
public:
    Options(const Methcla_Option* options)
    {
        for (const Methcla_Option* cur = options; cur->key != nullptr; cur++) {
            m_options[cur->key] = cur->value;
        }
    }

    template <typename T> T lookup(const std::string& key, const T& def=nullptr)
    {
        auto it = m_options.find(key);
        return it == m_options.end() ? def : static_cast<T>(it->second);
    }

private:
    typedef std::unordered_map<std::string,const void*> OptionMap;
    OptionMap m_options;
};

static const char* kNoErrorMsg = "no error";
static const char* kBadAllocMsg = "memory allocation failure";

struct Methcla_Engine
{
    struct PacketHandler
    {
        Methcla_PacketHandler handler;
        void* data;

        void operator()(Methcla_RequestId requestId, const void* packet, size_t size)
        {
            handler(data, requestId, packet, size);
        }
    };

    Methcla_Engine(Methcla_PacketHandler handler, void* handlerData, const Methcla_Option* inOptions) noexcept
        : m_error(kMethcla_NoError)
        , m_errorMessage(kNoErrorMsg)
    {
        Options options(inOptions);

        // TODO: Put this somewhere else
        Methcla::Audio::PluginManager::LibraryFunctions libs;
        for ( auto it = options.lookup<const Methcla_PluginLibrary*>(METHCLA_OPTION__PLUGIN_LIBRARIES)
            ; it->function != nullptr
            ; it++ )
        {
            libs.push_back(it->function);
        }

        m_pluginManager = new Methcla::Audio::PluginManager(libs);
        const char* pluginPath = options.lookup<const char*>(METHCLA_OPTION__PLUGIN_PATH, ".");
        m_engine = new Methcla::Audio::Engine(
            *m_pluginManager,
            PacketHandler { .handler = handler, .data = handlerData },
            pluginPath );
    }
    ~Methcla_Engine()
    {
        delete m_engine;
        delete m_pluginManager;
    }

    Methcla::Audio::PluginManager* m_pluginManager;
    Methcla::Audio::Engine*        m_engine;
    Methcla_Error                  m_error;
    const char*                    m_errorMessage;
    std::string                    m_errorBuffer;
};

Methcla_Engine* methcla_engine_new(Methcla_PacketHandler handler, void* handler_data, const Methcla_Option* options)
{
    // cout << "Methcla_Engine_new" << endl;
    try {
        return new Methcla_Engine(handler, handler_data, options);
    } catch (std::bad_alloc) {
        return nullptr;
    }
}

void methcla_engine_free(Methcla_Engine* engine)
{
    // cout << "Methcla_Engine_free" << endl;
    methcla_engine_stop(engine);
    try {
        delete engine;
    } catch(...) { }
}

Methcla_Error methcla_engine_error(const Methcla_Engine* engine)
{
    return engine == nullptr ? kMethcla_BadAlloc : engine->m_error;
}

const char* methcla_engine_error_message(const Methcla_Engine* engine)
{
    return engine == nullptr
            ? kBadAllocMsg
            : (engine->m_error == kMethcla_NoError
                ? kNoErrorMsg
                : engine->m_errorMessage);
}

#define METHCLA_ENGINE_TRY \
    if (engine != nullptr) { \
        engine->m_error = kMethcla_NoError; \
        try

#define METHCLA_ENGINE_CATCH \
    catch (std::exception& e) { \
        try { \
            engine->m_error = kMethcla_Error; \
            engine->m_errorBuffer = e.what(); \
            engine->m_errorMessage = engine->m_errorBuffer.c_str(); \
        } catch (std::bad_alloc) { \
            engine->m_error = kMethcla_BadAlloc; \
            engine->m_errorMessage = kBadAllocMsg; \
        } \
    } }

void methcla_engine_start(Methcla_Engine* engine)
{
    // cout << "Methcla_Engine_start" << endl;
    METHCLA_ENGINE_TRY {
        engine->m_engine->start();
    } METHCLA_ENGINE_CATCH;
}

void methcla_engine_stop(Methcla_Engine* engine)
{
    // cout << "Methcla_Engine_stop" << endl;
    METHCLA_ENGINE_TRY {
        engine->m_engine->stop();
    } METHCLA_ENGINE_CATCH;
}

void methcla_engine_send(Methcla_Engine* engine, const void* packet, size_t size)
{
    METHCLA_ENGINE_TRY {
        engine->m_engine->env().send(packet, size);
    } METHCLA_ENGINE_CATCH;
}

void* methcla_engine_impl(Methcla_Engine* engine)
{
    METHCLA_ENGINE_TRY {
        return engine->m_engine;
    } METHCLA_ENGINE_CATCH;
    return nullptr;
}

