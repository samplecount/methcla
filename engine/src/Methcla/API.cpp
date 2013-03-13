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

#include <boost/filesystem.hpp>

#include <cstdlib>
#include <cstring>
#include <iostream>
#include <unordered_map>

#include "lv2/lv2plug.in/ns/ext/atom/forge.h"
#include "lv2/lv2plug.in/ns/ext/atom/util.h"

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

struct Methcla_Engine
{
    Methcla_Engine(const Methcla_Option* inOptions)
    {
        Options options(inOptions);
        const boost::filesystem::path lv2Path(options.lookup<const char*>(METHCLA_OPTION__LV2_PATH));

        // TODO: Put this somewhere else
        auto libs = options.lookup<const Methcla_LV2_Library*>(METHCLA_OPTION__LV2_LIBRARIES);
        Methcla::Audio::PluginManager::StaticLibraryMap staticLibs;
        if (libs != nullptr) {
            while (libs->plugin != nullptr) {
                staticLibs[libs->plugin] = libs->function;
                libs++;
            }
        }

        m_pluginManager = new Methcla::Audio::PluginManager(staticLibs);
        m_engine = new Methcla::Audio::Engine(*m_pluginManager, lv2Path);
    }
    ~Methcla_Engine()
    {
        delete m_engine;
        delete m_pluginManager;
    }

    Methcla::Audio::PluginManager* m_pluginManager;
    Methcla::Audio::Engine*        m_engine;
};

Methcla_Engine* methcla_engine_new(const Methcla_Option* options)
{
    // cout << "Methcla_Engine_new" << endl;
    return new Methcla_Engine(options);
}

void Methcla_Engine_free(Methcla_Engine* engine)
{
    // cout << "Methcla_Engine_free" << endl;
    methcla_engine_stop(engine);
    delete engine;
}

void methcla_engine_start(Methcla_Engine* engine)
{
    // cout << "Methcla_Engine_start" << endl;
    engine->m_engine->start();
}

void methcla_engine_stop(Methcla_Engine* engine)
{
    // cout << "Methcla_Engine_stop" << endl;
    engine->m_engine->stop();
}

LV2_URID methcla_engine_map_uri(Methcla_Engine* engine, const char* uri)
{
    return engine->m_pluginManager->uriMap().map(uri);
}

const char* methcla_engine_unmap_uri(Methcla_Engine* engine, LV2_URID urid)
{
    return engine->m_pluginManager->uriMap().unmap(urid);
}

void methcla_engine_request(Methcla_Engine* engine, Methcla_Response_Handler handler, void* handler_data, const LV2_Atom* request)
{
    engine->m_engine->env().request(handler, handler_data, request);
}

void* methcla_engine_impl(Methcla_Engine* engine)
{
    return engine->m_engine;
}
