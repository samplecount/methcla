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

#include "Methcla/API.h"
#include "Methcla/Audio/Client.hpp"
#include "Methcla/Audio/IO/Client.hpp"
#include "Methcla/Audio/IO/JackDriver.hpp"
#include "Methcla/Audio/Group.hpp"
#include "Methcla/Audio/Synth.hpp"
#include "Methcla/Audio/SynthDef.hpp"

#include <cstdlib>
#include <cstring>
#include <iostream>

#include "lilv/lilv.h"
#include "lv2/lv2plug.in/ns/ext/atom/atom.h"
#include "lv2/lv2plug.in/ns/ext/atom/forge.h"
#include "lv2/lv2plug.in/ns/ext/atom/util.h"

class MyLoader : public Methcla::Audio::Plugin::StaticLoader
{
public:
    MyLoader()
        : Methcla::Audio::Plugin::StaticLoader(descriptorFunctions())
    {
        const char* bundlePath = getenv("METHCLA_LV2_PATH");
        if (bundlePath) {
            setenv("LV2_PATH", bundlePath, 1);
        } else {
            setenv("LV2_PATH", "/Users/sk/Library/Audio/Plug-Ins/LV2", 1);
        }
    }

    static Methcla::Audio::Plugin::StaticLoader::DescriptorFunctionMap descriptorFunctions()
    {
        Methcla::Audio::Plugin::StaticLoader::DescriptorFunctionMap dfs;
        extern const LV2_Descriptor* puesnada_sine_lv2_descriptor(uint32_t index);
        dfs["http://methc.la/lv2/plugins/sine"] = puesnada_sine_lv2_descriptor;
        return dfs;
    }
};

class MyEngine : public Methcla::Audio::Engine
{
public:
    MyEngine(MyLoader* loader)
        : Methcla::Audio::Engine(loader)
    { }

    virtual void configure(const Methcla::Audio::IO::Driver& driver)
    {
        Methcla::Audio::Engine::configure(driver);

        // Create sine instance
        const Methcla::Audio::Plugin::Manager::PluginHandle& def = env().plugins().lookup(
            "http://methc.la/lv2/plugins/sine" );
        Methcla::Audio::Synth* synth = Methcla::Audio::Synth::construct(env(), env().nextResourceId(), env().rootNode(), *def);
        env().rootNode()->addToTail(*synth);
        synth->mapOutput(0, env().audioBus(Methcla::Audio::ResourceId(3)), Methcla::Audio::kOut);

//        const Methcla::Audio::SynthDef& scopeDef = environment()->lookupSynthDef("scope");
//        Methcla::Audio::Synth* scope = Methcla::Audio::Synth::construct(*environment(), 2, environment()->rootNode(), scopeDef);
//        environment()->rootNode()->addToTail(*scope);
//        scope->mapInput(0, Methcla::Audio::AudioBusId(Methcla::Audio::AudioBusId::kOutput, 0), Methcla::Audio::kIn);
//        m_scope = scope->synth<Methcla::Audio::ScopeSynth>();
    }
};

using namespace std;

struct Methcla_Engine
{
    Methcla::Audio::Engine*     m_engine;
    Methcla::Audio::IO::Driver* m_audioDriver;
};

Methcla_Engine* Methcla_Engine_new()
{
    // cout << "Methcla_Engine_new" << endl;
    Methcla_Engine* engine = new Methcla_Engine;
    engine->m_engine = new MyEngine(new MyLoader());
    engine->m_audioDriver = new Methcla::Audio::IO::JackDriver(engine->m_engine);
    return engine;
}

void Methcla_Engine_free(Methcla_Engine* engine)
{
    // cout << "Methcla_Engine_free" << endl;
    Methcla_Engine_stop(engine);
    delete engine->m_engine;
    delete engine->m_audioDriver;
    delete engine;
}

void Methcla_Engine_start(Methcla_Engine* engine)
{
    // cout << "Methcla_Engine_start" << endl;
    engine->m_audioDriver->start();
}

void Methcla_Engine_stop(Methcla_Engine* engine)
{
    // cout << "Methcla_Engine_stop" << endl;
    engine->m_audioDriver->stop();
}

LV2_URID Methcla_Engine_mapUri(Methcla_Engine* engine, const char* uri)
{
	return engine->m_engine->env().mapUri(uri);
}

const char* Methcla_Engine_unmapUri(Methcla_Engine* engine, LV2_URID urid)
{
	return engine->m_engine->env().unmapUri(urid);
}

void Methcla_Engine_request(Methcla_Engine* engine, const LV2_Atom* request, Methcla_HandleResponse handler, void* handlerData)
{
    engine->m_engine->env().request(request, handler, handlerData);
}
