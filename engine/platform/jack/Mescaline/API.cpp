#include "Mescaline/API.h"
#include "Mescaline/Audio/Client.hpp"
#include "Mescaline/Audio/IO/Client.hpp"
#include "Mescaline/Audio/IO/JackDriver.hpp"
#include "Mescaline/Audio/Group.hpp"
#include "Mescaline/Audio/Synth.hpp"
#include "Mescaline/Audio/SynthDef.hpp"

#include <cstdlib>
#include <cstring>
#include <iostream>

#include "lilv/lilv.h"
#include "lv2/lv2plug.in/ns/ext/atom/atom.h"
#include "lv2/lv2plug.in/ns/ext/atom/forge.h"
#include "lv2/lv2plug.in/ns/ext/atom/util.h"

class MyLoader : public Mescaline::Audio::Plugin::StaticLoader
{
public:
    MyLoader()
        : Mescaline::Audio::Plugin::StaticLoader(descriptorFunctions())
    {
        const char* bundlePath = getenv("MESCALINE_LV2_PATH");
        if (bundlePath) {
            setenv("LV2_PATH", bundlePath, 1);
        } else {
            setenv("LV2_PATH", "/Users/sk/Library/Audio/Plug-Ins/LV2", 1);
        }
    }

    static Mescaline::Audio::Plugin::StaticLoader::DescriptorFunctionMap descriptorFunctions()
    {
        Mescaline::Audio::Plugin::StaticLoader::DescriptorFunctionMap dfs;
        extern const LV2_Descriptor* puesnada_sine_lv2_descriptor(uint32_t index);
        dfs["http://mescaline.puesnada.es/lv2/plugins/sine"] = puesnada_sine_lv2_descriptor;
        return dfs;
    }
};

class MyEngine : public Mescaline::Audio::Engine
{
public:
    MyEngine(MyLoader* loader)
        : Mescaline::Audio::Engine(loader)
    { }

    virtual void configure(const Mescaline::Audio::IO::Driver& driver)
    {
        Mescaline::Audio::Engine::configure(driver);

        // Create sine instance
        const Mescaline::Audio::Plugin::Manager::PluginHandle& def = env().plugins().lookup(
            "http://mescaline.puesnada.es/lv2/plugins/sine" );
        Mescaline::Audio::Synth* synth = Mescaline::Audio::Synth::construct(env(), env().nextResourceId(), env().rootNode(), *def);
        env().rootNode()->addToTail(*synth);
        synth->mapOutput(0, env().audioBus(Mescaline::Audio::ResourceId(3)), Mescaline::Audio::kOut);

//        const Mescaline::Audio::SynthDef& scopeDef = environment()->lookupSynthDef("scope");
//        Mescaline::Audio::Synth* scope = Mescaline::Audio::Synth::construct(*environment(), 2, environment()->rootNode(), scopeDef);
//        environment()->rootNode()->addToTail(*scope);
//        scope->mapInput(0, Mescaline::Audio::AudioBusId(Mescaline::Audio::AudioBusId::kOutput, 0), Mescaline::Audio::kIn);
//        m_scope = scope->synth<Mescaline::Audio::ScopeSynth>();
    }
};

using namespace std;

struct Mescaline_Engine
{
    Mescaline::Audio::Engine*     m_engine;
    Mescaline::Audio::IO::Driver* m_audioDriver;
};

Mescaline_Engine* Mescaline_Engine_new()
{
    // cout << "Mescaline_Engine_new" << endl;
    Mescaline_Engine* engine = new Mescaline_Engine;
    engine->m_engine = new MyEngine(new MyLoader());
    engine->m_audioDriver = new Mescaline::Audio::IO::JackDriver(engine->m_engine);
    return engine;
}

void Mescaline_Engine_free(Mescaline_Engine* engine)
{
    // cout << "Mescaline_Engine_free" << endl;
    Mescaline_Engine_stop(engine);
    delete engine->m_engine;
    delete engine->m_audioDriver;
    delete engine;
}

void Mescaline_Engine_start(Mescaline_Engine* engine)
{
    // cout << "Mescaline_Engine_start" << endl;
    engine->m_audioDriver->start();
}

void Mescaline_Engine_stop(Mescaline_Engine* engine)
{
    // cout << "Mescaline_Engine_stop" << endl;
    engine->m_audioDriver->stop();
}

LV2_URID Mescaline_Engine_mapUri(Mescaline_Engine* engine, const char* uri)
{
	return engine->m_engine->env().mapUri(uri);
}

const char* Mescaline_Engine_unmapUri(Mescaline_Engine* engine, LV2_URID urid)
{
	return engine->m_engine->env().unmapUri(urid);
}

void Mescaline_Engine_request(Mescaline_Engine* engine, LV2_Atom* request, Mescaline_HandleResponse handler, void* handlerData)
{
    engine->m_engine->env().request(request, handler, handlerData);
}
