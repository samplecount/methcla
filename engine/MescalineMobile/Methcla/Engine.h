//
//  Engine.h
//  MescalineMobile
//
//  Created by Stefan Kersten on 2/7/13.
//
//

#ifndef MescalineMobile_Engine_h
#define MescalineMobile_Engine_h

#include <Mescaline/Audio/Client.hpp>
#include <Mescaline/Audio/Engine.hpp>
#include <Mescaline/Audio/IO/Client.hpp>
#include <Mescaline/Audio/IO/Driver.hpp>
#include <Mescaline/Audio/IO/RemoteIODriver.hpp>
#include <Mescaline/Audio/Group.hpp>
#include <Mescaline/Audio/Synth.hpp>
#include <Mescaline/Audio/SynthDef.hpp>
#include <Mescaline/Utility/MessageQueue.hpp>

#include <AudioToolbox/AudioServices.h>

#include <boost/filesystem.hpp>
#include <iostream>

#include "lilv/lilv.h"
#include "lv2/lv2plug.in/ns/ext/atom/atom.h"
#include "lv2/lv2plug.in/ns/ext/atom/forge.h"
#include "lv2/lv2plug.in/ns/ext/atom/util.h"

//MESCALINE_EXPORT void MESCALINE_INIT_FUNC(osc)(MescalineHost*);
//MESCALINE_EXPORT void MESCALINE_INIT_FUNC(Scope)(MescalineHost*);

class MyLoader : public Mescaline::Audio::Plugin::StaticLoader
{
public:
    MyLoader()
        : Mescaline::Audio::Plugin::StaticLoader(descriptorFunctions())
    {
//        setenv("LV2_PATH", "/Users/sk/Library/Audio/Plug-Ins/LV2", 1);
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
        : Mescaline::Audio::Engine(loader, lv2BundleDirectory())
        , m_osc(0)
//        , m_scope(0)
    { }

    static boost::filesystem::path lv2BundleDirectory()
    {
        NSString* resources = [[NSBundle mainBundle] resourcePath];
        NSString* bundles = [resources stringByAppendingPathComponent:@"lv2/bundles"];
        return boost::filesystem::path([bundles UTF8String]);
    }

    virtual void configure(const Mescaline::Audio::IO::Driver& driver)
    {
        Mescaline::Audio::Engine::configure(driver);

        // Create sine instance
        const Mescaline::Audio::Plugin::Manager::PluginHandle& def = env().plugins().lookup(
            "http://mescaline.puesnada.es/lv2/plugins/sine" );
        Mescaline::Audio::Synth* synth = m_osc = Mescaline::Audio::Synth::construct(env(), env().rootNode(), Mescaline::Audio::Node::kAddToTail, *def);
        synth->mapOutput(0, env().externalAudioOutput(0).id(), Mescaline::Audio::kOut);

//        const Mescaline::Audio::SynthDef& scopeDef = environment()->lookupSynthDef("scope");
//        Mescaline::Audio::Synth* scope = Mescaline::Audio::Synth::construct(*environment(), 2, environment()->rootNode(), scopeDef);
//        environment()->rootNode()->addToTail(*scope);
//        scope->mapInput(0, Mescaline::Audio::AudioBusId(Mescaline::Audio::AudioBusId::kOutput, 0), Mescaline::Audio::kIn);
//        m_scope = scope->synth<Mescaline::Audio::ScopeSynth>();
    }

    Mescaline::Audio::Synth* osc() { return m_osc; }
//    Mescaline::Audio::ScopeSynth* scope() { return m_scope; }

private:
    Mescaline::Audio::Synth* m_osc;
//    Mescaline::Audio::ScopeSynth* m_scope;
};

#endif
