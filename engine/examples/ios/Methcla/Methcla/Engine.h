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

#ifndef MethclaMobile_Engine_h
#define MethclaMobile_Engine_h

#include <methcla/engine.hpp>

#include <Methcla/Audio/Engine.hpp>
#include <Methcla/Audio/IO/RemoteIODriver.hpp> // NOTE: for OSStatusInfo only
#include <Methcla/Audio/Group.hpp>
#include <Methcla/Audio/Synth.hpp>
#include <Methcla/Audio/SynthDef.hpp>
#include <Methcla/Utility/MessageQueue.hpp>

#include <AudioToolbox/AudioServices.h>

#include <boost/filesystem.hpp>
#include <iostream>
#include <memory>

#include "lilv/lilv.h"
#include "lv2/lv2plug.in/ns/ext/atom/atom.h"
#include "lv2/lv2plug.in/ns/ext/atom/forge.h"
#include "lv2/lv2plug.in/ns/ext/atom/util.h"

#include "lv2/methc.la/plugins/sine.lv2/sine.h"

//extern "C" const LV2_Descriptor* methcla_lv2_plugins_sine_lv2_descriptor(uint32_t index);
extern "C" const LV2_Descriptor* methcla_lv2_plugins_sine_lv2_descriptor(uint32_t index) __attribute__((used));

Methcla::Engine* makeEngine()
{
    NSString* resources = [[NSBundle mainBundle] resourcePath];
    NSString* bundles = [resources stringByAppendingPathComponent:@"lv2/bundles"];

    const char* lv2Path = [bundles UTF8String];
    Methcla_LV2_Library libs[] = { METHCLA_LV2_PLUGINS_SINE_LIB, METHCLA_END_LIBRARIES };

    Methcla_Option options[] = {
        { METHCLA_OPTION__LV2_PATH, lv2Path },
        { METHCLA_OPTION__LV2_LIBRARIES, libs },
          METHCLA_END_OPTIONS };

//    volatile void* fuckMe = (void*)methcla_lv2_plugins_sine_lv2_descriptor;

//    extern const LV2_Descriptor* methcla_sine_lv2_descriptor(uint32_t index);
//    Methcla_Library_Symbol symbols[2] = {
//        { METHCLA_LV2_URI "/plugins/sine", "lv2_descriptor", (Methcla_Library_Function)methcla_sine_lv2_descriptor }
//        , METHCLA_END_SYMBOLS };

    Methcla::Engine* theEngine = new Methcla::Engine(options);
    theEngine->start();

    Methcla::Audio::Engine* engine = static_cast<Methcla::Audio::Engine*>(theEngine->impl());

//    const std::shared_ptr<Methcla::Audio::Plugin> def = engine->env().plugins().lookup(
//        engine->env().mapUri(METHCLA_LV2_URI "/plugins/sine") );
//    Methcla::Audio::Synth* synth = Methcla::Audio::Synth::construct(
//        engine->env(), engine->env().rootNode(), Methcla::Audio::Node::kAddToTail, *def);

    Methcla::NodeId synthId = Methcla::synth(*theEngine, METHCLA_LV2_URI "/plugins/sine");
    
//    synth->mapOutput(0, engine->env().externalAudioOutput(0).id(), Methcla::Audio::kOut);

    return theEngine;
}

#endif
