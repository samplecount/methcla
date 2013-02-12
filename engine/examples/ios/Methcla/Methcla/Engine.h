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

#include <methcla/engine.h>

#include <Methcla/Audio/Engine.hpp>
#include <Methcla/Audio/IO/RemoteIODriver.hpp> // NOTE: for OSStatusInfo only
#include <Methcla/Audio/Group.hpp>
#include <Methcla/Audio/Synth.hpp>
#include <Methcla/Audio/SynthDef.hpp>
#include <Methcla/Utility/MessageQueue.hpp>

#include <AudioToolbox/AudioServices.h>

#include <boost/filesystem.hpp>
#include <iostream>

#include "lilv/lilv.h"
#include "lv2/lv2plug.in/ns/ext/atom/atom.h"
#include "lv2/lv2plug.in/ns/ext/atom/forge.h"
#include "lv2/lv2plug.in/ns/ext/atom/util.h"

Methcla_Engine* makeEngine()
{
    NSString* resources = [[NSBundle mainBundle] resourcePath];
    NSString* bundles = [resources stringByAppendingPathComponent:@"lv2/bundles"];

    const char* lv2Path = [bundles UTF8String];
    Methcla_Option options[2] = {
        { METHCLA_OPTION__LV2_PATH, lv2Path }
        , METHCLA_END_OPTIONS };

    extern const LV2_Descriptor* methcla_sine_lv2_descriptor(uint32_t index);
    Methcla_Library_Symbol symbols[2] = {
        { METHCLA_LV2_URI "/plugins/sine", "lv2_descriptor", (Methcla_Library_Function)methcla_sine_lv2_descriptor }
        , METHCLA_END_SYMBOLS };

    Methcla_Engine* theEngine = methcla_engine_new_with_backend("", options, symbols);

    Methcla::Audio::Engine* engine = static_cast<Methcla::Audio::Engine*>(methcla_engine_impl(theEngine));

    const Methcla::Audio::PluginManager::PluginHandle& def = engine->env().plugins().lookup(
        engine->env().mapUri(METHCLA_LV2_URI "/plugins/sine") );
    Methcla::Audio::Synth* synth = Methcla::Audio::Synth::construct(
        engine->env(), engine->env().rootNode(), Methcla::Audio::Node::kAddToTail, *def);

    synth->mapOutput(0, engine->env().externalAudioOutput(0).id(), Methcla::Audio::kOut);

    return theEngine;
}

#endif
