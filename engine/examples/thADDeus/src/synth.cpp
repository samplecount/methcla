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

#include "synth.hpp"

#include <methc.la/plugins/sine/sine.h>

#include <cassert>

thaddeus::Engine::Engine()
{
    m_engine = new Methcla::Engine({ Methcla::Option::pluginLibrary(methcla_plugins_sine) });
    m_engine->start();
}

thaddeus::Engine::~Engine()
{
    m_engine->stop();
    delete m_engine;
}

void thaddeus::Engine::start()
{
    m_engine->start();
}

void thaddeus::Engine::stop()
{
    m_engine->stop();
}

static inline float mapFreq(float x)
{
    return x * 800 + 200;
}

void thaddeus::Engine::startVoice(VoiceId voice, float x, float y)
{
    if (m_voices.find(voice) != m_voices.end()) {
        stopVoice(voice);
    }
    const float xFreq = mapFreq(x);
    const float yFreq = mapFreq(y);
    const Methcla::SynthId synth_x = m_engine->synth(METHCLA_PLUGINS_SINE_URI, { xFreq });
    const Methcla::SynthId synth_y = m_engine->synth(METHCLA_PLUGINS_SINE_URI, { yFreq });
    m_engine->mapOutput(synth_x, 0, Methcla::AudioBusId(1));
    m_engine->mapOutput(synth_y, 0, Methcla::AudioBusId(1));
//        std::cout << "Synth " << synth << " started: freq=" << ps.freq << " amp=" << ps.amp << std::endl;
    m_voices[voice] = std::make_tuple(synth_x, synth_y);
}

void thaddeus::Engine::updateVoice(VoiceId voice, float x, float y)
{
    auto it = m_voices.find(voice);
    assert( it != m_voices.end() );
    auto synths = it->second;
    m_engine->set(std::get<0>(synths), 0, mapFreq(x));
    m_engine->set(std::get<1>(synths), 0, mapFreq(y));
}

void thaddeus::Engine::stopVoice(VoiceId voice)
{
    auto it = m_voices.find(voice);
    if (it != m_voices.end()) {
        auto synths = it->second;
        m_engine->freeNode(std::get<0>(synths));
        m_engine->freeNode(std::get<1>(synths));
        m_voices.erase(it);
    }
}
