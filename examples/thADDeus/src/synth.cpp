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

#include <methcla/plugins/sine.h>

#include <cassert>

thaddeus::Engine::Engine(Methcla::EngineOptions options,
                         Methcla_AudioDriver*   audioDriver)
{
    options.addLibrary(methcla_plugins_sine);
    m_engine = new Methcla::Engine(options, audioDriver);
    m_engine->start();
    m_engine->setLogFlags(kMethcla_EngineLogDebug);
}

thaddeus::Engine::~Engine()
{
    m_engine->stop();
    delete m_engine;
}

void thaddeus::Engine::start() { m_engine->start(); }

void thaddeus::Engine::stop() { m_engine->stop(); }

void thaddeus::Engine::startVoice(VoiceId voice, float freq, float amp)
{
    if (m_voices.find(voice) != m_voices.end())
    {
        stopVoice(voice);
    }

    Methcla::Request request(m_engine);
    request.openBundle();
    const Methcla::SynthId synth =
        request.synth(METHCLA_PLUGINS_SINE_URI, m_engine->root(), {freq, amp});
    request.activate(synth);
    request.mapOutput(synth, 0, Methcla::AudioBusId(0),
                      Methcla::kBusMappingExternal);
    request.closeBundle();
    request.send();
    //        std::cout << "Synth " << synth << " started: freq=" << ps.freq <<
    //        " amp=" << ps.amp << std::endl;
    m_voices[voice] = synth;
}

void thaddeus::Engine::updateVoice(VoiceId voice, float freq, float amp)
{
    auto it = m_voices.find(voice);
    if (it != m_voices.end())
    {
        auto             synth = it->second;
        Methcla::Request request(m_engine);
        request.openBundle();
        request.set(synth, 0, freq);
        request.set(synth, 1, amp);
        request.closeBundle();
        request.send();
    }
}

void thaddeus::Engine::stopVoice(VoiceId voice)
{
    auto it = m_voices.find(voice);
    if (it != m_voices.end())
    {
        auto             synth = it->second;
        Methcla::Request request(m_engine);
        request.openBundle();
        request.free(synth);
        request.closeBundle();
        request.send();
        m_voices.erase(it);
    }
}
