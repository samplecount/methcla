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

#ifndef THADDEUS_SYNTH_HPP_INCLUDED
#define THADDEUS_SYNTH_HPP_INCLUDED

#include <cstdint>
#include <methcla/engine.hpp>
#include <unordered_map>

namespace thaddeus
{
    class Engine
    {
    public:
        Engine(Methcla::EngineOptions options=Methcla::EngineOptions(), Methcla_AudioDriver* audioDriver=nullptr);
        ~Engine();

        void start();
        void stop();

        typedef intptr_t VoiceId;

        void startVoice(VoiceId voice, float freq, float amp);
        void updateVoice(VoiceId voice, float freq, float amp);
        void stopVoice(VoiceId voice);

    private:
        Methcla::Engine* m_engine;
        std::unordered_map<VoiceId,Methcla::SynthId> m_voices;
    };
}

#endif // THADDEUS_SYNTH_HPP_INCLUDED
