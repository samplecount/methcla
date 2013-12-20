// Copyright 2013 Samplecount S.L.
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

#ifndef ENGINE_HPP_INCLUDED
#define ENGINE_HPP_INCLUDED

#include <methcla/engine.hpp>
#include <string>
#include <vector>
#include <unordered_map>

namespace Methcla { namespace Examples { namespace Sampler {

class Sound
{
public:
    Sound(const Methcla::Engine& engine, const std::string& path);

    const std::string& path() const
    {
        return m_path;
    }

    float duration() const
    {
        return m_duration;
    }

private:
    std::string m_path;
    float m_duration;
};

template <typename T> T linmap(T outMin, T outMax, T inMin, T inMax, T x)
{
    return (x - inMin) / (inMax - inMin) * (outMax - outMin) + outMin;
}

template <typename T> T expmap(T outMin, T outMax, T inMin, T inMax, T x)
{
    return outMin * std::pow(outMax / outMin, (x - inMin) / (inMax - inMin));
}

template <typename T> T dbamp(T db)
{
    return std::pow(T(10), db/T(20));
}

class Engine
{
public:
    struct Options
    {
        Methcla::EngineOptions engineOptions;
        Methcla_AudioDriver* audioDriver = nullptr;
        std::vector<std::string> sounds;
        std::string soundDir;
    };

    Engine(Options options);
    ~Engine();

    Engine(const Engine& other) = delete;
    Engine& operator=(const Engine& other) = delete;

    size_t numSounds() const;

    typedef intptr_t VoiceId;

    // Start a voice with a certain sound and amplitude.
    void startVoice(VoiceId voice, size_t sound, float amp=1.f, float rate=1.f);
    // Update a voice's amplitude while playing.
    void updateVoice(VoiceId voice, float amp, float rate=1.f);
    // Stop a voice.
    void stopVoice(VoiceId voice);

private:
    Methcla::Engine& engine() { return *m_engine; }

private:
    std::vector<Sound>  m_sounds;
    Methcla::Engine*    m_engine;
    Methcla::GroupId    m_voiceGroup;
    std::vector<Methcla::SynthId> m_patchCables;
    std::unordered_map<VoiceId,Methcla::SynthId> m_voices;
};

} } }

#endif // ENGINE_HPP_INCLUDED
