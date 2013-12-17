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

class Engine
{
public:
    Engine(const std::string& soundDir);
    ~Engine();

    Engine(const Engine& other) = delete;
    Engine& operator=(const Engine& other) = delete;

    // Return the index of the next sound to be played.
    // Simply cycles through all available sounds.
    size_t nextSound();

    typedef intptr_t VoiceId;

    // Start a voice with a certain sound and amplitude.
    void startVoice(VoiceId voice, size_t sound, float amp);
    // Update a voice's amplitude while playing.
    void updateVoice(VoiceId voice, float amp);
    // Stop a voice.
    void stopVoice(VoiceId voice);

private:
    Methcla::Engine& engine() { return *m_engine; }

private:
    std::vector<Sound>  m_sounds;
    Methcla::Engine*    m_engine;
    size_t              m_nextSound;
    Methcla::GroupId    m_voiceGroup;
    std::vector<Methcla::SynthId> m_patchCables;
    std::unordered_map<VoiceId,Methcla::SynthId> m_voices;
};

#endif // ENGINE_HPP_INCLUDED
