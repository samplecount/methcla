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

#include "Engine.hpp"

#include <methcla/file.hpp>
#include <methcla/plugins/pro/disksampler.h>
#include <methcla/plugins/sampler.h>
#include <methcla/plugins/node-control.h>
#include <methcla/plugins/patch-cable.h>

#include <sstream>
#include <stdexcept>
#include <tinydir.h>

using namespace Methcla::Examples::Sampler;

Sound::Sound(const Methcla::Engine& engine, const std::string& path)
    : m_path(path)
{
    Methcla::SoundFile file(engine, path);
    m_duration = (double)file.info().frames / (double)file.info().samplerate;
}

// Return a list of sounds in directory path.
static std::vector<Sound> loadSounds(Methcla::Engine& engine, const std::string& path)
{
    engine.logLine(kMethcla_LogDebug, std::string("Loading sounds from ") + path);

    std::vector<Sound> result;

    tinydir_dir dir;
    int err = tinydir_open(&dir, path.c_str());
    if (err != 0)
    {
        std::stringstream s;
        s << "Couldn't open directory " << path
          << ": " << strerror(errno);
        throw std::runtime_error(s.str());
    }

    while (dir.has_next)
    {
        tinydir_file file;
        tinydir_readfile(&dir, &file);
        std::stringstream s;
        s << "readfile: " << file.path << " " << file.name << " " << file.is_dir << " " << file.is_reg;
        engine.logLine(kMethcla_LogDebug, s.str());
        if (!file.is_dir) {
            try {
                engine.logLine(kMethcla_LogDebug, std::string("Loading sound ") + path + "/" + std::string(file.name));
                result.push_back(Sound(engine, path + "/" + std::string(file.name)));
            } catch (std::exception& e) {
                std::stringstream s;
                s << "Exception while registering sound "
                  << file.name << ": " << e.what();
                engine.logLine(kMethcla_LogError, s.str());
            }
        }
        tinydir_next(&dir);
    }

    tinydir_close(&dir);

    return result;
}

Engine::Engine(Options inOptions)
    : m_engine(nullptr)
    , m_useDisk(false)
{
    Methcla::EngineOptions options(inOptions.engineOptions);
    options.audioDriver.bufferSize = 256;
    options.addLibrary(methcla_plugins_node_control)
           .addLibrary(methcla_plugins_patch_cable)
           .addLibrary(methcla_plugins_sampler)
           .addLibrary(methcla_plugins_disksampler);

    // Create the engine with a set of plugins.
    m_engine = new Methcla::Engine(options, inOptions.audioDriver);

    if (!inOptions.soundDir.empty())
        m_sounds = loadSounds(*m_engine, inOptions.soundDir);
    for (auto file : inOptions.sounds)
        m_sounds.push_back(Sound(*m_engine, file));

    // Start the engine.
    engine().start();

    m_voiceGroup = engine().group(engine().root());

//    for (auto bus : { 0, 1 })
//    {
//        Methcla::Request request(engine());
//        request.openBundle(Methcla::immediately);
//        auto synth = request.synth(METHCLA_PLUGINS_PATCH_CABLE_URI, engine().root(), {});
//        request.activate(synth);
//        request.mapInput(synth, 0, Methcla::AudioBusId(bus));
//        request.mapOutput(synth, 0, Methcla::AudioBusId(bus), Methcla::kBusMappingExternal);
//        request.closeBundle();
//        request.send();
//        m_patchCables.push_back(synth);
//    }
}

Engine::~Engine()
{
    engine().free(m_voiceGroup);
    for (auto synth : m_patchCables) {
        engine().free(synth);
    }
    delete m_engine;
}

size_t Engine::numSounds() const
{
    return m_sounds.size();
}

void Engine::useDisk(bool flag)
{
    if (flag && !Methcla::Version::isPro())
    {
        m_engine->logLine(kMethcla_LogError, "Disk streaming is only available in Methcla Pro!");
    }
    else
    {
        m_useDisk = flag;
    }
}

static Methcla_Time kLatency = 0.1;

static float mapRate(float value)
{
#if 0
    const float numOctaves = 4.f;
    return expmap(1.f/numOctaves, numOctaves, 0.f, 1.f, value);
#else
    return 1.f;
#endif
}

void Engine::startVoice(VoiceId voice, size_t soundIndex, float amp, float rate)
{
    if (m_voices.find(voice) != m_voices.end())
        stopVoice(voice);

    if (soundIndex < m_sounds.size())
    {
        const Sound& sound = m_sounds[soundIndex];
        Methcla::Request request(engine());
        request.openBundle(Methcla::immediately);
            // Allocate two buses
            Methcla::AudioBusId bus1 = m_engine->audioBusId().alloc();
            Methcla::AudioBusId bus2 = m_engine->audioBusId().alloc();

            // Create synth and map outputs to buses
            const Methcla::SynthId synth = request.synth(
                m_useDisk
                    ? METHCLA_PLUGINS_DISKSAMPLER_URI
                    : METHCLA_PLUGINS_SAMPLER_URI,
                m_voiceGroup,
                { amp, mapRate(rate) },
                { Methcla::Value(sound.path())
                , Methcla::Value(true) }
            );
            request.mapOutput(synth, 0, bus1);
            request.mapOutput(synth, 1, bus2);

            // Envelope options
            const std::list<Methcla::Value> envOptions =
                { Methcla::Value(0.05f)
                , Methcla::Value(1.f)
                , Methcla::Value(1.f)
                , Methcla::Value(1.5f)
                };

            auto envelope1 = request.synth(METHCLA_PLUGINS_ASR_ENVELOPE_URI, m_voiceGroup, {}, envOptions);
            request.mapInput(envelope1, 0, bus1);
            request.mapOutput(envelope1, 0, Methcla::AudioBusId(0), Methcla::kBusMappingExternal);
            request.whenDone(envelope1, Methcla::kNodeDoneFreeSelf|Methcla::kNodeDoneFreePreceeding);

            auto envelope2 = request.synth(METHCLA_PLUGINS_ASR_ENVELOPE_URI, m_voiceGroup, {}, envOptions);
            request.mapInput(envelope2, 0, bus2);
            request.mapOutput(envelope2, 0, Methcla::AudioBusId(1), Methcla::kBusMappingExternal);
            request.whenDone(envelope2, Methcla::kNodeDoneFreeSelf);

            request.openBundle(engine().currentTime() + kLatency);
                request.activate(synth);
                request.activate(envelope1);
                request.activate(envelope2);
            request.closeBundle();
        request.closeBundle();

        m_engine->addNotificationHandler(m_engine->freeNodeIdHandler(synth, [this](Methcla::NodeId nodeId){
            std::stringstream s;
            s << "Freed " << nodeId.id();
            m_engine->logLine(kMethcla_LogDebug, s.str());
        }));
        m_engine->addNotificationHandler(m_engine->freeNodeIdHandler(envelope1, [this](Methcla::NodeId nodeId){
            std::stringstream s;
            s << "Freed " << nodeId.id();
            m_engine->logLine(kMethcla_LogDebug, s.str());
        }));
        m_engine->addNotificationHandler(m_engine->freeNodeIdHandler(envelope2, [this,bus1,bus2](Methcla::NodeId nodeId){
            std::stringstream s;
            s << "Freed " << nodeId.id();
            m_engine->logLine(kMethcla_LogDebug, s.str());
            m_engine->audioBusId().free(bus1);
            m_engine->audioBusId().free(bus2);
        }));

        request.send();
//        m_voices[voice] = synth;
        // std::cout << "Synth " << synth.id()
        //           << sound.path()
        //           << " duration=" << sound.duration()
        //           << " param=" << param
        //           << " rate=" << mapRate(param)
        //           << std::endl;
    }
}

void Engine::updateVoice(VoiceId voice, float amp, float rate)
{
//    auto it = m_voices.find(voice);
//    assert( it != m_voices.end() );
//    const float rate = mapRate(param);
//    m_engine->set(it->second, 1, rate);
//    std::cout << "Synth " << it->second.id()
//              << " param=" << param
//              << " rate=" << rate
//              << std::endl;
}

void Engine::stopVoice(VoiceId voice)
{
//    auto it = m_voices.find(voice);
//    if (it != m_voices.end()) {
//        Methcla::Request request(engine());
//        request.openBundle(engine().currentTime() + kLatency);
//        request.free(it->second);
//        request.closeBundle();
////        request.send();
//        m_voices.erase(it);
//    }
}
