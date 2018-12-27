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

#ifndef METHCLA_AUDIO_SYNTH_HPP_INCLUDED
#define METHCLA_AUDIO_SYNTH_HPP_INCLUDED

#include "Methcla/Audio/AudioBus.hpp"
// #include "Methcla/Audio/DSP.h"
#include "Methcla/Audio/Engine.hpp"

#include <methcla/plugin.h>

#include <cstdint>
#include <thread>

#include <oscpp/server.hpp>

namespace Methcla { namespace Audio {

    enum InputConnectionType
    {
        kIn,
        kInFeedback
    };

    enum OutputConnectionType
    {
        kOut,
        kReplaceOut
    };

    class Synth;

    template <typename Bus> class Connection
    {
        Methcla_PortCount       m_index;
        Methcla_BusMappingFlags m_flags;
        Bus*                    m_bus;

    public:
        Connection(Methcla_PortCount index)
        : m_index(index)
        , m_flags(kMethcla_BusMappingInternal)
        , m_bus(nullptr)
        {}

        Methcla_PortCount index() const { return m_index; }

        bool connect(Bus* bus, Methcla_BusMappingFlags flags)
        {
            bool changed = false;
            if (bus != m_bus)
            {
                m_bus = bus;
                changed = true;
            }
            m_flags = flags;
            return changed;
        }

    protected:
        Methcla_BusMappingFlags flags() const { return m_flags; }
        Bus*                    bus() { return m_bus; }
    };

    class AudioInputConnection : public Connection<AudioBus>
    {
    public:
        AudioInputConnection(Methcla_PortCount index)
        : Connection<AudioBus>(index)
        {}

        void read(const Environment& env, size_t numFrames, sample_t* dst,
                  size_t offset = 0)
        {
            if (bus() != nullptr)
            {
                // std::lock_guard<AudioBus::Lock> lock(bus->lock());
                if (((flags() & kMethcla_BusMappingExternal) ==
                     kMethcla_BusMappingExternal) ||
                    ((flags() & kMethcla_BusMappingFeedback) ==
                     kMethcla_BusMappingFeedback) ||
                    (bus()->epoch() == env.epoch()))
                {
                    const sample_t* buffer = bus()->data();
                    std::copy(buffer + offset, buffer + offset + numFrames,
                              dst);
                }
                else
                {
                    memset(dst, 0, numFrames * sizeof(sample_t));
                }
            }
            else
            {
                memset(dst, 0, numFrames * sizeof(sample_t));
            }
        }
    };

    class AudioOutputConnection : public Connection<AudioBus>
    {
    public:
        AudioOutputConnection(Methcla_PortCount index)
        : Connection<AudioBus>(index)
        {}

        void write(const Environment& env, size_t numFrames,
                   const sample_t* src, size_t offset = 0)
        {
            if (bus() != nullptr)
            {
                sample_t* buffer = bus()->data();
                if (bus()->epoch() == env.epoch())
                { // Bus has been written to in this epoch
                    if ((flags() & kMethcla_BusMappingReplace) ==
                        kMethcla_BusMappingReplace)
                    { // Replace
                        std::copy(src, src + numFrames, buffer + offset);
                    }
                    else
                    { // Accumulate
                        sample_t* dst = buffer + offset;
                        // accumulate(dst, src, numFrames);
                        for (size_t i = 0; i < numFrames; i++)
                        {
                            dst[i] += src[i];
                        }
                    }
                }
                else
                { // Bus hasn't been written in this epoch
                    // Assign
                    memset(buffer, 0, offset * sizeof(sample_t));
                    std::copy(src, src + numFrames, buffer + offset);
                    bus()->setEpoch(env.epoch());
                }
            }
        }
    };

    class Synth : public Node
    {
    protected:
        Synth(Environment& env, NodeId nodeId, const SynthDef& synthDef,
              Methcla_PortCount numControlInputs,
              Methcla_PortCount numControlOutputs,
              Methcla_PortCount numAudioInputs,
              Methcla_PortCount numAudioOutputs, Methcla_Synth* synth,
              AudioInputConnection*  audioInputConnections,
              AudioOutputConnection* audioOutputConnections,
              sample_t* controlBuffers, sample_t* audioBuffers);
        ~Synth();

        void         construct(const Methcla_SynthOptions* synthOptions);
        void         connectPorts(const Methcla_SynthOptions* synthOptions,
                                  OSCPP::Server::ArgStream    controls);
        virtual void doProcess(size_t numFrames) override;

    public:
        static Synth* construct(Environment& env, NodeId nodeId,
                                const SynthDef&          synthDef,
                                OSCPP::Server::ArgStream controls,
                                OSCPP::Server::ArgStream args);

        // Convert Methcla_Synth to Synth.
        static Synth* fromSynth(Methcla_Synth* synth);

        virtual bool isSynth() const override { return true; }

        //* Return this synth's SynthDef.
        const SynthDef& synthDef() const { return m_synthDef; }

        //* Return number of audio inputs.
        Methcla_PortCount numAudioInputs() const { return m_numAudioInputs; }

        //* Map input to bus.
        void mapInput(Methcla_PortCount input, const AudioBusId& busId,
                      Methcla_BusMappingFlags flags);

        //* Return number of audio outputs.
        Methcla_PortCount numAudioOutputs() const { return m_numAudioOutputs; }

        //* Map output to bus.
        void mapOutput(Methcla_PortCount output, const AudioBusId& busId,
                       Methcla_BusMappingFlags flags);

        Methcla_PortCount numControlInputs() const
        {
            return m_numControlInputs;
        }
        Methcla_PortCount numControlOutputs() const
        {
            return m_numControlOutputs;
        }

        float controlInput(Methcla_PortCount index) const
        {
            assert(index < numControlInputs());
            return m_controlBuffers[index];
        }

        float& controlInput(Methcla_PortCount index)
        {
            assert(index < numControlInputs());
            return m_controlBuffers[index];
        }

        float controlOutput(Methcla_PortCount index) const
        {
            assert(index < numControlOutputs());
            return m_controlBuffers[numControlInputs() + index];
        }

        //* Activate synth.
        void activate(double sampleOffset = 0.);

        /// Sample offset for sample accurate synth scheduling.
        float sampleOffset() const { return m_sampleOffset; }

    private:
        enum State
        {
            kStateInactive,
            kStateActive,
            kStateActivating
        };

        struct Flags
        {
            unsigned int state : 2;
        };

        const SynthDef&         m_synthDef;
        const Methcla_PortCount m_numControlInputs;
        const Methcla_PortCount m_numControlOutputs;
        const Methcla_PortCount m_numAudioInputs;
        const Methcla_PortCount m_numAudioOutputs;
        Flags                   m_flags;
        double                  m_sampleOffset;
        Methcla_Synth*          m_synth;
        AudioInputConnection*   m_audioInputConnections;
        AudioOutputConnection*  m_audioOutputConnections;
        sample_t*               m_controlBuffers;
        sample_t*               m_audioBuffers;
    };

}} // namespace Methcla::Audio

#endif // METHCLA_AUDIO_SYNTH_HPP_INCLUDED
