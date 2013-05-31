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

#include <boost/utility.hpp>
#include <cstdint>
#include <methcla/plugin.h>
#include <oscpp/server.hpp>
#include <thread>

namespace Methcla { namespace Audio {

enum InputConnectionType
{
    kIn
  , kInFeedback
};

enum OutputConnectionType
{
    kOut
  , kReplaceOut
};

class Synth;

template <typename BusId, typename ConnectionType>
class Connection : public boost::noncopyable
{
public:
    Connection(Methcla_PortCount index, ConnectionType type)
        : m_index(index)
        , m_busId(0)
    {
        m_flags.connected = false;
        m_flags.type = type;
    }

    Methcla_PortCount index() const { return m_index; }
    bool isConnected() const { return m_flags.connected; }
    BusId busId() const { return m_busId; }
    ConnectionType type() const { return (ConnectionType)m_flags.type; }

    bool connect(const BusId& busId, ConnectionType type)
    {
        bool changed = false;
        if (!m_flags.connected || (busId != m_busId)) {
            m_flags.connected = true;
            m_busId = busId;
            changed = true;
        }
        m_flags.type = type;
        return changed;
    }

private:
    struct Flags
    {
        bool connected : 1;
        int type       : 4;
    };

    Flags               m_flags;
    Methcla_PortCount   m_index;
    BusId               m_busId;
};

class AudioInputConnection : public Connection<AudioBusId,InputConnectionType>
{
public:
    AudioInputConnection(Methcla_PortCount index)
        : Connection<AudioBusId,InputConnectionType>(index, kIn)
    { }

    void read(Environment& env, size_t numFrames, sample_t* dst)
    {
        if (isConnected()) {
            AudioBus* bus = env.audioBus(busId());

            if (bus != nullptr) {
                // std::lock_guard<AudioBus::Lock> lock(bus->lock());
                if ((type() == kInFeedback) || (bus->epoch() == env.epoch())) {
                    memcpy(dst, bus->data(), numFrames * sizeof(sample_t));
                } else {
                    memset(dst, 0, numFrames * sizeof(sample_t));
                }
            } else {
                memset(dst, 0, numFrames * sizeof(sample_t));
            }
        }
    }
};

class AudioOutputConnection : public Connection<AudioBusId,OutputConnectionType>
{
public:
    AudioOutputConnection(Methcla_PortCount index)
        : Connection<AudioBusId,OutputConnectionType>(index, kOut)
        , m_offset(0)
        , m_buffer(0)
    { }

    bool connect(const AudioBusId& busId, const OutputConnectionType& type, size_t offset, sample_t* buffer)
    {
        BOOST_ASSERT((m_offset == 0) && (m_buffer == 0));
        m_offset = offset;
        m_buffer = buffer;
        return Connection<AudioBusId,OutputConnectionType>::connect(busId, type);
    }

    void release(Environment& env)
    {
        if (m_buffer != 0) {
            env.rtMem().free(m_buffer);
            m_offset = 0;
            m_buffer = 0;
        }
    }

    void write(Environment& env, size_t numFrames, const sample_t* src)
    {
        if (isConnected()) {
            AudioBus* bus = env.audioBus(busId());

            if (bus != nullptr) {
                const Epoch epoch = env.epoch();
                // std::lock_guard<AudioBus::Lock> lock(bus->lock());
                if ((type() != kReplaceOut) && (bus->epoch() == epoch)) {
                    // Accumulate
                    sample_t* dst = bus->data();
                    // accumulate(dst, src, numFrames);
                    for (size_t i=0; i < numFrames; i++) {
                        dst[i] += src[i];
                    }
                } else {
                    // Copy
                    memcpy(bus->data(), src, numFrames * sizeof(sample_t));
                    bus->setEpoch(epoch);
                }
            }
        }
    }

private:
    size_t      m_offset;
    sample_t*   m_buffer;
};

class Synth : public Node
{
protected:
    Synth( Environment& env
         , NodeId nodeId
         , Group* target
         , AddAction addAction
         , const SynthDef& synthDef
         , OSC::Server::ArgStream controls
         , const Methcla_SynthOptions* synthOptions
         , Methcla_PortCount numControlInputs
         , Methcla_PortCount numControlOutputs
         , Methcla_PortCount numAudioInputs
         , Methcla_PortCount numAudioOutputs
         , size_t synthOffset
         , size_t audioInputConnectionsOffset
         , size_t audioOutputConnectionsOffset
         , size_t controlBufferOffset
         , size_t audioBufferOffset
         , size_t audioBufferSize
         );
    ~Synth();

    virtual void doProcess(size_t numFrames) override;

public:
    static Synth* construct(Environment& env, NodeId nodeId, Group* target, Node::AddAction addAction, const SynthDef& synthDef, OSC::Server::ArgStream controls, OSC::Server::ArgStream args);

    virtual bool isSynth() const override { return true; }

    //* Return this synth's SynthDef.
    const SynthDef& synthDef() const { return m_synthDef; }

    //* Return synth handle.
    Methcla_Synth* asHandle() { return m_synth; }

    //* Return the synth corresponding to handle.
    static Synth* asSynth(Methcla_Synth* handle);

    //* Return number of audio inputs.
    Methcla_PortCount numAudioInputs() const { return m_numAudioInputs; }

    //* Map input to bus.
    void mapInput(Methcla_PortCount input, const AudioBusId& busId, InputConnectionType type);

    //* Return number of audio outputs.
    Methcla_PortCount numAudioOutputs() const { return m_numAudioOutputs; }

    //* Map output to bus.
    void mapOutput(Methcla_PortCount output, const AudioBusId& busId, OutputConnectionType type);

    Methcla_PortCount numControlInputs() const { return m_numControlInputs; }
    Methcla_PortCount numControlOutputs() const { return m_numControlOutputs; }

    float controlInput(Methcla_PortCount index) const
    {
        BOOST_ASSERT_MSG( index < numControlInputs(), "control input index out of range" );
        return m_controlBuffers[index];
    }

    float& controlInput(Methcla_PortCount index)
    {
        BOOST_ASSERT_MSG( index < numControlInputs(), "control input index out of range" );
        return m_controlBuffers[index];
    }

    float controlOutput(Methcla_PortCount index) const
    {
        BOOST_ASSERT_MSG( index < numControlOutputs(), "control output index out of range" );
        return m_controlBuffers[numControlInputs() + index];
    }

    /// Sample offset for sample accurate synth scheduling.
    size_t sampleOffset() const { return 0; }

private:
    // struct Flags
    // {
    //     bool audioInputConnectionsChanged       : 1;
    //     bool audioOutputConnectionsChanged      : 1;
    //     // bool controlInputConnectionsChanged     : 1;
    //     // bool controlOutputConnectionsChanged    : 1;
    //     // bool hasTriggerInput                    : 1;
    // };

    const SynthDef&         m_synthDef;
    const Methcla_PortCount m_numControlInputs;
    const Methcla_PortCount m_numControlOutputs;
    const Methcla_PortCount m_numAudioInputs;
    const Methcla_PortCount m_numAudioOutputs;
    // Flags                   m_flags;
    Methcla_Synth*          m_synth;
    AudioInputConnection*   m_audioInputConnections;
    AudioOutputConnection*  m_audioOutputConnections;
    sample_t*               m_controlBuffers;
    sample_t*               m_audioBuffers;
};

} }

#endif // METHCLA_AUDIO_SYNTH_HPP_INCLUDED
