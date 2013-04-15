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
#include "Methcla/Audio/Engine.hpp"

#include <boost/intrusive/list.hpp>
#include <boost/utility.hpp>

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
                 , public boost::intrusive::list_base_hook<>
{
public:
    Connection(size_t index, ConnectionType type)
        : m_index(index)
        , m_busId(-1)
        , m_type(type)
    { }

    size_t index() const { return m_index; }
    BusId busId() const { return m_busId; }
    const ConnectionType& type() const { return m_type; }

    bool connect(const BusId& busId, const ConnectionType& type)
    {
        bool changed = false;
        if (busId != m_busId) {
            m_busId = busId;
            changed = true;
        }
        m_type = type;
        return changed;
    }

private:
    size_t          m_index;
    BusId           m_busId;
    ConnectionType  m_type;
};

class AudioInputConnection : public Connection<AudioBusId,InputConnectionType>
{
public:
    AudioInputConnection(size_t index)
        : Connection<AudioBusId,InputConnectionType>(index, kIn)
    { }

    void read(Environment& env, size_t numFrames, sample_t* dst)
    {
        AudioBus* bus = env.audioBus(busId());

        if (bus != nullptr) {
            std::lock_guard<AudioBus::Lock> lock(bus->lock());
            if ((type() == kInFeedback) || (bus->epoch() == env.epoch())) {
                memcpy(dst, bus->data(), numFrames * sizeof(sample_t));
            } else {
                memset(dst, 0, numFrames * sizeof(sample_t));
            }
        } else {
            memset(dst, 0, numFrames * sizeof(sample_t));
        }
    }
};

class AudioOutputConnection : public Connection<AudioBusId,OutputConnectionType>
{
public:
    AudioOutputConnection(size_t index)
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
        AudioBus* bus = env.audioBus(busId());

        if (bus != nullptr) {
            const Epoch epoch = env.epoch();
            std::lock_guard<AudioBus::Lock> lock(bus->lock());
            if ((type() != kReplaceOut) && (bus->epoch() == epoch)) {
                // Accumulate
                sample_t* dst = bus->data();
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

private:
    size_t      m_offset;
    sample_t*   m_buffer;
};

class Synth : public Node
{
protected:
    enum Flags
    {
        kAudioInputConnectionsChanged
      , kAudioOutputConnectionsChanged
      , kControlInputConnectionsChanged
      , kControlOutputConnectionsChanged
      , kHasTriggerInput
    };

    Synth( Environment& env
         , Group* target
         , AddAction addAction
         , const Plugin::Plugin& synthDef
         , size_t synthOffset
         , size_t audioInputConnectionsOffset
         , size_t audioOutputConnectionsOffset
         , size_t controlBufferOffset
         , size_t audioBufferOffset
         );
    virtual ~Synth();

public:
    static Synth* construct(Environment& env, Group* target, Node::AddAction addAction, const Plugin::Plugin& synthDef);

    virtual bool isSynth() const override { return true; }

    const Plugin::Plugin& synthDef() const { return m_synthDef; }

    /// Map input to bus.
    void mapInput(size_t input, const AudioBusId& busId, InputConnectionType type);

    // Map output to bus.
    void mapOutput(size_t output, const AudioBusId& busId, OutputConnectionType type);

    typedef boost::intrusive::list<AudioInputConnection>  AudioInputConnections;
    typedef boost::intrusive::list<AudioOutputConnection> AudioOutputConnections;
    // typedef boost::container::vector<Connection<ControlBus, InputConnectionType> > ControlInputConnections;
    // typedef boost::container::vector<Connection<ControlBus, OutputConnectionType> > ControlOutputConnections;

    size_t numControlInputs() const { return m_synthDef.numControlInputs(); }
    size_t numControlOutputs() const { return m_synthDef.numControlOutputs(); }

    float controlInput(size_t index) const
    {
        BOOST_ASSERT_MSG( index < numControlInputs(), "control input index out of range" );
        return m_controlBuffers[index];
    }

    float& controlInput(size_t index)
    {
        BOOST_ASSERT_MSG( index < numControlInputs(), "control input index out of range" );
        return m_controlBuffers[index];
    }

    float controlOutput(size_t index) const
    {
        BOOST_ASSERT_MSG( index < numControlOutputs(), "control output index out of range" );
        return m_controlBuffers[numControlInputs() + index];
    }

    /// Sample offset for sample accurate synth scheduling.
    size_t sampleOffset() const { return 0; }

    /// Sets up inputs and outputs and calls compute.
    virtual void process(size_t numFrames);

    template <class T> T* synth() { return static_cast<T*>(m_synth); }

private:
    const Plugin::Plugin&   m_synthDef;
    std::bitset<32>         m_flags;
    LV2_Handle              m_synth;
    AudioInputConnections   m_audioInputConnections;
    AudioOutputConnections  m_audioOutputConnections;
    sample_t*               m_controlBuffers;
    sample_t*               m_audioBuffers;
};

}; };

#endif // METHCLA_AUDIO_SYNTH_HPP_INCLUDED
