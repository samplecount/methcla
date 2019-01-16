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

#include "Methcla/Audio/Synth.hpp"

#include <boost/type_traits/alignment_of.hpp>

#include <algorithm>

using namespace Methcla::Audio;
using namespace Methcla::Memory;

static const Alignment kBufferAlignment = kSIMDAlignment;

Synth::Synth(Environment& env, NodeId nodeId, const SynthDef& synthDef,
             Methcla_PortCount numControlInputs,
             Methcla_PortCount numControlOutputs,
             Methcla_PortCount numAudioInputs,
             Methcla_PortCount numAudioOutputs, Methcla_Synth* synth,
             AudioInputConnection*  audioInputConnections,
             AudioOutputConnection* audioOutputConnections,
             sample_t* controlBuffers, sample_t* audioBuffers)
: Node(env, nodeId)
, m_synthDef(synthDef)
, m_numControlInputs(numControlInputs)
, m_numControlOutputs(numControlOutputs)
, m_numAudioInputs(numAudioInputs)
, m_numAudioOutputs(numAudioOutputs)
, m_sampleOffset(0.)
, m_synth(synth)
, m_audioInputConnections(audioInputConnections)
, m_audioOutputConnections(audioOutputConnections)
, m_controlBuffers(controlBuffers)
, m_audioBuffers(audioBuffers)
{
    // Initialize flags
    memset(&m_flags, 0, sizeof(m_flags));
    m_flags.state = kStateInactive;

    // Align audio buffers
    m_audioBuffers = kBufferAlignment.align(audioBuffers);

    // Validate alignment
    assert(
        Alignment::isAligned(boost::alignment_of<AudioInputConnection>::value,
                             (uintptr_t)m_audioInputConnections));
    assert(
        Alignment::isAligned(boost::alignment_of<AudioOutputConnection>::value,
                             (uintptr_t)m_audioOutputConnections));
    assert(Alignment::isAligned(boost::alignment_of<sample_t>::value,
                                (uintptr_t)m_controlBuffers));
    assert(kBufferAlignment.isAligned(m_audioBuffers));
}

Synth::~Synth()
{
    Methcla_World world(env());
    m_synthDef.destroy(&world, m_synth);
}

Synth* Synth::construct(Environment& env, NodeId nodeId,
                        const SynthDef&          synthDef,
                        OSCPP::Server::ArgStream controls,
                        OSCPP::Server::ArgStream options)
{
    // Get synth options
    const Methcla_SynthOptions* synthOptions = synthDef.configure(options);

    Methcla_PortCount numControlInputs = 0;
    Methcla_PortCount numControlOutputs = 0;
    Methcla_PortCount numAudioInputs = 0;
    Methcla_PortCount numAudioOutputs = 0;

    // Get port counts.
    Methcla_PortDescriptor port;
    for (size_t i = 0; synthDef.portDescriptor(synthOptions, i, &port); i++)
    {
        switch (port.type)
        {
            case kMethcla_AudioPort:
                switch (port.direction)
                {
                    case kMethcla_Input:
                        numAudioInputs++;
                        break;
                    case kMethcla_Output:
                        numAudioOutputs++;
                        break;
                }
                break;
            case kMethcla_ControlPort:
                switch (port.direction)
                {
                    case kMethcla_Input:
                        numControlInputs++;
                        break;
                    case kMethcla_Output:
                        numControlOutputs++;
                        break;
                }
        }
    }

    // const size_t numControlInputs           = synthDef.numControlInputs();
    // const size_t numControlOutputs          = synthDef.numControlOutputs();
    // const size_t numAudioInputs             = synthDef.numAudioInputs();
    // const size_t numAudioOutputs            = synthDef.numAudioOutputs();
    const size_t blockSize = env.blockSize();

    const size_t synthAllocSize = sizeof(Synth) + synthDef.instanceSize();
    const size_t audioInputOffset = synthAllocSize;
    const size_t audioInputAllocSize =
        numAudioInputs * sizeof(AudioInputConnection);
    const size_t audioOutputOffset = audioInputOffset + audioInputAllocSize;
    const size_t audioOutputAllocSize =
        numAudioOutputs * sizeof(AudioOutputConnection);
    const size_t controlBufferOffset = audioOutputOffset + audioOutputAllocSize;
    const size_t controlBufferAllocSize =
        (numControlInputs + numControlOutputs) * sizeof(sample_t);
    const size_t audioBufferOffset =
        controlBufferOffset + controlBufferAllocSize;
    const size_t audioBufferAllocSize =
        (numAudioInputs + numAudioOutputs) * blockSize * sizeof(sample_t);
    const size_t allocSize = audioBufferOffset + audioBufferAllocSize +
                             kBufferAlignment /* alignment margin */;

    char* mem = env.rtMem().allocOf<char>(allocSize);

    // Instantiate synth
    Synth* synth = new (mem)
        Synth(env, nodeId, synthDef, numControlInputs, numControlOutputs,
              numAudioInputs, numAudioOutputs,
              reinterpret_cast<Methcla_Synth*>(mem + sizeof(Synth)),
              reinterpret_cast<AudioInputConnection*>(mem + audioInputOffset),
              reinterpret_cast<AudioOutputConnection*>(mem + audioOutputOffset),
              reinterpret_cast<sample_t*>(mem + controlBufferOffset),
              reinterpret_cast<sample_t*>(mem + audioBufferOffset));

    // Construct synth
    synth->construct(synthOptions);

    // Connect ports
    synth->connectPorts(synthOptions, controls);

    return synth;
}

Synth* Synth::fromSynth(Methcla_Synth* synth)
{
    // NOTE: This needs to be adapted if Synth memory layout is changed!
    return reinterpret_cast<Synth*>(static_cast<char*>(synth) - sizeof(Synth));
}

void Synth::construct(const Methcla_SynthOptions* synthOptions)
{
    Methcla_World world(env());
    m_synthDef.construct(&world, synthOptions, m_synth);
}

void Synth::connectPorts(const Methcla_SynthOptions* synthOptions,
                         OSCPP::Server::ArgStream    controls)
{
    Methcla_PortDescriptor port;
    Methcla_PortCount      controlInputIndex = 0;
    Methcla_PortCount      controlOutputIndex = 0;
    Methcla_PortCount      audioInputIndex = 0;
    Methcla_PortCount      audioOutputIndex = 0;
    for (size_t i = 0; m_synthDef.portDescriptor(synthOptions, i, &port); i++)
    {
        switch (port.type)
        {
            case kMethcla_ControlPort:
                switch (port.direction)
                {
                    case kMethcla_Input:
                    {
                        // Initialize with control value
                        m_controlBuffers[controlInputIndex] =
                            controls.next<float>();
                        sample_t* buffer = &m_controlBuffers[controlInputIndex];
                        m_synthDef.connect(m_synth, i, buffer);
                        controlInputIndex++;
                    };
                    break;
                    case kMethcla_Output:
                    {
                        sample_t* buffer =
                            &m_controlBuffers[numControlInputs() +
                                              controlOutputIndex];
                        m_synthDef.connect(m_synth, i, buffer);
                        controlOutputIndex++;
                    };
                    break;
                };
                break;
            case kMethcla_AudioPort:
                switch (port.direction)
                {
                    case kMethcla_Input:
                    {
                        new (&m_audioInputConnections[audioInputIndex])
                            AudioInputConnection(audioInputIndex);
                        sample_t* buffer = m_audioBuffers +
                                           audioInputIndex * env().blockSize();
                        assert(kBufferAlignment.isAligned(buffer));
                        m_synthDef.connect(m_synth, i, buffer);
                        audioInputIndex++;
                    };
                    break;
                    case kMethcla_Output:
                    {
                        new (&m_audioOutputConnections[audioOutputIndex])
                            AudioOutputConnection(audioOutputIndex);
                        sample_t* buffer =
                            m_audioBuffers +
                            (numAudioInputs() + audioOutputIndex) *
                                env().blockSize();
                        assert(kBufferAlignment.isAligned(buffer));
                        m_synthDef.connect(m_synth, i, buffer);
                        audioOutputIndex++;
                    };
                    break;
                };
                break;
        }
    }
}

template <class T> struct IfIndex
{
    IfIndex(Methcla_PortCount index)
    : m_index(index)
    {}

    inline bool operator()(const T& x) const
    {
        return x.index() == m_index;
    }

private:
    Methcla_PortCount m_index;
};

template <class T> struct ByBusId
{
    inline bool operator()(const T& a, const T& b) const
    {
        return a.busId() < b.busId();
    }
};

void Synth::mapInput(Methcla_PortCount index, const AudioBusId& busId,
                     Methcla_BusMappingFlags flags)
{
    AudioInputConnection* const begin = m_audioInputConnections;
    AudioInputConnection* const end = begin + numAudioInputs();

    AudioInputConnection* conn =
        std::find_if(begin, end, IfIndex<AudioInputConnection>(index));

    if (conn != end)
    {
        AudioBus* bus = flags & kMethcla_BusMappingExternal
                            ? env().externalAudioInput(busId)
                            : env().audioBus(busId);
        conn->connect(bus, flags);
    }
}

void Synth::mapOutput(Methcla_PortCount index, const AudioBusId& busId,
                      Methcla_BusMappingFlags flags)
{
    AudioOutputConnection* const begin = m_audioOutputConnections;
    AudioOutputConnection* const end = begin + numAudioOutputs();

    AudioOutputConnection* conn =
        std::find_if(begin, end, IfIndex<AudioOutputConnection>(index));

    if (conn != end)
    {
        AudioBus* bus = flags & kMethcla_BusMappingExternal
                            ? env().externalAudioOutput(busId)
                            : env().audioBus(busId);
        conn->connect(bus, flags);
    }
}

void Synth::activate(double sampleOffset)
{
    if (m_flags.state == kStateInactive)
    {
        m_sampleOffset = sampleOffset;
        Methcla_World world(env());
        m_synthDef.activate(&world, m_synth);
        m_flags.state = kStateActivating;
    }
}

void Synth::doProcess(size_t numFrames)
{
    // Sort connections by bus id (if necessary)
    // Only needed for bus locking protocol in a parallel implementation
    // if (m_flags.audioInputConnectionsChanged) {
    //     m_flags.audioInputConnectionsChanged = false;
    //     std::sort( m_audioInputConnections
    //              , m_audioInputConnections + numAudioInputs()
    //              , ByBusId<AudioInputConnection>() );
    // }
    // if (m_flags.audioOutputConnectionsChanged) {
    //     m_flags.audioOutputConnectionsChanged = false;
    //     std::sort( m_audioOutputConnections
    //              , m_audioOutputConnections + numAudioOutputs()
    //              , ByBusId<AudioOutputConnection>() );
    // }

    Environment& env = this->env();
    const size_t blockSize = env.blockSize();

    sample_t* const inputBuffers = m_audioBuffers;
    sample_t* const outputBuffers =
        m_audioBuffers + numAudioInputs() * blockSize;

    if (m_flags.state == kStateActive)
    {
        // TODO: Iterate only over connected connections (by tracking number of
        // connections).
        for (size_t i = 0; i < numAudioInputs(); i++)
        {
            AudioInputConnection& x = m_audioInputConnections[i];
            x.read(env, numFrames, inputBuffers + x.index() * blockSize);
        }

        Methcla_World world(env);
        m_synthDef.process(&world, m_synth, numFrames);

        for (size_t i = 0; i < numAudioOutputs(); i++)
        {
            AudioOutputConnection& x = m_audioOutputConnections[i];
            x.write(env, numFrames, outputBuffers + x.index() * blockSize);
        }
        // Reset triggers
        //    if (m_flags.test(kHasTriggerInput)) {
        //        for (size_t i=0; i < numControlInputs(); i++) {
        //            if (synthDef().controlInputSpec(i).flags &
        //            kMethclaControlTrigger) {
        //                *controlInput(i) = 0.f;
        //            }
        //        }
        //    }
    }
    else if (m_flags.state == kStateActivating)
    {
        const size_t sampleOffset = std::floor(m_sampleOffset);
        assert(m_sampleOffset < (double)numFrames && sampleOffset < numFrames);
        const size_t remainingFrames = numFrames - sampleOffset;

        for (size_t i = 0; i < numAudioInputs(); i++)
        {
            AudioInputConnection& x = m_audioInputConnections[i];
            x.read(env, remainingFrames, inputBuffers + x.index() * blockSize,
                   sampleOffset);
        }

        Methcla_World world(env);
        m_synthDef.process(&world, m_synth, remainingFrames);

        for (size_t i = 0; i < numAudioOutputs(); i++)
        {
            AudioOutputConnection& x = m_audioOutputConnections[i];
            x.write(env, remainingFrames, outputBuffers + x.index() * blockSize,
                    sampleOffset);
        }

        m_flags.state = kStateActive;
    }
}
