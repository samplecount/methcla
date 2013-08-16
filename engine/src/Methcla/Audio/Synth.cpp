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

#include <algorithm>
#include <boost/type_traits/alignment_of.hpp>

using namespace Methcla::Audio;
using namespace Methcla::Memory;

static const Alignment kBufferAlignment = kSIMDAlignment;

Synth::Synth( Environment& env
            , NodeId nodeId
            , Group* target
            , Node::AddAction addAction
            , const SynthDef& synthDef
            , OSCPP::Server::ArgStream controls
            , const Methcla_SynthOptions* synthOptions
            , Methcla_PortCount numControlInputs
            , Methcla_PortCount numControlOutputs
            , Methcla_PortCount numAudioInputs
            , Methcla_PortCount numAudioOutputs
            , Methcla_Synth* synth
            , AudioInputConnection* audioInputConnections
            , AudioOutputConnection* audioOutputConnections
            , sample_t* controlBuffers
            , sample_t* audioBuffers
            )
    : Node(env, nodeId, target, addAction)
    , m_synthDef(synthDef)
    , m_numControlInputs(numControlInputs)
    , m_numControlOutputs(numControlOutputs)
    , m_numAudioInputs(numAudioInputs)
    , m_numAudioOutputs(numAudioOutputs)
    , m_synth(synth)
    , m_audioInputConnections(audioInputConnections)
    , m_audioOutputConnections(audioOutputConnections)
    , m_controlBuffers(controlBuffers)
    , m_audioBuffers(audioBuffers)
{
    const size_t blockSize = env.blockSize();

    // Initialize flags
    // memset(&m_flags, 0, sizeof(m_flags));

    // Align audio buffers
    m_audioBuffers = kBufferAlignment.align(audioBuffers);

    // Construct synth
    synthDef.construct(env.asWorld(), synthOptions, this, m_synth);

    // Validate alignment
    BOOST_ASSERT( Alignment::isAligned(boost::alignment_of<AudioInputConnection>::value,
                                       (uintptr_t)m_audioInputConnections) );
    BOOST_ASSERT( Alignment::isAligned(boost::alignment_of<AudioOutputConnection>::value,
                                       (uintptr_t)m_audioOutputConnections) );
    BOOST_ASSERT( Alignment::isAligned(boost::alignment_of<sample_t>::value,
                                       (uintptr_t)m_controlBuffers) );
    BOOST_ASSERT( kBufferAlignment.isAligned(m_audioBuffers) );


    // Connect ports
    Methcla_PortDescriptor port;
    Methcla_PortCount controlInputIndex  = 0;
    Methcla_PortCount controlOutputIndex = 0;
    Methcla_PortCount audioInputIndex    = 0;
    Methcla_PortCount audioOutputIndex   = 0;
    for (size_t i=0; synthDef.portDescriptor(synthOptions, i, &port); i++) {
        switch (port.type) {
        case kMethcla_ControlPort:
            switch (port.direction) {
            case kMethcla_Input: {
                // Initialize with control value
                m_controlBuffers[controlInputIndex] = controls.next<float>();
                sample_t* buffer = &m_controlBuffers[controlInputIndex];
                m_synthDef.connect(m_synth, i, buffer);
                controlInputIndex++;
                };
                break;
            case kMethcla_Output: {
                sample_t* buffer = &m_controlBuffers[numControlInputs + controlOutputIndex];
                m_synthDef.connect(m_synth, i, buffer);
                controlOutputIndex++;
                };
                break;
            };
            break;
        case kMethcla_AudioPort:
            switch (port.direction) {
            case kMethcla_Input: {
                new (&m_audioInputConnections[audioInputIndex]) AudioInputConnection(audioInputIndex);
                sample_t* buffer = m_audioBuffers + audioInputIndex * blockSize;
                BOOST_ASSERT( kBufferAlignment.isAligned(buffer) );
                m_synthDef.connect(m_synth, i, buffer);
                audioInputIndex++;
                };
                break;
            case kMethcla_Output: {
                new (&m_audioOutputConnections[audioOutputIndex]) AudioOutputConnection(audioOutputIndex);
                sample_t* buffer = m_audioBuffers + (numAudioInputs + audioOutputIndex) * blockSize;
                BOOST_ASSERT( kBufferAlignment.isAligned(buffer) );
                m_synthDef.connect(m_synth, i, buffer);
                audioOutputIndex++;
                };
                break;
            };
            break;
        }
    }

    // Check for control input triggers
//    for (size_t i=0; i < numControlInputs(); i++) {
//        if (m_synthDef.controlInputSpec(i).flags & kMethclaControlTrigger) {
//            m_flags.set(kHasTriggerInput);
//            break;
//        }
//    }

    // Activate synth instance
    // This might be deferred to when the synth is actually started by the scheduler
    synthDef.activate(env.asWorld(), m_synth);
}

Synth::~Synth()
{
    m_synthDef.destroy(env().asWorld(), m_synth);
}

Synth* Synth::construct(Environment& env, NodeId nodeId, Group* target, Node::AddAction addAction, const SynthDef& synthDef, OSCPP::Server::ArgStream controls, OSCPP::Server::ArgStream options)
{
    // TODO: This is not really necessary; each buffer could be aligned correctly, with some padding in between buffers.
    BOOST_ASSERT_MSG( kBufferAlignment.isAligned(env.blockSize() * sizeof(sample_t))
                    , "Environment.blockSize must be a multiple of kBufferAlignment" );

    const Methcla_SynthOptions* synthOptions = synthDef.configure(options);

    Methcla_PortCount numControlInputs  = 0;
    Methcla_PortCount numControlOutputs = 0;
    Methcla_PortCount numAudioInputs    = 0;
    Methcla_PortCount numAudioOutputs   = 0;

    // Get port counts.
    Methcla_PortDescriptor port;
    for (size_t i=0; synthDef.portDescriptor(synthOptions, i, &port); i++) {
        switch (port.type) {
            case kMethcla_AudioPort:
                switch (port.direction) {
                    case kMethcla_Input:
                        numAudioInputs++;
                        break;
                    case kMethcla_Output:
                        numAudioOutputs++;
                        break;
                }
                break;
            case kMethcla_ControlPort:
                switch (port.direction) {
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
    const size_t blockSize                  = env.blockSize();

    const size_t synthAllocSize             = sizeof(Synth) + synthDef.instanceSize();
    const size_t audioInputOffset           = synthAllocSize;
    const size_t audioInputAllocSize        = numAudioInputs * sizeof(AudioInputConnection);
    const size_t audioOutputOffset          = audioInputOffset + audioInputAllocSize;
    const size_t audioOutputAllocSize       = numAudioOutputs * sizeof(AudioOutputConnection);
    const size_t controlBufferOffset        = audioOutputOffset + audioOutputAllocSize;
    const size_t controlBufferAllocSize     = (numControlInputs + numControlOutputs) * sizeof(sample_t);
    const size_t audioBufferOffset          = controlBufferOffset + controlBufferAllocSize;
    const size_t audioBufferAllocSize       = (numAudioInputs + numAudioOutputs) * blockSize * sizeof(sample_t);
    const size_t allocSize                  = audioBufferOffset + audioBufferAllocSize + kBufferAlignment /* alignment margin */;

    char* mem = env.rtMem().allocOf<char>(allocSize);

    // Instantiate synth
    return new (mem) Synth( env, nodeId, target, addAction
                          , synthDef, controls, synthOptions
                          , numControlInputs
                          , numControlOutputs
                          , numAudioInputs
                          , numAudioOutputs
                          , reinterpret_cast<Methcla_Synth*>(mem + sizeof(Synth))
                          , reinterpret_cast<AudioInputConnection*>(mem + audioInputOffset)
                          , reinterpret_cast<AudioOutputConnection*>(mem + audioOutputOffset)
                          , reinterpret_cast<sample_t*>(mem + controlBufferOffset)
                          , reinterpret_cast<sample_t*>(mem + audioBufferOffset)
                          );
}

template <class T>
struct IfIndex
{
    IfIndex(Methcla_PortCount index)
        : m_index(index)
    { }

    inline bool operator () (const T& x) const
    {
        return x.index() == m_index;
    }

private:
    Methcla_PortCount m_index;
};

template <class T>
struct ByBusId
{
    inline bool operator () (const T& a, const T& b) const
    {
        return a.busId() < b.busId();
    }
};

void Synth::mapInput(Methcla_PortCount index, const AudioBusId& busId, BusMappingFlags flags)
{
    AudioInputConnection* const begin = m_audioInputConnections;
    AudioInputConnection* const end = begin + numAudioInputs();

    AudioInputConnection* conn =
        std::find_if(begin, end, IfIndex<AudioInputConnection>(index));

    if (conn != end) {
        AudioBus* bus = flags & kBusMappingExternal
                            ? env().externalAudioInput(busId)
                            : env().audioBus(busId);
        conn->connect(bus, flags);
        // if () {
        //     m_flags.audioInputConnectionsChanged = true;
        // }
    }
}

void Synth::mapOutput(Methcla_PortCount index, const AudioBusId& busId, BusMappingFlags flags)
{
    AudioOutputConnection* const begin = m_audioOutputConnections;
    AudioOutputConnection* const end = begin + numAudioOutputs();
    const size_t offset = sampleOffset();
    sample_t* buffer = offset > 0 ? env().rtMem().allocAlignedOf<sample_t>(kBufferAlignment, offset) : 0;

    AudioOutputConnection* conn =
        std::find_if(begin, end, IfIndex<AudioOutputConnection>(index));

    if (conn != end) {
        conn->release(env());
        AudioBus* bus = flags & kBusMappingExternal
                            ? env().externalAudioOutput(busId)
                            : env().audioBus(busId);
        conn->connect(bus, flags, offset, buffer);
        // if () {
        //     m_flags.audioOutputConnectionsChanged = true;
        // }
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
    // TODO: Iterate only over connected connections (by tracking number of connections).
    for (size_t i=0; i < numAudioInputs(); i++) {
        AudioInputConnection& x = m_audioInputConnections[i];
        x.read(env, numFrames, inputBuffers + x.index() * blockSize);
    }

    m_synthDef.process(env.asWorld(), m_synth, numFrames);

    sample_t* const outputBuffers = m_audioBuffers + numAudioInputs() * blockSize;
    for (size_t i=0; i < numAudioOutputs(); i++) {
        AudioOutputConnection& x = m_audioOutputConnections[i];
        x.write(env, numFrames, outputBuffers + x.index() * blockSize);
    }

    // Reset triggers
//    if (m_flags.test(kHasTriggerInput)) {
//        for (size_t i=0; i < numControlInputs(); i++) {
//            if (synthDef().controlInputSpec(i).flags & kMethclaControlTrigger) {
//                *controlInput(i) = 0.f;
//            }
//        }
//    }
}
