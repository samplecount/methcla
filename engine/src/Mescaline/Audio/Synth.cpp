#include <Mescaline/Audio/Synth.hpp>

using namespace Mescaline::Audio;

Synth::Synth( Environment& env
            , const NodeId& id
            , Group* parent
            , const Plugin::Plugin& synthDef
            , LV2_Handle synth
            , AudioInputConnection* audioInputConnections
            , AudioOutputConnection* audioOutputConnections
            , sample_t* controlBuffers
            , sample_t* audioBuffers
            )
    : Node(env, id, parent)
    , m_synthDef(synthDef)
    , m_synth(synth)
    , m_controlBuffers(controlBuffers)
    , m_audioBuffers(audioBuffers)
{
    // Connect control ports
    for (size_t i=0; i < numControlInputs(); i++) {
        m_controlBuffers[i] = m_synthDef.controlInputPort(i).defaultValue();
        m_synthDef.connectPort(
            m_synth
          , m_synthDef.controlInputPort(i).index()
          , m_controlBuffers + i );
    }
    for (size_t i=0; i < numControlOutputs(); i++) {
        m_synthDef.connectPort(
            m_synth
          , m_synthDef.controlInputPort(i).index()
          , m_controlBuffers + numControlInputs() + i );
    }

    // Connect audio ports
    const size_t blockSize = env.blockSize();
    sample_t* currentAudioBuffer = m_audioBuffers;

    for (size_t i=0; i < numAudioInputs(); i++) {
        m_audioInputConnections.push_back(audioInputConnections[i]);
        m_synthDef.connectPort( m_synth
                              , m_synthDef.audioInputPort(i).index()
                              , currentAudioBuffer );
        currentAudioBuffer += blockSize;
    }

    for (size_t i=0; i < numAudioOutputs(); i++) {
        m_audioOutputConnections.push_back(audioOutputConnections[i]);
        m_synthDef.connectPort( m_synth
                              , m_synthDef.audioOutputPort(i).index()
                              , currentAudioBuffer );
        currentAudioBuffer += blockSize;
    }

    // Check for control input triggers
//    for (size_t i=0; i < numControlInputs(); i++) {
//        if (m_synthDef.controlInputSpec(i).flags & kMescalineControlTrigger) {
//            m_flags.set(kHasTriggerInput);
//            break;
//        }
//    }

    // Activate the synth
    m_synthDef.activate(m_synth);
}

Synth::~Synth()
{
    m_synthDef.deactivate(m_synth);
    m_synthDef.destroy(m_synth);
}

void* Synth::operator new(size_t, void* where)
{
    return where;
}

Synth* Synth::construct(Environment& env, const NodeId& id, Group* parent, const Plugin::Plugin& synthDef)
{
    const Alignment bufferAlignment(Alignment::SIMDAlignment());

    BOOST_ASSERT_MSG( bufferAlignment.isAligned(env.blockSize() * sizeof(sample_t))
                    , "Environment.blockSize must be aligned to Alignment::SIMDAlignment" );

    const size_t numControlInputs           = synthDef.numControlInputs();
    const size_t numControlOutputs          = synthDef.numControlOutputs();
    const size_t numAudioInputs             = synthDef.numAudioInputs();
    const size_t numAudioOutputs            = synthDef.numAudioOutputs();
    const size_t blockSize                  = env.blockSize();

    const size_t synthAllocSize             = sizeof(Synth) + synthDef.instanceSize();
    const size_t audioInputOffset           = synthAllocSize;
    const size_t audioInputAllocSize        = numAudioInputs * sizeof(AudioInputConnection);
    const size_t audioOutputOffset          = audioInputOffset + audioInputAllocSize;
    const size_t audioOutputAllocSize       = numAudioOutputs * sizeof(AudioOutputConnection);
    const size_t controlBufferOffset        = audioOutputOffset + audioOutputAllocSize;
    const size_t controlBufferAllocSize     = (numControlInputs + numControlOutputs) * sizeof(sample_t);
    const size_t audioBufferDataOffset      = bufferAlignment.align(controlBufferOffset + controlBufferAllocSize);
    const size_t audioBufferDataAllocSize   = (numAudioInputs + numAudioOutputs) * blockSize * sizeof(sample_t);
    const size_t allocSize                  = audioBufferDataOffset + audioBufferDataAllocSize;

    // Need to align the allocated memory here so the audio buffer memory turns out aligned, too.
    char* mem = env.rtMem().allocAligned<char>(bufferAlignment, allocSize);

    AudioInputConnection* audioInputConnections     = reinterpret_cast<AudioInputConnection*>(mem + audioInputOffset);
    AudioOutputConnection* audioOutputConnections   = reinterpret_cast<AudioOutputConnection*>(mem + audioOutputOffset);
    sample_t* controlBuffers                        = reinterpret_cast<sample_t*>(mem + controlBufferOffset);
    sample_t* audioBuffers                          = reinterpret_cast<sample_t*>(mem + audioBufferDataOffset);

    // Initialize audio connections
    for (size_t i=0; i < numAudioInputs; i++) {
        new (&audioInputConnections[i]) AudioInputConnection(i);
    }
    for (size_t i=0; i < numAudioOutputs; i++) {
        new (&audioOutputConnections[i]) AudioOutputConnection(i);
    }

    // Create plugin instance
    LV2_Handle synth = synthDef.construct(mem + sizeof(Synth), env.sampleRate());

    // Initialize rest of synth
    return new (mem) Synth( env, id, parent
                          , synthDef, synth
                          , audioInputConnections
                          , audioOutputConnections
                          , controlBuffers
                          , audioBuffers );
}

template <typename BusId, typename ConnType>
struct IfConnectionIndex
{
    IfConnectionIndex(size_t index)
        : m_index(index)
    { }

    bool operator () (const Connection<BusId,ConnType>& conn)
    {
        return conn.index() == m_index;
    }
    
    size_t m_index;
};

template <typename BusId, typename ConnType>
struct SortByBusId
{
    bool operator () (const Connection<BusId,ConnType>& a, const Connection<BusId,ConnType>& b)
    {
        return a.busId() < b.busId();
    }
};

// template <typename BusId, typename ConnType>
// AudioInputConnection& conn connectionAt(size_t index)
// {
//     return find_if(m_audioInputConnections.begin(), m_audioInputConnections.end(), IfConnectionIndex(index));
// }

void Synth::mapInput(size_t index, const AudioBusId& bus, InputConnectionType type)
{
    AudioInputConnections::iterator conn =
        find_if( m_audioInputConnections.begin()
               , m_audioInputConnections.end()
               , IfConnectionIndex<AudioBusId,InputConnectionType>(index) );
    if (conn != m_audioInputConnections.end()) {
        if (conn->connect(bus, type)) {
            m_flags.set(kAudioInputConnectionsChanged);
        }
    }
}

void Synth::mapOutput(size_t index, const AudioBusId& bus, OutputConnectionType type)
{
    size_t offset = sampleOffset();
    sample_t* buffer = offset > 0 ? environment().rtMem().alloc<sample_t>(offset) : 0;
    AudioOutputConnections::iterator conn =
        find_if( m_audioOutputConnections.begin()
               , m_audioOutputConnections.end()
               , IfConnectionIndex<AudioBusId,OutputConnectionType>(index) );
    if (conn != m_audioOutputConnections.end()) {
        conn->release(environment());
        if (conn->connect(bus, type, offset, buffer)) {
            m_flags.set(kAudioOutputConnectionsChanged);
        }
    }    
}

void Synth::process(size_t numFrames)
{
    // Sort connections by bus id (if necessary)
    if (m_flags.test(kAudioInputConnectionsChanged)) {
        m_audioInputConnections.sort(SortByBusId<AudioBusId,InputConnectionType>());
        m_flags.reset(kAudioInputConnectionsChanged);
    }
    if (m_flags.test(kAudioOutputConnectionsChanged)) {
        m_audioOutputConnections.sort(SortByBusId<AudioBusId,OutputConnectionType>());
        m_flags.reset(kAudioOutputConnectionsChanged);
    }

    Environment& env = environment();
    const size_t blockSize = env.blockSize();

    sample_t* const inputBuffers = m_audioBuffers;
    for ( AudioInputConnections::iterator it = m_audioInputConnections.begin()
        ; it != m_audioInputConnections.end()
        ; it++ )
    {
        it->read(env, numFrames, inputBuffers + it->index() * blockSize);
    }

    m_synthDef.run(m_synth, numFrames);

    sample_t* const outputBuffers = m_audioBuffers + numAudioInputs() * blockSize;
    for ( AudioOutputConnections::iterator it = m_audioOutputConnections.begin()
        ; it != m_audioOutputConnections.end()
        ; it++ )
    {
        it->write(env, numFrames, outputBuffers + it->index() * blockSize);
    }

    // Reset triggers
//    if (m_flags.test(kHasTriggerInput)) {
//        for (size_t i=0; i < numControlInputs(); i++) {
//            if (synthDef().controlInputSpec(i).flags & kMescalineControlTrigger) {
//                *controlInput(i) = 0.f;
//            }
//        }
//    }
}
