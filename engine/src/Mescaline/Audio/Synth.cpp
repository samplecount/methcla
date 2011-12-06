#include <Mescaline/Audio/Synth.hpp>

using namespace Mescaline::Audio;

Synth::Synth( Environment& env
            , const NodeId& id
            , Group* parent
            , const SynthDef& synthDef
            , MescalineSynth* synth
            , size_t numAudioInputs
            , AudioInputConnection* audioInputConnections
            , size_t numAudioOutputs
            , AudioOutputConnection* audioOutputConnections
            , sample_t** audioBuffers
            )
    : Node(env, id, parent)
    , m_synthDef(synthDef)
    , m_synth(synth)
    , m_numAudioInputs(numAudioInputs)
    , m_numAudioOutputs(numAudioOutputs)
    , m_audioBuffers(audioBuffers)
{
    for (size_t i=0; i < numAudioInputs; i++) {
        m_audioInputConnections.push_back(audioInputConnections[i]);
    }
    for (size_t i=0; i < numAudioOutputs; i++) {
        m_audioOutputConnections.push_back(audioOutputConnections[i]);
    }
}

Synth::~Synth()
{
    // Call destructors
}

Synth* Synth::construct(Environment& env, const NodeId& id, Group* parent, const SynthDef& synthDef)
{
    const Alignment bufferAlignment(Alignment::SIMDAlignment());
    BOOST_ASSERT_MSG( bufferAlignment.isAligned(env.blockSize() * sizeof(sample_t))
                    , "blockSize must be aligned to Alignment::kSIMDAlignment" );

    const size_t numAudioInputs = synthDef.numAudioInputs();
    const size_t numAudioOutputs = synthDef.numAudioOutputs();
    const size_t blockSize = env.blockSize();

    const size_t synthAllocSize = sizeof(Synth) + synthDef.instanceSize();
    const size_t audioInputOffset = synthAllocSize;
    const size_t audioInputAllocSize = numAudioInputs * sizeof(AudioInputConnection);
    const size_t audioOutputOffset = audioInputOffset + audioInputAllocSize;
    const size_t audioOutputAllocSize = numAudioOutputs * sizeof(AudioOutputConnection);
    const size_t audioBufferOffset = audioOutputOffset + audioOutputAllocSize;
    const size_t audioBufferAllocSize = (numAudioInputs + numAudioOutputs) * sizeof(sample_t*);
    const size_t bufferDataOffset = bufferAlignment.align(audioBufferOffset + audioBufferAllocSize);
    const size_t bufferDataAllocSize = (numAudioInputs + numAudioOutputs) * blockSize * sizeof(sample_t);
    const size_t allocSize = bufferDataOffset + bufferDataAllocSize;
    
    // Need to align the alloc'd memory here so the audio buffer memory turns out aligned, too.
    char* mem = env.rtMem().allocAligned<char>(bufferAlignment, allocSize);
    
    AudioInputConnection* audioInputConnections = (AudioInputConnection*)(mem + audioInputOffset);
    AudioOutputConnection* audioOutputConnections = (AudioOutputConnection*)(mem + audioOutputOffset);
    sample_t** audioInputBuffers = (sample_t**)(mem + audioBufferOffset);
    sample_t** audioOutputBuffers = audioInputBuffers + numAudioInputs;
    sample_t* inputBufferData = (sample_t*)(mem + bufferDataOffset);
    sample_t* outputBufferData = inputBufferData + numAudioInputs * blockSize;

    // Initialize shtuff
    for (size_t i=0; i < numAudioInputs; i++) {
        new (audioInputConnections + i) AudioInputConnection(i);
        audioInputBuffers[i] = inputBufferData + i * blockSize;
    }
    for (size_t i=0; i < numAudioOutputs; i++) {
        new (audioOutputConnections + i) AudioOutputConnection(i);
        audioOutputBuffers[i] = outputBufferData + i * blockSize;
    }

    MescalineSynth* synth = reinterpret_cast<MescalineSynth*>(mem + sizeof(Synth));
    synthDef.construct(synth, 0, 0);

    return new (mem) Synth( env, id, parent
                          , synthDef, synth
                          , numAudioInputs, audioInputConnections
                          , numAudioOutputs, audioOutputConnections
                          , audioInputBuffers );
}

void Synth::free()
{
    Node::free<Synth>(this);
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
    sample_t** inputBuffers = m_audioBuffers;
    sample_t** outputBuffers = m_audioBuffers + numAudioInputs();

    for ( AudioInputConnections::iterator it = m_audioInputConnections.begin()
        ; it != m_audioInputConnections.end()
        ; it++ )
    {
        it->read(env, numFrames, inputBuffers[it->index()]);
    }

    (*m_synth->fProcess)(m_synth, numFrames, inputBuffers, outputBuffers);

    for ( AudioOutputConnections::iterator it = m_audioOutputConnections.begin()
        ; it != m_audioOutputConnections.end()
        ; it++ )
    {
        it->write(env, numFrames, outputBuffers[it->index()]);
    }
}
