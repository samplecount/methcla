#ifndef MESCALINE_AUDIO_SYNTH_HPP_INCLUDED
#define MESCALINE_AUDIO_SYNTH_HPP_INCLUDED

#include <Mescaline/Audio/AudioBus.hpp>
#include <Mescaline/Audio/Engine.hpp>
#include <Mescaline/Audio/Plugin/API.h>

#include <boost/intrusive/list.hpp>
#include <boost/utility.hpp>

namespace Mescaline { namespace Audio {

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
        , m_type(type)
    { }

    size_t index() const { return m_index; }
    const BusId& busId() const { return m_busId; }
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

class AudioInputConnection : public Connection<AudioBusId, InputConnectionType>
{
public:
    AudioInputConnection(size_t index)
        : Connection<AudioBusId,InputConnectionType>(index, kIn)
    { }

    void read(Environment& env, size_t numFrames, sample_t* dst)
    {
        if (busId()) {
            AudioBus& bus = env.audioBus(busId());
            bus.lockForReading();
            // TODO: Exception handling, scoped lock
            if (bus.epoch() == env.epoch()) {
                memcpy(dst, bus.data(), numFrames * sizeof(sample_t));
            } else {
                memset(dst, 0, numFrames * sizeof(sample_t));
            }
            bus.unlockForReading();
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
        if (busId()) {
            AudioBus& bus = env.audioBus(busId());
            const Epoch epoch = env.epoch();
            bus.lockForWriting();
            // TODO: Exception handling, scoped lock
            if (bus.epoch() == epoch) {
                // Accumulate
                sample_t* dst = bus.data();
                for (size_t i=0; i < numFrames; i++) {
                    dst[i] += src[i];
                }
            } else {
                // Copy
                memcpy(bus.data(), src, numFrames * sizeof(sample_t));
                bus.setEpoch(epoch);
            }
            bus.unlockForWriting();
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
    };

    Synth( Environment& env
         , const NodeId& id
         , Group* parent
         , const SynthDef& synthDef
         , MescalineSynth* synth
         , size_t numAudioInputs
         , AudioInputConnection* audioInputConnections
         , size_t numAudioOutputs
         , AudioOutputConnection* audioOutputConnections
         , sample_t** audioBuffers
         );

public:
    static Synth* construct(Environment& env, const NodeId& id, Group* parent, const SynthDef& synthDef);
    virtual void free();
    virtual ~Synth();

    /// Return this synth's SynthDef.
    const SynthDef& synthDef() const { return m_synthDef; }

    /// Return number of inputs.
    size_t numAudioInputs() const { return m_numAudioInputs; }
    /// Map input to bus.
    void mapInput(size_t input, const AudioBusId& bus, InputConnectionType type);

    // Return number of outputs.
    size_t numAudioOutputs() const { return m_numAudioOutputs; }
    // Map output to bus.
    void mapOutput(size_t output, const AudioBusId& bus, OutputConnectionType type);

    typedef boost::intrusive::list<AudioInputConnection>  AudioInputConnections;
    typedef boost::intrusive::list<AudioOutputConnection> AudioOutputConnections;
    // typedef boost::container::vector<Connection<ControlBus, InputConnectionType> > ControlInputConnections;
    // typedef boost::container::vector<Connection<ControlBus, OutputConnectionType> > ControlOutputConnections;

    /// Sample offset for sample accurate synth scheduling.
    size_t sampleOffset() const { return 0; }

    /// Sets up inputs and outputs and calls compute.
    virtual void process(size_t numFrames);

    template <class T> T* synth() { return reinterpret_cast<T*>(m_synth); }

private:
    const SynthDef&         m_synthDef;
    std::bitset<32>         m_flags;
    MescalineSynth*         m_synth;
    size_t                  m_numAudioInputs;
    size_t                  m_numAudioOutputs;
    // size_t                  m_numControlInputs;
    // size_t                  m_numControlOutputs;
    // AudioBusId              m_audioInputBus;
    // AudioBusId              m_audioOutputBus;
    AudioInputConnections   m_audioInputConnections;
    AudioOutputConnections  m_audioOutputConnections;
    sample_t**              m_audioBuffers;
};

}; };

#endif // MESCALINE_AUDIO_SYNTH_HPP_INCLUDED
