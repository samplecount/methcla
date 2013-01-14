#ifndef MESCALINE_AUDIO_AUDIOBUS_HPP_INCLUDED
#define MESCALINE_AUDIO_AUDIOBUS_HPP_INCLUDED

#include <Mescaline/Audio.hpp>
#include <Mescaline/Audio/Resource.hpp>

#include <boost/serialization/strong_typedef.hpp>

namespace Mescaline { namespace Audio {

BOOST_STRONG_TYPEDEF(uint32_t, AudioBusId);

class AudioBus : public Resource<AudioBusId>
{
public:
    typedef AudioBusId Id;

    class Lock
    {
    public:
        void lock() { }
        void try_lock() { }
        void unlock() { }

        void lock_shared() { }
        bool try_lock_shared() { return true; }
        void unlock_shared() { }
    };

    // typedef boost::intrusive_ptr<AudioBus> Handle;

public:
    AudioBus(Environment& env, const AudioBusId& id, size_t numFrames, sample_t* data, const Epoch& epoch);
    virtual ~AudioBus();

    Lock& lock() { return m_lock; }

    const Epoch& epoch() const { return m_epoch; }
    void setEpoch(const Epoch& epoch) { m_epoch = epoch; }

    sample_t* data() { return m_data; }

protected:
    void setData(sample_t* data) { m_data = data; }

private:
    Lock        m_lock;
    Epoch       m_epoch;
    sample_t*   m_data;
};

class ExternalAudioBus : public AudioBus
{
public:
    ExternalAudioBus(Environment& env, const AudioBusId& id, size_t numFrames, const Epoch& epoch);
    void setData(sample_t* data) { AudioBus::setData(data); }
};

class InternalAudioBus : public AudioBus
{
public:
    InternalAudioBus(Environment& env, const AudioBusId& id, size_t numFrames, const Epoch& epoch);
    virtual ~InternalAudioBus();
};

}; };

#endif // MESCALINE_AUDIO_AUDIOBUS_HPP_INCLUDED
