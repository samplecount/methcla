#ifndef MESCALINE_AUDIO_AUDIOBUS_HPP_INCLUDED
#define MESCALINE_AUDIO_AUDIOBUS_HPP_INCLUDED

#include <Mescaline/Audio.hpp>
#include <Mescaline/Audio/Resource.hpp>

namespace Mescaline { namespace Audio {

class AudioBus : public Resource
{
public:
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

    typedef boost::intrusive_ptr<AudioBus> Handle;

public:
    AudioBus(Environment& env, const ResourceId& id, size_t numFrames, sample_t* data, const Epoch& epoch);
    virtual ~AudioBus();

    sample_t* data() { return m_data; }
    const Epoch& epoch() const { return m_epoch; }

    Lock& lock() { return m_lock; }

    void setEpoch(const Epoch& epoch) { m_epoch = epoch; }

protected:
    void setData(sample_t* data) { m_data = data; }

private:
    Lock        m_lock;
    sample_t*   m_data;
    Epoch       m_epoch;
};

class ExternalAudioBus : public AudioBus
{
public:
    ExternalAudioBus(Environment& env, const ResourceId& id, size_t numFrames, const Epoch& epoch);
    void setData(sample_t* data) { AudioBus::setData(data); }
};

class InternalAudioBus : public AudioBus
{
public:
    InternalAudioBus(Environment& env, const ResourceId& id, size_t numFrames, const Epoch& epoch);
    virtual ~InternalAudioBus();
};

}; };

#endif // MESCALINE_AUDIO_AUDIOBUS_HPP_INCLUDED
