#ifndef MESCALINE_AUDIO_AUDIOBUS_HPP_INCLUDED
#define MESCALINE_AUDIO_AUDIOBUS_HPP_INCLUDED

#include <Mescaline/Audio.hpp>
#include <boost/cstdint.hpp>
#include <boost/utility.hpp>

namespace Mescaline { namespace Audio {

class AudioBusId
{
public:
    enum Scope
    {
        kInput = 1
      , kOutput = 2
      , kInternal = 3
    };
    
    AudioBusId()
        : m_id(InvalidId)
    { }
    AudioBusId(Scope scope, uint32_t id)
        : m_id((uint32_t(scope) & ScopeMask) | (id << ScopeBits))
    { }

    Scope scope() const { return Scope(m_id & ScopeMask); }
    uint32_t id() const { return m_id >> ScopeBits; }
    operator bool () const { return m_id != InvalidId; }

    AudioBusId operator + (size_t offset) { return AudioBusId(scope(), m_id + offset); }

protected:
    static const uint32_t InvalidId = 0;
    static const uint32_t ScopeBits = 2;
    static const uint32_t ScopeMask = 3;

private:
    uint32_t m_id;
};

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

public:
    AudioBus(const ResourceId& id, size_t numFrames, sample_t* data, const Epoch& epoch);
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
    ExternalAudioBus(const ResourceId& id, size_t numFrames, const Epoch& epoch);
    void setData(sample_t* data) { AudioBus::setData(data); }
};

class InternalAudioBus : public AudioBus
{
public:
    InternalAudioBus(const ResourceId& id, size_t numFrames, const Epoch& epoch);
    virtual ~InternalAudioBus();
};

}; };

#endif // MESCALINE_AUDIO_AUDIOBUS_HPP_INCLUDED
