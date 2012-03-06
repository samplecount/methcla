#ifndef MESCALINE_AUDIO_HPP_INCLUDED
#define MESCALINE_AUDIO_HPP_INCLUDED

#include <Mescaline/Audio/Plugin/Types.h>

#include <boost/assert.hpp>
#include <boost/cstdint.hpp>
#include <boost/utility.hpp>

namespace Mescaline { namespace Audio
{
    typedef uint32_t Epoch;

    typedef uint32_t URID;

    class ResourceId
    {
    public:
        ResourceId()
            : m_id(InvalidId)
        { }
        explicit ResourceId(uint32_t id)
            : m_id(id)
        {
            BOOST_ASSERT( id != InvalidId );
        }
        ResourceId(const ResourceId& other)
            : m_id(other.m_id)
        { }

        operator uint32_t() const { return m_id; }
        operator bool() const { return m_id != InvalidId; }

        bool operator==(const ResourceId& other) const
        {
            return m_id == other.m_id;
        }

        bool operator!=(const ResourceId& other) const
        {
            return m_id != other.m_id;
        }

        bool operator<(const ResourceId& other) const
        {
            return m_id < other.m_id;
        }

        bool operator>(const ResourceId& other) const
        {
            return m_id > other.m_id;
        }

    protected:
        static const uint32_t InvalidId = 0;

    private:
        uint32_t m_id;
    };

    class Resource : boost::noncopyable
    {
    public:
        Resource(const ResourceId& id)
            : m_id(id)
        { }
        virtual ~Resource() { }

//        virtual URID typeId() const = 0;
        const ResourceId& id() const { return m_id; }

    private:
        ResourceId m_id;
    };
}; };

#endif // MESCALINE_AUDIO_HPP_INCLUDED