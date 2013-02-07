#ifndef METHCLA_UTILITY_RINGBUFFER_HPP_INCLUDED
#define METHCLA_UTILITY_RINGBUFFER_HPP_INCLUDED

#include <boost/utility.hpp>
#include <new>
#include "zix/ring.h"

namespace Methcla { namespace Utility {

class RingBuffer : boost::noncopyable
{
public:
    RingBuffer(uint32_t size)
    {
        m_impl = zix_ring_new(size);
        if (m_impl == nullptr)
        	throw std::bad_alloc();
    }
    ~RingBuffer()
    {
    	zix_ring_free(m_impl);
    }

    uint32_t capacity() const { return zix_ring_capacity(m_impl); }
    uint32_t readSpace() const { return zix_ring_read_space(m_impl); }
    uint32_t writeSpace() const { return zix_ring_write_space(m_impl); }
    uint32_t read(void* dst, uint32_t size) { return zix_ring_read(m_impl, dst, size); }
    uint32_t write(const void* src, uint32_t size) { return zix_ring_write(m_impl, src, size); }
    void* writeHead() { return zix_ring_write_head(m_impl); }

private:
    ZixRing* m_impl;
};

}; };

#endif // METHCLA_UTILITY_RINGBUFFER_HPP_INCLUDED