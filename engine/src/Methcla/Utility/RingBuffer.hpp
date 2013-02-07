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