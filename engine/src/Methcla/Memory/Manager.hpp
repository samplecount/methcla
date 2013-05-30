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

#ifndef METHCLA_MEMORY_MANAGER_HPP_INCLUDED
#define METHCLA_MEMORY_MANAGER_HPP_INCLUDED

#include "Methcla/Exception.hpp"
#include "Methcla/Memory.hpp"

#include <boost/assert.hpp>
#include <boost/type_traits/alignment_of.hpp>
#include <cstddef>
#include <tlsf.h>

namespace Methcla { namespace Memory {

class RTMemoryManager
{
public:
    //* Construct a realtime memory allocator with a capacity of `size` kB.
    RTMemoryManager(size_t size);
    ~RTMemoryManager();

    //* Allocate memory of `size` bytes.
    //
    // @throw std::invalid_argument
    // @throw std::bad_alloc
    void* alloc(size_t size);

    //* Allocate aligned memory of `size` bytes.
    //
    // @throw std::invalid_argument
    // @throw std::bad_alloc
    void* allocAligned(Alignment align, size_t size);

    //* Free memory allocated by this allocator.
    void free(void* ptr) noexcept;

    //* Allocate memory for `n` elements of type `T`.
    //
    // @throw std::invalid_argument
    // @throw std::bad_alloc
    template <typename T> T* allocOf(size_t n=1)
    {
        return static_cast<T*>(alloc(n * sizeof(T)));
    }

    //* Allocate aligned memory for `n` elements of type `T`.
    //
    // @throw std::invalid_argument
    // @throw std::bad_alloc
    template <typename T> T* allocAlignedOf(Alignment align, size_t n=1)
    {
        return static_cast<T*>(allocAligned(align, n * sizeof(T)));
    }

private:
    void*       m_memory;
    tlsf_pool   m_pool;
};

template <class T, class Allocator> class AllocatedBase
{
    struct Chunk
    {
        Allocator*       alloc;
        char             padding[boost::alignment_of<T>::value-sizeof(Allocator*)];
        char             data[];
    };

protected:
    static void* alloc(Allocator& allocator, size_t size)
    {
        Chunk* chunk = static_cast<Chunk*>(allocator.alloc(sizeof(Chunk) + size));
        chunk->alloc = &allocator;
        BOOST_ASSERT( Alignment::isAligned(
            boost::alignment_of<T>::value,
            reinterpret_cast<std::uintptr_t>(chunk->data)) );
        return chunk->data;
    }

    static void destroy(void* ptr)
    {
        Chunk* chunk = static_cast<Chunk*>(ptr) - 1;
        BOOST_ASSERT( chunk->data == ptr );
        chunk->alloc->free(chunk);
    }

private:
    void* operator new(size_t);
};

template <class T, class Allocator> class Allocated
    : public AllocatedBase<T, Allocator>
{
    typedef AllocatedBase<T, Allocator> super;

public:
    void* operator new(size_t size, Allocator& alloc)
    {
        return super::alloc(alloc, size);
    }
    void operator delete(void* ptr, Allocator&)
    {
        super::destroy(ptr);
    }
    void* operator new(size_t size, Allocator& alloc, size_t additional)
    {
        return super::alloc(alloc, size+additional);
    }
    void operator delete(void* ptr, Allocator&, size_t)
    {
        super::destroy(ptr);
    }
    void operator delete(void* ptr)
    {
        super::destroy(ptr);
    }
};
}; };

#endif // METHCLA_MEMORY_MANAGER_HPP_INCLUDED
