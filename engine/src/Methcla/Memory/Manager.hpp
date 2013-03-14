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

#include <boost/cstdint.hpp>
#include <tlsf.h>
#include <stdexcept>

//#include <boost/lockfree/fifo.hpp>

namespace Methcla { namespace Memory {

class RTMemoryManager
{
public:
    RTMemoryManager(size_t poolSize)
        : m_memory(nullptr)
        , m_pool(nullptr)
    {
        m_memory = Memory::alloc(poolSize);
        m_pool = tlsf_create(m_memory, poolSize);
        if (m_pool == nullptr) {
            Memory::free(m_memory);
            BOOST_THROW_EXCEPTION(std::bad_alloc());
        }
    }
    ~RTMemoryManager()
    {
        tlsf_destroy(m_pool);
        Memory::free(m_memory);
    }

    //* Allocate memory of `size` bytes.
    //
    // @throw std::invalid_argument
    // @throw std::bad_alloc
    void* alloc(size_t size)
    {
        if (size == 0)
            BOOST_THROW_EXCEPTION(std::invalid_argument("allocation size must be greater than zero"));
        void* ptr = tlsf_malloc(m_pool, size);
        if (ptr == nullptr)
            BOOST_THROW_EXCEPTION(std::bad_alloc());
        return ptr;
    }

    //* Allocate aligned memory of `size` bytes.
    //
    // @throw std::invalid_argument
    // @throw std::bad_alloc
    template <size_t align> void* allocAligned(size_t size)
    {
        if (size == 0)
            BOOST_THROW_EXCEPTION(std::invalid_argument("allocation size must be greater than zero"));
        void* ptr = tlsf_memalign(m_pool, align, size);
        if (ptr == nullptr)
            BOOST_THROW_EXCEPTION(std::bad_alloc());
        return ptr;
    }

    //* Free memory allocated by this allocator.
    void free(void* ptr) noexcept
    {
        if (ptr != nullptr)
            tlsf_free(m_pool, ptr);
    }

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
    template <typename T, size_t align> T* allocAlignedOf(size_t n=1)
    {
        return static_cast<T*>(allocAligned<align>(n * sizeof(T)));
    }

private:
    void*       m_memory;
    tlsf_pool   m_pool;
};

template <class T, class Allocator, size_t align=kDefaultAlignment> class AllocatedBase
{
    struct Chunk
    {
        Allocator*  alloc;
        char        padding[Alignment<align>::padding(sizeof(alloc))];
        char        data[];
    };

protected:
    static const size_t kAlignment = align;

    static void* alloc(Allocator& allocator, size_t size)
    {
        Chunk* chunk = static_cast<Chunk*>(allocator.template allocAligned<align>(sizeof(Chunk) + size));
        chunk->alloc = &allocator;
        BOOST_ASSERT( Alignment<align>::isAligned(reinterpret_cast<size_t>(chunk->data)) );
        return chunk->data;
    }
    static void destroy(void* ptr)
    {
        Chunk* chunk = static_cast<Chunk*>(ptr) - 1;
        BOOST_ASSERT( chunk->data == ptr );
        static_cast<T*>(ptr)->~T();
        chunk->alloc->free(chunk);
    }

    void* operator new(size_t);
};

template <class T, class Allocator, size_t align=kDefaultAlignment> class Allocated
    : public AllocatedBase<T, Allocator, align>
{
    typedef AllocatedBase<T, Allocator, align> super;

public:
    void* operator new(size_t size, Allocator& alloc)
    {
        return super::alloc(alloc, size);
    }
    void operator delete(void* ptr, Allocator& alloc)
    {
        super::destroy(ptr);
    }
    void* operator new(size_t size, Allocator& alloc, size_t additional)
    {
        return super::alloc(alloc, size+additional);
    }
    void operator delete(void* ptr, Allocator& alloc, size_t realSize)
    {
        super::destroy(ptr);
    }
    void operator delete(void* ptr)
    {
        super::destroy(ptr);
    }
};

// template <class Manager> class DeferredMemoryManager
// {
// public:
//     typedef void (*Destructor)(void* ptr);
//
//     DeferredMemoryManager(Manager& mem)
//         : m_mem(mem)
//     { }
//
//     void* malloc(size_t size)
//     {
//         Chunk* chunk;
//         while (m_fifo.dequeue(chunk)) {
//             if (chunk->destroy != 0) {
//                 chunk->destroy(chunk->data);
//             }
//             m_mem.free(chunk);
//         }
//         cout << "DeferredMemoryManager::malloc" << endl;
//         chunk = static_cast<Chunk*>(m_mem.malloc(sizeof(Chunk) + size));
//         chunk->destroy = 0;
//         return chunk->data;
//     }
//     void free(Destructor destroy, void* ptr)
//     {
//         Chunk* chunk = static_cast<Chunk*>(ptr) - 1;
//         chunk->destroy = destroy;
//         do {
//             /* SPIN */
//         } while (!m_fifo.enqueue(chunk));
//     }
//
// private:
//     struct Chunk
//     {
//         Destructor  destroy;
//         char        data[];
//     };
//
//     Manager& m_mem;
//     boost::lockfree::fifo<Chunk*> m_fifo;
// };
//
// template <class T, class Manager> class AllocatedDeferredBase
// {
// public:
//     typedef DeferredMemoryManager<Manager> Allocator;
//
//     void* operator new(size_t size, Allocator& alloc)
//     {
//         Chunk* chunk = static_cast<Chunk*>(alloc.malloc(sizeof(Chunk) + size));
//         chunk->alloc = &alloc;
//         return chunk->data;
//     }
//     void operator delete(void* ptr, Allocator& alloc)
//     {
//         AllocatedDeferredBase<T, Manager>::free(ptr);
//     }
//
//     void free()
//     {
//         AllocatedDeferredBase<T, Manager>::free(this);
//     }
//
// protected:
//     void operator delete(void*)
//     {
//         BOOST_ASSERT( false );
//     }
//
// private:
//     struct Chunk
//     {
//         Allocator* alloc;
//         char       data[];
//     };
//
//     static void destroy(void* ptr)
//     {
//         cout << "AllocatedDeferredBase::destroy" << endl;
//         Chunk* chunk = static_cast<Chunk*>(ptr);
//         reinterpret_cast<T*>(chunk->data)->~T();
//     }
//     static void free(void* ptr)
//     {
//         cout << "AllocatedDeferredBase::free" << endl;
//         Chunk* chunk = static_cast<Chunk*>(ptr) - 1;
//         BOOST_ASSERT( chunk->data == ptr );
//         chunk->alloc->free(&destroy, chunk);
//     }
//
//     void* operator new(size_t);
// };

}; };

#endif // METHCLA_MEMORY_MANAGER_HPP_INCLUDED
