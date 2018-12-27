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

#include "Methcla/Memory.hpp"

#include <boost/type_traits/aligned_storage.hpp>
#include <boost/type_traits/alignment_of.hpp>

#include <cassert>
#include <cstddef>

#include <tlsf.h>

namespace Methcla { namespace Memory {

    class Allocator
    {
    public:
        //* Allocate memory of `size` bytes.
        //
        // @throw std::invalid_argument
        // @throw std::bad_alloc
        virtual void* alloc(size_t size) = 0;

        //* Free memory allocated by `alloc`.
        virtual void free(void* ptr) noexcept = 0;

        //* Allocate aligned memory of `size` bytes.
        //
        // @throw std::invalid_argument
        // @throw std::bad_alloc
        virtual void* allocAligned(Alignment align, size_t size) = 0;

        //* Free memory allocated by `allocAligned`.
        virtual void freeAligned(void* ptr) noexcept = 0;

        //* Allocate memory for `n` elements of type `T`.
        //
        // @throw std::invalid_argument
        // @throw std::bad_alloc
        template <typename T> T* allocOf(size_t n = 1)
        {
            return static_cast<T*>(alloc(n * sizeof(T)));
        }

        //* Allocate aligned memory for `n` elements of type `T`.
        //
        // @throw std::invalid_argument
        // @throw std::bad_alloc
        template <typename T> T* allocAlignedOf(Alignment align, size_t n = 1)
        {
            return static_cast<T*>(allocAligned(align, n * sizeof(T)));
        }

        //* Construct an object in a chunk of memory returned by this allocator.
        template <class T, class... Args> T* construct(Args&&... args)
        {
            void* mem = alloc(sizeof(T));
            try
            {
                return new (mem) T(std::forward<Args>(args)...);
            }
            catch (...)
            {
                free(mem);
                throw;
            }
        }
    };

    class RTMemoryManager : public Allocator
    {
    public:
        //* Construct a realtime memory allocator with a capacity of `size` kB.
        RTMemoryManager(size_t size);
        ~RTMemoryManager();

        void* alloc(size_t size) override;
        void  free(void* ptr) noexcept override;
        void* allocAligned(Alignment align, size_t size) override;
        void  freeAligned(void* ptr) noexcept override;

        struct Statistics
        {
            size_t freeNumBytes = 0;
            size_t usedNumBytes = 0;
        };

        Statistics statistics() const;

    private:
        void*  m_memory;
        tlsf_t m_pool;
    };

    template <class T, class Allocator> class AllocatedBase
    {
        struct Chunk
        {
            typedef boost::aligned_storage<1, boost::alignment_of<T>::value>
                Padding;

            Allocator* alloc;
            Padding    padding;
        };

    protected:
        static void* alloc(Allocator& allocator, size_t size)
        {
            Chunk* chunk =
                static_cast<Chunk*>(allocator.alloc(sizeof(Chunk) + size));
            chunk->alloc = &allocator;
            void* ptr = chunk + 1;
            assert(Alignment::isAligned(boost::alignment_of<T>::value,
                                        reinterpret_cast<std::uintptr_t>(ptr)));
            return ptr;
        }

        static void destroy(void* ptr)
        {
            Chunk* chunk = static_cast<Chunk*>(ptr) - 1;
            chunk->alloc->free(chunk);
        }

    private:
        void* operator new(size_t);
    };

    template <class T, class Allocator>
    class Allocated : public AllocatedBase<T, Allocator>
    {
        typedef AllocatedBase<T, Allocator> super;

    public:
        void* operator new(size_t size, Allocator& alloc)
        {
            return super::alloc(alloc, size);
        }
        void  operator delete(void* ptr, Allocator&) { super::destroy(ptr); }
        void* operator new(size_t size, Allocator& alloc, size_t additional)
        {
            return super::alloc(alloc, size + additional);
        }
        void operator delete(void* ptr, Allocator&, size_t)
        {
            super::destroy(ptr);
        }
        void operator delete(void* ptr) { super::destroy(ptr); }
    };
}} // namespace Methcla::Memory

#endif // METHCLA_MEMORY_MANAGER_HPP_INCLUDED
