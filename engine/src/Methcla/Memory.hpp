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

#ifndef METHCLA_MEMORY_HPP_INCLUDED
#define METHCLA_MEMORY_HPP_INCLUDED

#include "Methcla/Exception.hpp"
#include <memory>
#include <stdexcept>

namespace Methcla { namespace Memory {

template <size_t alignment> class Alignment
{
public:
    static const size_t kAlignment = alignment;
    static const size_t kMask = ~(kAlignment - 1);

    static_assert( (kAlignment & (kAlignment - 1)) == 0, "Alignment must be a power of two" );
    static_assert( kAlignment >= sizeof(nullptr), "Alignment must be >= sizeof(nullptr)" );

    constexpr inline static size_t isAligned(size_t size)
    {
        return (size & kMask) == size;
    }
    constexpr inline static size_t align(size_t size)
    {
        return (size + alignment) & kMask;
    }
    constexpr inline static size_t padding(size_t size)
    {
        return align(size) - size;
    }
};

/// Default alignment, corresponding to the size of a pointer.
static const size_t kDefaultAlignment = sizeof(nullptr);
/// Alignment needed for data accessed by SIMD instructions.
static const size_t kSIMDAlignment = 16;

// Primitives
inline static void* alloc(size_t size)
    throw(std::invalid_argument, std::bad_alloc)
{
    if (size == 0)
        BOOST_THROW_EXCEPTION(std::invalid_argument("size must be greater than zero"));
    void* ptr = ::malloc(size);
    if (ptr == nullptr)
        BOOST_THROW_EXCEPTION(std::bad_alloc());
    return ptr;
}

inline static void free(void* ptr)
{
    if (ptr != nullptr)
        ::free(ptr);
}

template <size_t align> void* allocAligned(size_t size)
    throw(std::invalid_argument, std::bad_alloc)
{
    if (size == 0)
        BOOST_THROW_EXCEPTION(std::invalid_argument("size must be greater than zero"));
    void* ptr;
    int err = posix_memalign(&ptr, align, size);
    if (err != 0)
        BOOST_THROW_EXCEPTION(std::bad_alloc());
    return ptr;
}

// Wrappers
template <typename T> T* allocOf(size_t n=1)
    throw(std::invalid_argument, std::bad_alloc)
{
    return static_cast<T*>(alloc(n * sizeof(T)));
}

template <typename T, size_t align=alignof(T)> T* allocAlignedOf(size_t n=1)
    throw(std::invalid_argument, std::bad_alloc)
{
    return static_cast<T*>(allocAligned<align>(n * sizeof(T)));
}

}; };

#endif // METHCLA_MEMORY_HPP_INCLUDED
