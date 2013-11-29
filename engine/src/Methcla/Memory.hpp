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

#include <algorithm>
#include <cassert>
#include <cstddef>
#include <cstdint>

namespace Methcla { namespace Memory {

class Alignment
{
public:
    Alignment(size_t alignment)
        : m_alignment(std::max(alignment, sizeof(void*)))
    {
        assert( (m_alignment & (m_alignment - 1)) == 0 /* Alignment must be a power of two */ );
    }
    Alignment(const Alignment&) = default;

    operator size_t () const
    {
        return m_alignment;
    }

    template <typename T> bool isAligned(T size) const
    {
        return isAligned(m_alignment, size);
    }

    template <typename T> T align(T size) const
    {
        return align(m_alignment, size);
    }

    template <typename T> size_t padding(T size) const
    {
        return padding(m_alignment, size);
    }

    // Aligning pointers
    template <typename T> bool isAligned(T* ptr) const
    {
        return isAligned(reinterpret_cast<uintptr_t>(ptr));
    }

    template <typename T> T* align(T* ptr) const
    {
        return reinterpret_cast<T*>(align(reinterpret_cast<uintptr_t>(ptr)));
    }

    template <typename T> size_t padding(T* ptr) const
    {
        return padding(reinterpret_cast<uintptr_t>(ptr));
    }

    // Static alignment functions
    template <typename T> static bool isAligned(size_t alignment, T n)
    {
        return (n & ~(alignment-1)) == n;
    }

    template <typename T> static T align(size_t alignment, T n)
    {
        return (n + alignment) & ~(alignment-1);
    }

    template <typename T> static size_t padding(size_t alignment, T n)
    {
        return align(alignment, n) - n;
    }

private:
    size_t m_alignment;
};

//* Default alignment.
static const Alignment kDefaultAlignment(
#if !defined(__clang__) && defined(__GNUC__) && (__GNUC__ < 4 || (__GNUC__ == 4 && __GNUC_MINOR__ <= 8))
    // GCC <= 4.8 doesn't define std::max_align_t
    alignof(max_align_t)
#else
    alignof(std::max_align_t)
#endif
    );

//* Alignment needed for data accessed by SIMD instructions.
static const Alignment kSIMDAlignment(16);

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

} }

#endif // METHCLA_MEMORY_HPP_INCLUDED
