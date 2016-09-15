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

#include "Methcla/Memory.hpp"

#include <cassert>
#include <cstdlib>
#include <memory>
#include <stdexcept>

#if defined(__ANDROID__) || defined(__native_client__) || defined(__MINGW32__)
# include <malloc.h>
#endif

void* Methcla::Memory::alloc(size_t size)
{
    if (size == 0)
        throw std::invalid_argument("size must be greater than zero");

    void* ptr = std::malloc(size);

    if (ptr == nullptr)
        throw std::bad_alloc();

    return ptr;
}

void Methcla::Memory::free(void* ptr) noexcept
{
    std::free(ptr);
}

void* Methcla::Memory::allocAligned(Alignment align, size_t size)
{
    if (size == 0)
        throw std::invalid_argument("size must be greater than zero");

    void* ptr = nullptr;

#if defined(__ANDROID__) || defined(__native_client__)
    ptr = memalign(align, size);
#elif defined(__MINGW32__)
    ptr = _aligned_malloc(size, align);
#else
    int err = posix_memalign(&ptr, align, size);
    if (err != 0) ptr = nullptr;
#endif

    if (ptr == nullptr)
        throw std::bad_alloc();

    assert(align.isAligned(ptr));

    return ptr;
}

void Methcla::Memory::freeAligned(void* ptr) noexcept
{
#if defined(__MINGW32__)
    _aligned_free(ptr);
#else
    Methcla::Memory::free(ptr);
#endif
}
