// Copyright 2012-2013 Samplecount S.L.
//
// SPDX-License-Identifier: Apache-2.0

#include "Methcla/Memory.hpp"

#include <cassert>
#include <cstdlib>
#include <memory>
#include <stdexcept>

#if defined(__ANDROID__) || defined(__native_client__)
#    include <malloc.h>
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
#else
    int err = posix_memalign(&ptr, align, size);
    if (err != 0)
        ptr = nullptr;
#endif

    if (ptr == nullptr)
        throw std::bad_alloc();

    assert(align.isAligned(ptr));

    return ptr;
}

void Methcla::Memory::freeAligned(void* ptr) noexcept
{
    Methcla::Memory::free(ptr);
}
