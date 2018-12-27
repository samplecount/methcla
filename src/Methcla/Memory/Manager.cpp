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

#include "Methcla/Memory/Manager.hpp"

#include <new>       // std::bad_alloc
#include <stdexcept> // std::invalid_argument

// Set to 1 to disable the realtime memory manager.
#define METHCLA_NO_RT_MEMORY 0

using namespace Methcla::Memory;

#if METHCLA_NO_RT_MEMORY
RTMemoryManager::RTMemoryManager(size_t)
: m_memory(nullptr)
, m_pool(nullptr)
{}
#else
RTMemoryManager::RTMemoryManager(size_t poolSize)
: m_memory(nullptr)
, m_pool(nullptr)
{
    const size_t allocSize = tlsf_overhead() + poolSize;
    m_memory = Memory::alloc(allocSize);
    m_pool = tlsf_create(m_memory, allocSize);
    if (m_pool == nullptr)
    {
        Memory::free(m_memory);
        throw std::bad_alloc();
    }
}
#endif

RTMemoryManager::~RTMemoryManager()
{
#if !METHCLA_NO_RT_MEMORY
    tlsf_destroy(m_pool);
    Memory::free(m_memory);
#endif
}

void* RTMemoryManager::alloc(size_t size)
{
#if METHCLA_NO_RT_MEMORY
    return Methcla::Memory::alloc(size);
#else
    if (size == 0)
        throw std::invalid_argument(
            "allocation size must be greater than zero");
    void* ptr = tlsf_malloc(m_pool, size);
    if (ptr == nullptr)
        throw std::bad_alloc();
    return ptr;
#endif
}

void RTMemoryManager::free(void* ptr) noexcept
{
#if METHCLA_NO_RT_MEMORY
    Methcla::Memory::free(ptr);
#else
    if (ptr != nullptr)
        tlsf_free(m_pool, ptr);
#endif
}

void* RTMemoryManager::allocAligned(Alignment align, size_t size)
{
#if METHCLA_NO_RT_MEMORY
    return Methcla::Memory::allocAligned(align, size);
#else
    if (size == 0)
        throw std::invalid_argument(
            "allocation size must be greater than zero");
    void* ptr = tlsf_memalign(m_pool, align, size);
    if (ptr == nullptr)
        throw std::bad_alloc();
    return ptr;
#endif
}

void RTMemoryManager::freeAligned(void* ptr) noexcept
{
#if METHCLA_NO_RT_MEMORY
    Methcla::Memory::freeAligned(ptr);
#else
    if (ptr != nullptr)
        tlsf_free(m_pool, ptr);
#endif
}

static void collectStatistics(void* /* ptr */, size_t size, int used,
                              void* user)
{
    auto stats = static_cast<RTMemoryManager::Statistics*>(user);
    if (used)
        stats->usedNumBytes += size;
    else
        stats->freeNumBytes += size;
}

RTMemoryManager::Statistics RTMemoryManager::statistics() const
{
    Statistics stats;
    stats.freeNumBytes = 0;
    stats.usedNumBytes = 0;
#if !METHCLA_NO_RT_MEMORY
    tlsf_walk_heap(m_pool, collectStatistics, &stats);
#endif
    return stats;
}
