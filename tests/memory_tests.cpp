// Copyright (C) 2012-2013 Samplecount S.L.
// Copyright (C) 2026 Methcla contributors
//
// SPDX-License-Identifier: Apache-2.0

#include "Methcla/Memory/Manager.hpp"

#include <list>

#include "gtest/gtest.h"

// Allocate until the pool is exhausted, then free everything. Verifies that
// used/free byte accounting is consistent and all memory is recovered.
TEST(Methcla_Memory_Manager, Alloc_free_should_be_noop)
{
    const size_t     memSize = 8192;
    const size_t     allocSize = 33;
    auto             mem = new Methcla::Memory::RTMemoryManager(memSize);
    std::list<void*> ptrs;
    while (true)
    {
        try
        {
            void* ptr = mem->alloc(allocSize);
            ASSERT_TRUE(ptr != nullptr);
            ptrs.push_back(ptr);
        }
        catch (std::bad_alloc&)
        {
            break;
        }
    }
    {
        Methcla::Memory::RTMemoryManager::Statistics stats(mem->statistics());
        EXPECT_GT(stats.usedNumBytes, ptrs.size() * allocSize);
        EXPECT_LT(stats.freeNumBytes, memSize - stats.usedNumBytes);
    }
    for (auto ptr : ptrs)
    {
        mem->free(ptr);
    }
    {
        Methcla::Memory::RTMemoryManager::Statistics stats(mem->statistics());
        EXPECT_EQ(stats.freeNumBytes, memSize);
        EXPECT_EQ(stats.usedNumBytes, 0u);
    }
}
