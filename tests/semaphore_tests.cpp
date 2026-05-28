// Copyright (C) 2026 Methcla contributors
//
// SPDX-License-Identifier: Apache-2.0

#include "Methcla/Utility/Semaphore.hpp"

#include <atomic>
#include <thread>

#include "gtest/gtest.h"

// Initialising with N allows exactly N waits before blocking.
TEST(Methcla_Utility_Semaphore, Constructor)
{
    for (size_t n : {1, 2, 3, 10, 20, 50, 100, 1000, 1024, 10000})
    {
        Methcla::Utility::Semaphore sem(n);
        size_t                      count(0);

        for (size_t i = 0; i < n; i++)
        {
            sem.wait();
            count++;
        }

        EXPECT_EQ(count, n);
    }
}

// Producer thread posts N times; main thread waits N times and verifies all
// increments completed, confirming cross-thread ordering.
TEST(Methcla_Utility_Semaphore, Post_wait)
{
    for (size_t n : {1, 2, 3, 10, 20, 50, 100, 1000, 1024, 10000})
    {
        Methcla::Utility::Semaphore sem;
        std::atomic<size_t>         count(0);

        std::thread thread([&]() {
            for (size_t i = 0; i < n; i++)
            {
                count++;
                sem.post();
            }
        });
        for (size_t i = 0; i < n; i++)
        {
            sem.wait();
        }
        EXPECT_EQ(count.load(), n);
        thread.join();
    }
}

TEST(Methcla_Utility_Semaphore, TryWait_empty_returns_false)
{
    Methcla::Utility::Semaphore sem;
    EXPECT_FALSE(sem.tryWait());
}

TEST(Methcla_Utility_Semaphore, TryWait_after_post_returns_true)
{
    Methcla::Utility::Semaphore sem;
    sem.post();
    EXPECT_TRUE(sem.tryWait());
}

TEST(Methcla_Utility_Semaphore, TryWait_decrements_count)
{
    Methcla::Utility::Semaphore sem(2);
    EXPECT_TRUE(sem.tryWait());
    EXPECT_TRUE(sem.tryWait());
    EXPECT_FALSE(sem.tryWait());
}

TEST(Methcla_Utility_Semaphore, TryWait_after_cross_thread_post)
{
    Methcla::Utility::Semaphore sem;

    std::thread t([&]() { sem.post(); });
    t.join(); // join guarantees post() has completed before tryWait()

    EXPECT_TRUE(sem.tryWait());
}
