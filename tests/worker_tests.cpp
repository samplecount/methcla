// Copyright (C) 2012-2013 Samplecount S.L.
// Copyright (C) 2026 Methcla contributors
//
// SPDX-License-Identifier: Apache-2.0

#include "Methcla/Utility/MessageQueue.hpp"
#include "Methcla/Utility/Semaphore.hpp"

#include <atomic>
#include <mutex>

#include "gtest/gtest.h"

namespace {

    class Log
    {
    public:
        Log()
        : m_lock(s_mutex)
        {}

        template <typename T> Log& operator<<(const T& /* x */)
        {
            return *this;
        }

    private:
        std::lock_guard<std::mutex> m_lock;
        static std::mutex           s_mutex;
    };

    std::mutex Log::s_mutex;

} // namespace

namespace test_worker {
    struct Command
    {
        void perform()
        {}
    };
} // namespace test_worker

TEST(Methcla_Utility_Worker, Queue_overflow_should_throw)
{
    const size_t queueSize = 1024;

    Methcla::Utility::Worker<test_worker::Command> worker(queueSize, false);

    for (size_t i = 0; i < worker.maxCapacity(); i++)
    {
        worker.sendToWorker(test_worker::Command());
    }

    ASSERT_ANY_THROW(worker.sendToWorker(test_worker::Command()));
}

namespace test_worker_thread {
    struct Command
    {
        void perform()
        {
            (*m_count)++;
            m_sem->post();
            Log() << "POST " << m_id;
        }

        size_t                       m_id;
        std::atomic<size_t>*         m_count;
        Methcla::Utility::Semaphore* m_sem;
    };
} // namespace test_worker_thread

// Each command increments a counter and posts to a semaphore. The main thread
// waits once per command and checks the final count. Repeated for 1–4 threads.
TEST(Methcla_Utility_WorkerThread, All_commands_should_be_executed)
{
    using test_worker_thread::Command;

    const size_t queueSize = 16;

    for (size_t threadCount = 1; threadCount <= 4; threadCount++)
    {
        Methcla::Utility::WorkerThread<Command> worker(queueSize, threadCount);

        std::atomic<size_t>         count(0);
        Methcla::Utility::Semaphore sem;

        for (size_t i = 0; i < worker.maxCapacity(); i++)
        {
            Command cmd;
            cmd.m_id = i;
            cmd.m_count = &count;
            cmd.m_sem = &sem;
            worker.sendToWorker(cmd);
        }

        for (size_t i = 0; i < worker.maxCapacity(); i++)
        {
            sem.wait();
            Log() << "WAIT " << i << " " << count.load();
        }

        EXPECT_EQ(count.load(), worker.maxCapacity());
    }
}
