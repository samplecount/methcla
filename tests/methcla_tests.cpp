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
#include "Methcla/Utility/MessageQueue.hpp"
#include "Methcla/Utility/Semaphore.hpp"

#include "methcla_tests.hpp"

#include <atomic>
#include <iostream>
#include <list>
#include <mutex>
#include <thread>

static std::string gInputFileDirectory = "tests/input";
static std::string gOutputFileDirectory = "tests/output";

void Methcla::Tests::initialize(std::string inputFileDirectory,
                                std::string outputFileDirectory)
{
    gInputFileDirectory = inputFileDirectory;
    gOutputFileDirectory = outputFileDirectory;
}

std::string Methcla::Tests::inputFile(const std::string& name)
{
    return gInputFileDirectory + "/" + name;
}

std::string Methcla::Tests::outputFile(const std::string& name)
{
    return gOutputFileDirectory + "/" + name;
}

namespace test_Methcla_Utility_Worker {
    struct Command
    {
        void perform() {}
    };
}; // namespace test_Methcla_Utility_Worker

namespace Methcla { namespace Test {

    // Synchronize logging
    class Log
    {
    public:
        Log()
        : m_lock(s_mutex)
        {}

        template <typename T> Log& operator<<(const T& /* x */)
        {
            // std::cout << x << std::endl;
            return *this;
        }

    private:
        std::lock_guard<std::mutex> m_lock;
        static std::mutex           s_mutex;
    };

    std::mutex Log::s_mutex;
}} // namespace Methcla::Test

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

TEST(Methcla_Utility_Worker, Queue_overflow_should_throw)
{
    using test_Methcla_Utility_Worker::Command;

    const size_t queueSize = 1024;

    Methcla::Utility::Worker<Command> worker(queueSize, false);

    for (size_t i = 0; i < worker.maxCapacity(); i++)
    {
        worker.sendToWorker(Command());
    }

    ASSERT_ANY_THROW(worker.sendToWorker(Command()));
}

namespace test_Methcla_Utility_WorkerThread {
    struct Command
    {
        void perform()
        {
            (*m_count)++;
            m_sem->post();
            Methcla::Test::Log() << "POST " << m_id;
        }

        size_t                       m_id;
        std::atomic<size_t>*         m_count;
        Methcla::Utility::Semaphore* m_sem;
    };
}; // namespace test_Methcla_Utility_WorkerThread

TEST(Methcla_Utility_WorkerThread, All_commands_should_be_executed)
{
    using test_Methcla_Utility_WorkerThread::Command;

    const size_t queueSize = 16;

    for (size_t threadCount = 1; threadCount <= 4; threadCount++)
    {
        Methcla::Test::Log() << "threads " << threadCount;

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
            Methcla::Test::Log() << "WAIT " << i << " " << count.load();
        }

        EXPECT_EQ(count.load(), worker.maxCapacity());
    }
}

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
        catch (std::bad_alloc)
        {
            break;
        }
    }
    {
        Methcla::Memory::RTMemoryManager::Statistics stats(mem->statistics());
        // usedNumBytes is greater than ptrs.size() * allocSize due to memory
        // allocation overhead.
        EXPECT_GT(stats.usedNumBytes, ptrs.size() * allocSize);
        // Note that freeNumBytes is less than memSize - usedNumBytes due to
        // memory allocation overhead.
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
