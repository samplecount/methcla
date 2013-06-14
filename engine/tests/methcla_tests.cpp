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

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-private-field"
#define CATCH_CONFIG_MAIN
#include <catch.hpp>
#pragma GCC diagnostic pop

#include "Methcla/Utility/MessageQueue.hpp"
#include "Methcla/Utility/Semaphore.hpp"

#include <atomic>

TEST_CASE("Methcla/Utility/Worker", "Check for queue overflow.")
{
    struct Command
    {
        void perform() { }
    };

    const size_t queueSize = 1024;

    Methcla::Utility::Worker<Command> worker(queueSize, false);

    for (size_t i=0; i < worker.maxCapacity(); i++) {
        worker.sendToWorker(Command());
    }

    REQUIRE_THROWS(worker.sendToWorker(Command()));
}

TEST_CASE("Methcla/Utility/WorkerThread", "Check that all commands pushed to a worker thread are executed.")
{
    struct Command
    {
        void perform()
        {
            (*m_count)++;
            m_sem->post();
        }

        std::atomic<size_t>* m_count;
        Methcla::Utility::Semaphore* m_sem;
    };

    const size_t queueSize = 1024;

    for (size_t threadCount=1; threadCount <= 4; threadCount++) {
        Methcla::Utility::WorkerThread<Command> worker(queueSize, threadCount);

        std::atomic<size_t> count(0);
        Methcla::Utility::Semaphore sem;

        for (size_t i=0; i < worker.maxCapacity(); i++) {
            Command cmd;
            cmd.m_count = &count;
            cmd.m_sem = &sem;
            worker.sendToWorker(cmd);
        }

        for (size_t i=0; i < worker.maxCapacity(); i++) {
            sem.wait();
        }
        REQUIRE(count.load() == worker.maxCapacity());
    }
}
