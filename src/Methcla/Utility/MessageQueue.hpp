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

#ifndef METHCLA_UTILITY_MESSAGEQUEUE_HPP_INCLUDED
#define METHCLA_UTILITY_MESSAGEQUEUE_HPP_INCLUDED

#include <array>
#include <atomic>
#include <mutex>
#include <thread>
#include <vector>

#include "Methcla/Utility/Macros.h"
#include "Methcla/Utility/MessageQueueInterface.hpp"
#include "Methcla/Utility/Semaphore.hpp"
#include "Methcla/Utility/WorkerInterface.hpp"

METHCLA_WITHOUT_WARNINGS_BEGIN
# include <boost/lockfree/spsc_queue.hpp>
METHCLA_WITHOUT_WARNINGS_END

namespace Methcla { namespace Utility {

//* MWSR queue for sending commands to the engine.
// Request payload lifetime: from request until response callback.
// Caller is responsible for freeing request payload after the response callback has been called.
template <typename T> class MessageQueue : public MessageQueueInterface<T>
{
public:
    MessageQueue(size_t queueSize)
        : m_queue(queueSize)
    { }

    MessageQueue(const MessageQueue<T>& other) = delete;
    MessageQueue<T>& operator=(const MessageQueue<T>& other) = delete;

    void send(const T& msg) override
    {
        std::lock_guard<std::mutex> lock(m_mutex);
        bool success = m_queue.push(msg);
        if (!success) throw std::runtime_error("Message queue overflow");
    }

    bool next(T& msg) override
    {
        return m_queue.pop(msg);
    }

private:
    typedef boost::lockfree::spsc_queue<T> Queue;
    Queue      m_queue;
    std::mutex m_mutex;
};

template <class Command> class Transport
{
public:
    Transport(size_t queueSize, bool needsLock)
        : m_queue(queueSize)
        , m_needsLock(needsLock)
    { }
    virtual ~Transport()
    { }

    Transport(const Transport&) = delete;
    Transport& operator=(const Transport&) = delete;

    virtual void send(const Command& cmd) = 0;

    virtual bool dequeue(Command& cmd) = 0;

    void performOne()
    {
        Command cmd;
        if (dequeue(cmd)) {
            cmd.perform();
        }
    }

    void performAll()
    {
        Command cmd;
        while (dequeue(cmd)) {
            cmd.perform();
        }
    }

protected:
    typedef boost::lockfree::spsc_queue<Command> Queue;

    Queue& queue()
    {
        return m_queue;
    }

    bool needsLock() const
    {
        return m_needsLock;
    }

    std::mutex& mutex()
    {
        return m_mutex;
    }

    void sendCommand(const Command& cmd)
    {
        bool success = m_queue.push(cmd);
        if (!success) throw std::runtime_error("Channel overflow");
    }

private:
    // typedef boost::lockfree::queue<Command,boost::lockfree::capacity<queueSize>> Queue;
    Queue       m_queue;
    bool        m_needsLock;
    std::mutex  m_mutex;
};

template <class Command> class ToWorker : public Transport<Command>
{
public:
    ToWorker(size_t queueSize, bool needsLock, std::function<void()> signal)
        : Transport<Command>(queueSize, needsLock)
        , m_signal(signal)
    { }

    virtual void send(const Command& cmd) override
    {
        this->sendCommand(cmd);
        if (m_signal) m_signal();
    }

    bool dequeue(Command& cmd) override
    {
        if (this->needsLock()) {
            std::lock_guard<std::mutex> lock(this->mutex());
            return this->queue().pop(cmd);
        } else {
            return this->queue().pop(cmd);
        }
    }

private:
    std::function<void()> m_signal;
};

template <class Command> class FromWorker : public Transport<Command>
{
public:
    FromWorker(size_t queueSize, bool needsLock)
        : Transport<Command>(queueSize, needsLock)
    { }

    virtual void send(const Command& cmd) override
    {
        if (this->needsLock()) {
            std::lock_guard<std::mutex> lock(this->mutex());
            this->sendCommand(cmd);
        } else {
            this->sendCommand(cmd);
        }
    }

    bool dequeue(Command& cmd) override
    {
        return this->queue().pop(cmd);
    }
};

template <typename Command> class Worker : public WorkerInterface<Command>
{
public:
    Worker(size_t queueSize, bool needsLock)
        : m_queueSize(queueSize)
        , m_toWorker(queueSize, needsLock, [this](){ this->signalWorker(); })
        , m_fromWorker(queueSize, needsLock)
    { }

    Worker(const Worker&) = delete;
    Worker& operator=(const Worker&) = delete;

    size_t maxCapacity() const
    {
        return m_queueSize;
    }

    void sendToWorker(const Command& cmd) override
    {
        m_toWorker.send(cmd);
    }

    void sendFromWorker(const Command& cmd) override
    {
        m_fromWorker.send(cmd);
    }

    void perform() override
    {
        m_fromWorker.performAll();
    }

protected:
    void work()
    {
        m_toWorker.performOne();
    }

    virtual void signalWorker() { }

private:
    size_t              m_queueSize;
    ToWorker<Command>   m_toWorker;
    FromWorker<Command> m_fromWorker;
};

template <typename Command> class WorkerThread : public Worker<Command>
{
public:
    WorkerThread(size_t queueSize, size_t numThreads=1)
        : Worker<Command>(queueSize, numThreads > 1)
        , m_continue(true)
    {
        for (size_t i=0; i < std::max((size_t)1, numThreads); i++) {
            m_threads.emplace_back([this](){ this->process(); });
        }
    }

    ~WorkerThread()
    {
        stop();
    }

    void stop() override
    {
        if (m_continue.load(std::memory_order_relaxed))
        {
            m_continue.store(false, std::memory_order_relaxed);
            // Signal *all* threads
            for (size_t i=0; i < m_threads.size(); i++) {
                m_sem.post();
            }
            // Wait for threads to exit
            for (auto& t : m_threads) { t.join(); }
            // Clear array of threads
            m_threads.clear();
        }
    }

private:
    void process()
    {
        for (;;) {
            m_sem.wait();
            bool cont = m_continue.load(std::memory_order_relaxed);
            if (cont) {
                this->work();
            } else {
                break;
            }
        }
    }

    virtual void signalWorker() override
    {
        m_sem.post();
    }

private:
    Semaphore                   m_sem;
    std::atomic<bool>           m_continue;
    std::vector<std::thread>    m_threads;
};

} }

#endif // METHCLA_UTILITY_MESSAGEQUEUE_HPP_INCLUDED
