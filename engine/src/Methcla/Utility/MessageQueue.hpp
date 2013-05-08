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
#include <functional>
#include <thread>

#include <boost/lockfree/spsc_queue.hpp>
#include <boost/utility.hpp>

#include "Methcla/Utility/Semaphore.hpp"

namespace Methcla { namespace Utility {

//* MWSR queue for sending commands to the engine.
// Request payload lifetime: from request until response callback.
// Caller is responsible for freeing request payload after the response callback has been called.
template <typename T, size_t queueSize> class MessageQueue : boost::noncopyable
{
public:
    inline void send(const T& msg)
    {
        std::lock_guard<std::mutex> lock(m_writeMutex);
        bool success = m_queue.push(msg);
        if (!success) throw std::runtime_error("Message queue overflow");
    }

    inline bool next(T& msg)
    {
        return m_queue.pop(msg);
    }

private:
    typedef boost::lockfree::spsc_queue<T,boost::lockfree::capacity<queueSize>> Queue;
    Queue      m_queue;
    std::mutex m_writeMutex;
};

template <typename Command, size_t queueSize> class Channel : boost::noncopyable
{
public:
    typedef std::function<void()> CommitHook;

    Channel(const CommitHook& afterCommit)
        // : m_queue(queueSize)
        : m_afterCommit(afterCommit)
    { }

    void send(const Command& cmd)
    {
        bool success = m_queue.push(cmd);
        if (!success) throw std::runtime_error("Channel overflow");
        if (m_afterCommit) m_afterCommit();
    }

protected:
    typedef boost::lockfree::spsc_queue<Command,boost::lockfree::capacity<queueSize>> Queue;
    Queue      m_queue;
    CommitHook m_afterCommit;
};

template <typename Command, size_t queueSize> class Worker : boost::noncopyable
{
public:
    Worker()
        : m_toWorker([this](){ this->signalWorker(); })
        , m_fromWorker(typename Channel<Command,queueSize>::CommitHook())
    { }

    Channel<Command,queueSize>& toWorker()
    {
        return m_toWorker;
    }

    void perform()
    {
        drain(m_fromWorker, m_toWorker);
    }

protected:
    void work()
    {
        drain(m_toWorker, m_fromWorker);
    }

    virtual void signalWorker() { }

private:
    class Transport : public Channel<Command,queueSize>
    {
    public:
        Transport(const typename Channel<Command,queueSize>::CommitHook& afterCommit)
            : Channel<Command,queueSize>(afterCommit)
        { }

        bool dequeue(Command& cmd)
        {
            return this->m_queue.pop(cmd);
        }
    };

    inline static void drain(Transport& input, Transport& output)
    {
        for (;;) {
            Command cmd;
            bool success = input.dequeue(cmd);
            if (success) cmd.perform(output);
            else break;
        }
    }

private:
    Transport   m_toWorker;
    Transport   m_fromWorker;
};

template <typename Command, size_t queueSize> class WorkerThread : public Worker<Command, queueSize>
{
public:
    WorkerThread()
        : m_continue(true)
    {
        m_thread = std::thread(&WorkerThread::process, this);
    }
    ~WorkerThread()
    {
        m_continue.store(false, std::memory_order_acquire);
        m_sem.post();
        m_thread.join();
    }

private:
    void process()
    {
        for (;;) {
            m_sem.wait();
            bool cont = m_continue.load(std::memory_order_release);
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
    std::thread       m_thread;
    Semaphore         m_sem;
    std::atomic<bool> m_continue;
};

}; };

#endif // METHCLA_UTILITY_MESSAGEQUEUE_HPP_INCLUDED
