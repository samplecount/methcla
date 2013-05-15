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

#include <algorithm>
#include <array>
#include <atomic>
#include <thread>

// #include <boost/lockfree/queue.hpp>
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
        std::lock_guard<std::mutex> lock(m_mutex);
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
    std::mutex m_mutex;
};

template <typename Command, size_t queueSize> class Worker : boost::noncopyable
{
public:
    Worker()
        : m_toWorker(*this)
    { }

    void sendToWorker(const Command& cmd)
    {
        m_toWorker.send(cmd);
    }

    void sendFromWorker(const Command& cmd)
    {
        m_fromWorker.send(cmd);
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

    friend class Transport;
    virtual void signalWorker() { }

private:
    class Transport : boost::noncopyable
    {
    public:
        virtual void send(const Command& cmd)
        {
            bool success = m_queue.push(cmd);
            if (!success) throw std::runtime_error("Channel overflow");
        }
        virtual bool dequeue(Command& cmd) = 0;

    protected:
        typedef boost::lockfree::spsc_queue<Command,boost::lockfree::capacity<queueSize>> Queue;
        // typedef boost::lockfree::queue<Command,boost::lockfree::capacity<queueSize>> Queue;
        Queue m_queue;
    };

    class ToWorker : public Transport
    {
    public:
        ToWorker(Worker& worker)
            : m_worker(worker)
        { }

        virtual void send(const Command& cmd) override
        {
            Transport::send(cmd);
            m_worker.signalWorker();
        }
        bool dequeue(Command& cmd)
        {
            std::lock_guard<std::mutex> lock(m_mutex);
            return this->m_queue.pop(cmd);
        }

    private:
        Worker&    m_worker;
        std::mutex m_mutex;
    };

    class FromWorker : public Transport
    {
    public:
        virtual void send(const Command& cmd) override
        {
            std::lock_guard<std::mutex> lock(m_mutex);
            Transport::send(cmd);
        }
        bool dequeue(Command& cmd) override
        {
            return this->m_queue.pop(cmd);
        }

    private:
        std::mutex m_mutex;
    };

    inline static void drain(Transport& input, Transport& output)
    {
        for (;;) {
            Command cmd;
            bool success = input.dequeue(cmd);
            if (success) cmd.perform();
            else break;
        }
    }

private:
    ToWorker   m_toWorker;
    FromWorker m_fromWorker;
};

template <typename Command, size_t queueSize> class WorkerThread : public Worker<Command, queueSize>
{
public:
    WorkerThread(size_t numThreads=1)
        : m_continue(true)
    {
        for (size_t i=0; i < std::max<size_t>(1, numThreads); i++) {
            m_threads.push_back(std::thread(&WorkerThread::process, this));
        }
    }
    ~WorkerThread()
    {
        m_continue.store(false, std::memory_order_acquire);
        m_sem.post();
        for_each(m_threads.begin(), m_threads.end(), std::mem_fun_ref(&std::thread::join));
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
    std::vector<std::thread>    m_threads;
    Semaphore                   m_sem;
    std::atomic<bool>           m_continue;
};

}; };

#endif // METHCLA_UTILITY_MESSAGEQUEUE_HPP_INCLUDED
