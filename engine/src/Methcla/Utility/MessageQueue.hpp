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
// #include <atomic>
#include <list>
#include <thread>

#include <boost/atomic.hpp>
// #include <boost/lockfree/queue.hpp>
#include <boost/lockfree/spsc_queue.hpp>
#include <boost/thread.hpp>
#include <boost/utility.hpp>

#include "Methcla/Utility/Semaphore.hpp"

#define METHCLA_WORKER_USE_LIST 0
#define METHCLA_WORKER_USE_PTHREAD 0

#if METHCLA_WORKER_USE_LIST
# include <libkern/OSAtomic.h>
#endif

#if METHCLA_WORKER_USE_PTHREAD
# include <pthread.h>
#endif

namespace Methcla { namespace Utility {

#if METHCLA_WORKER_USE_LIST
template <class T> class ListQueue
{
public:
    bool push(const T& a)
    {
        boost::lock_guard<boost::mutex> lock(m_mutex);
        m_list.push_back(a);
        OSMemoryBarrier();
        return true;
    }

    bool pop(T& a)
    {
        boost::lock_guard<boost::mutex> lock(m_mutex);
        if (m_list.empty())
            return false;
        a = m_list.front();
        m_list.pop_front();
        OSMemoryBarrier();
        return true;
    }

private:
    boost::mutex   m_mutex;
    std::list<T> m_list;
};
#endif // METHCLA_WORKER_USE_LIST

//* MWSR queue for sending commands to the engine.
// Request payload lifetime: from request until response callback.
// Caller is responsible for freeing request payload after the response callback has been called.
template <typename T, size_t queueSize> class MessageQueue : boost::noncopyable
{
public:
    inline void send(const T& msg)
    {
        boost::lock_guard<boost::mutex> lock(m_mutex);
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
    boost::mutex m_mutex;
};

template <class Command> class Transport : boost::noncopyable
{
public:
    Transport(size_t queueSize)
#if !METHCLA_WORKER_USE_LIST
        : m_queue(queueSize)
#endif
    { }
    virtual ~Transport()
    { }

    virtual void send(const Command& cmd) = 0;

    virtual bool dequeue(Command& cmd) = 0;

    void drain()
    {
        for (;;) {
            Command cmd;
            bool success = dequeue(cmd);
            if (success) cmd.perform();
            else break;
        }
    }

protected:
    void sendCommand(const Command& cmd)
    {
        bool success = m_queue.push(cmd);
        if (!success) throw std::runtime_error("Channel overflow");
    }

protected:
#if METHCLA_WORKER_USE_LIST
    typedef ListQueue<Command> Queue;
#else
    typedef boost::lockfree::spsc_queue<Command> Queue;
    // typedef boost::lockfree::queue<Command,boost::lockfree::capacity<queueSize>> Queue;
#endif
    Queue m_queue;
};

template <class Command> class ToWorker : public Transport<Command>
{
public:
    ToWorker(size_t queueSize, std::function<void()> signal)
        : Transport<Command>(queueSize)
        , m_signal(signal)
    { }

    virtual void send(const Command& cmd) override
    {
        this->sendCommand(cmd);
        m_signal();
    }

    bool dequeue(Command& cmd)
    {
        boost::lock_guard<boost::mutex> lock(m_mutex);
        return this->m_queue.pop(cmd);
    }

private:
    std::function<void()> m_signal;
    boost::mutex          m_mutex;
};

template <class Command> class FromWorker : public Transport<Command>
{
public:
    FromWorker(size_t queueSize)
        : Transport<Command>(queueSize)
    { }

    virtual void send(const Command& cmd) override
    {
        boost::lock_guard<boost::mutex> lock(m_mutex);
        this->sendCommand(cmd);
    }

    bool dequeue(Command& cmd) override
    {
        return this->m_queue.pop(cmd);
    }

private:
    boost::mutex m_mutex;
};

template <typename Command> class Worker : boost::noncopyable
{
public:
    Worker(size_t queueSize)
        : m_toWorker(queueSize, [this](){ this->signalWorker(); })
        , m_fromWorker(queueSize)
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
        m_fromWorker.drain();
    }

protected:
    void work()
    {
        m_toWorker.drain();
    }

    virtual void signalWorker() { }

private:
    ToWorker<Command>   m_toWorker;
    FromWorker<Command> m_fromWorker;
};

template <typename Command> class WorkerThread : public Worker<Command>
{
public:
    WorkerThread(size_t queueSize, size_t numThreads=1)
        : Worker<Command>(queueSize)
        , m_continue(true)
    {
        start(numThreads);
    }

    ~WorkerThread()
    {
        m_continue.store(false, boost::memory_order_relaxed);
        // m_continue = false;
        m_sem.post();
        join();
    }

private:
#if !METHCLA_WORKER_USE_PTHREAD
    typedef std::thread thread;
#endif

    void start(size_t numThreads)
    {
#if METHCLA_WORKER_USE_PTHREAD
        pthread_create(&m_thread, nullptr, threadFunc, this);
#else
        // for (size_t i=0; i < std::max<size_t>(1, numThreads); i++) {
        //     m_threads.emplace_back([this](){ this->process(); });
        // }
        m_thread = thread([this](){ this->process(); });
#endif
    }

    void join()
    {
#if METHCLA_WORKER_USE_PTHREAD
        pthread_join(m_thread, nullptr);
#else
        // for (auto& t : m_threads) { t.join(); }
        m_thread.join();
#endif
    }

    void process()
    {
        for (;;) {
            m_sem.wait();
            bool cont = m_continue.load(boost::memory_order_relaxed);
            // bool cont = m_continue;
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

#if METHCLA_WORKER_USE_PTHREAD
    static void* threadFunc(void* data)
    {
        ((WorkerThread<Command>*)data)->process();
        return nullptr;
    }
#endif

private:
    Semaphore           m_sem;
    // bool m_continue;
    boost::atomic<bool> m_continue;
    // std::vector<std::thread>    m_threads;
#if METHCLA_WORKER_USE_PTHREAD
    pthread_t m_thread;
#else
    thread m_thread;
#endif
};

}; };

#endif // METHCLA_UTILITY_MESSAGEQUEUE_HPP_INCLUDED
