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

#include <boost/lockfree/ringbuffer.hpp>
#include <boost/utility.hpp>

#include "Methcla/LV2/URIDMap.hpp"
#include "Methcla/Utility/RingBuffer.hpp"
#include "Methcla/Utility/Semaphore.hpp"

#include "lv2/lv2plug.in/ns/ext/atom/atom.h"
#include "lv2/lv2plug.in/ns/ext/atom/forge.h"

namespace Methcla { namespace Utility {

//* MWSR queue for sending commands to the engine.
// Request payload lifetime: from request until response callback.
// Caller is responsible for freeing request payload after the response callback has been called.
template <size_t queueSize> class MessageQueue : boost::noncopyable
{
public:
    typedef void (*Respond)(void* data, LV2_Atom* request, const LV2_Atom* response);

    class Message
    {
    public:
        Message()
            : m_respond(nullptr)
            , m_data(nullptr)
            , m_request(nullptr)
        { }
        Message(Respond respond, void* data, const LV2_Atom* request)
            : m_respond(respond)
            , m_data(data)
            , m_request(request)
        { }
        Message(const Message& other) = default;

        const LV2_Atom* payload() const
        {
            return m_request;
        }

        void respond(const LV2_Atom* response) const
        {
            // Only allowed to respond once
            m_respond(m_data, const_cast<LV2_Atom*>(m_request), response);
        }

    private:
        Respond         m_respond;
        void*           m_data;
        const LV2_Atom* m_request;
    };

    void send(Respond respond, void* data, const LV2_Atom* request)
    {
        Message msg(respond, data, request);
        std::lock_guard<std::mutex> lock(m_writeMutex);
        bool success = m_queue.enqueue(msg);
        BOOST_ASSERT( success );
    }

    bool next(Message& msg)
    {
        size_t n = m_queue.dequeue(&msg);
        return n == 1;
    }

private:
    typedef boost::lockfree::ringbuffer<Message,queueSize> Queue;
    Queue      m_queue;
    std::mutex m_writeMutex;
};

template <size_t queueSize, size_t bufferSize> class Worker : boost::noncopyable
{
public:
    class Writer;

    typedef void (*Perform)(void* data, const LV2_Atom* payload, Writer& writer);

    class alignas(sizeof(LV2_Atom)) Message
    {
    public:
        Message(const Perform& perform, void* data)
            : m_perform(perform)
            , m_data(data)
        { }

        void perform(const LV2_Atom* payload, Writer& writer)
        {
            if (m_perform != nullptr)
                m_perform(m_data, payload, writer);
        }

    private:
        Perform m_perform;
        void*   m_data;
    };

    static_assert( sizeof(Message) % sizeof(LV2_Atom) == 0
                 , "sizeof(Message) multiple of sizeof(LV2_Atom)" );

    class Writer : boost::noncopyable
    {
    public:
        typedef std::function<void()> CommitHook;

        Writer(LV2::URIDMap& uriMap, const CommitHook& afterCommit)
            : m_queue(queueSize)
            , m_afterCommit(afterCommit)
        {
            lv2_atom_forge_init(&m_forge, uriMap.lv2Map());
        }

        LV2_Atom_Forge* prepare(const Perform& perform, void* data)
        {
            uint8_t* buffer = m_writeBuffer.data();
            Message msg(perform, data);
            memcpy(buffer, &msg, sizeof(Message));
            lv2_atom_forge_set_buffer(&m_forge, buffer + sizeof(Message), m_writeBuffer.size() - sizeof(Message));
            return &m_forge;
        }

        void commit()
        {
            uint8_t* buffer = m_writeBuffer.data();
            LV2_Atom* atom = reinterpret_cast<LV2_Atom*>(buffer + sizeof(Message));
            const size_t size = sizeof(Message) + sizeof(LV2_Atom) + atom->size;
            const size_t written = m_queue.write(buffer, size);
            BOOST_ASSERT_MSG( written == size, "RingBuffer overflow" );
            m_afterCommit();
        }

    protected:
        RingBuffer                          m_queue;
        LV2_Atom_Forge                      m_forge;
        std::array<uint8_t,bufferSize>    m_writeBuffer;
        CommitHook                          m_afterCommit;
    };

    class Transport : public Writer
    {
    public:
        Transport(LV2::URIDMap& uriMap, const typename Writer::CommitHook& afterCommit)
            : Writer(uriMap, afterCommit)
        { }
        RingBuffer& queue() { return this->m_queue; }
        size_t readBufferSize() const { return m_readBuffer.size(); }
        uint8_t* readBuffer() { return m_readBuffer.data(); }

    private:
        std::array<uint8_t,bufferSize> m_readBuffer;
    };

    Worker(LV2::URIDMap& uriMap)
        : m_toWorker(uriMap, [this](){ this->signalWorker(); })
        , m_fromWorker(uriMap, typename Writer::CommitHook())
    { }

    Writer& toWorker() { return m_toWorker; }

    void perform()
    {
        Transport& transport = m_fromWorker;
        uint8_t* buffer = transport.readBuffer();
        for (;;) {
            const size_t size = transport.queue().read(buffer, transport.readBufferSize());
            if (size == 0) break;
            BOOST_ASSERT( size >= sizeof(Message) + sizeof(LV2_Atom) );
            Message* msg = reinterpret_cast<Message*>(buffer);
            LV2_Atom* payload = reinterpret_cast<LV2_Atom*>(buffer + sizeof(Message));
            msg->perform(payload, m_toWorker);
        }
    }

protected:
    void work()
    {
        Transport& transport = m_toWorker;
        uint8_t* buffer = transport.readBuffer();
        const size_t headerSize = transport.queue().read(buffer, sizeof(Message) + sizeof(LV2_Atom));
        BOOST_ASSERT( headerSize == sizeof(Message) + sizeof(LV2_Atom) );
        Message* msg = reinterpret_cast<Message*>(buffer);
        LV2_Atom* payload = reinterpret_cast<LV2_Atom*>(buffer + sizeof(Message));
        const size_t payloadSize = transport.queue().read(buffer + headerSize, payload->size);
        BOOST_ASSERT( payloadSize == payload->size );
        msg->perform(payload, m_fromWorker);
    }

    virtual void signalWorker() { }

private:
    Transport   m_toWorker;
    Transport   m_fromWorker;
};

template <size_t queueSize, size_t bufferSize> class WorkerThread : public Worker<queueSize, bufferSize>
{
public:
    WorkerThread(LV2::URIDMap& uriMap)
        : Worker<queueSize,bufferSize>(uriMap)
        , m_continue(true)
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
    Semaphore           m_sem;
    std::atomic<bool> m_continue;
};

}; };

#endif // METHCLA_UTILITY_MESSAGEQUEUE_HPP_INCLUDED
