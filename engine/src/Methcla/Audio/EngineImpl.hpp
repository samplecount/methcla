// Copyright 2012-2014 Samplecount S.L.
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

#ifndef METHCLA_AUDIO_ENGINE_IMPL_HPP_INCLUDED
#define METHCLA_AUDIO_ENGINE_IMPL_HPP_INCLUDED

#include "Methcla/Audio/AudioBus.hpp"
#include "Methcla/Audio/Group.hpp"
#include "Methcla/Audio/Synth.hpp"
#include "Methcla/Memory.hpp"
#include "Methcla/Memory/Manager.hpp"
#include "Methcla/Platform.hpp"
#include "Methcla/Utility/Macros.h"
#include "Methcla/Utility/MessageQueue.hpp"

#include <methcla/log.hpp>

METHCLA_WITHOUT_WARNINGS_BEGIN
# include <boost/heap/priority_queue.hpp>
METHCLA_WITHOUT_WARNINGS_END

#include <atomic>
#include <cassert>
#include <functional>
#include <memory>

// OSC request with reference counting.
namespace Methcla { namespace Audio {

void perform_nrt_free(Environment*, void* data);
void perform_rt_free(Environment* env, void* data);

template <class T> static void perform_delete(Environment*, void* data)
{
    delete static_cast<T>(data);
}

template <class T> static void perform_perform(Environment* env, void* data)
{
    static_cast<T*>(data)->perform(env);
}

class Request
{
    typedef size_t RefCount;

    Environment* m_env;
    RefCount*    m_refs;
    void*        m_packet;
    size_t       m_size;

public:
    Request()
        : m_env(nullptr)
        , m_refs(nullptr)
        , m_packet(nullptr)
        , m_size(0)
    {
    }

    Request(Environment* env, const void* packet, size_t size)
        : m_env(env)
        , m_size(size)
    {
        // Allocate memory for packet and data block
        char* mem = Memory::allocOf<char>(sizeof(RefCount) + size);
        if (mem == nullptr)
            throw std::bad_alloc();

        m_refs = reinterpret_cast<RefCount*>(mem);
        *m_refs = 1;

        m_packet = mem + sizeof(RefCount);
        memcpy(m_packet, packet, size);
    }

    Request(const Request& other) = delete;
    Request& operator=(const Request& other) = delete;

    ~Request()
    {
        // std::cout << "~Request()\n";
        Methcla::Memory::free(m_refs);
    }

    void* packet()
    {
        return m_packet;
    }

    size_t size() const
    {
        return m_size;
    }

    void retain()
    {
        if (m_refs != nullptr)
            (*m_refs)++;
    }

    void release()
    {
        if (m_refs != nullptr)
        {
            (*m_refs)--;
            if (*m_refs == 0)
                m_env->sendToWorker(perform_delete<Request*>, this);
        }
    }
};

template <typename T> class Scheduler
{
    class Item
    {
    public:
        Item(Methcla_Time time, const T& data)
            : m_time(time)
            , m_data(data)
        { }

        Methcla_Time time() const
        {
            return m_time;
        }

        const T& data() const
        {
            return m_data;
        }

        bool operator==(const Item& other) const
        {
            return time() == other.time();
        }

        bool operator<(const Item& other) const
        {
            return time() > other.time();
        }

    private:
        Methcla_Time    m_time;
        T               m_data;
    };

    typedef boost::heap::priority_queue<
        Item,
        boost::heap::stable<true>,
        boost::heap::stability_counter_type<uint64_t>
        > PriorityQueue;

    // We need constant time size and reserve in order to avoid memory allocations from the audio thread.
    static_assert(PriorityQueue::has_reserve, "priority queue does not implement reserve()");
    static_assert(PriorityQueue::constant_time_size, "priority queue implementation has non-constant time size()");
    // We want a stable priority queue
    static_assert(PriorityQueue::is_stable, "priority queue implementation is not stable");

    size_t        m_maxSize;
    PriorityQueue m_queue;

public:
    Scheduler(size_t maxSize)
        : m_maxSize(maxSize)
    {
        m_queue.reserve(m_maxSize);
    }

    void push(Methcla_Time time, const T& data)
    {
        if (m_maxSize == 0 || m_queue.size() < m_maxSize) {
            m_queue.push(Item(time, data));
        } else {
            throw std::runtime_error("Scheduler queue overflow");
        }
    }

    bool isEmpty() const
    {
        return m_queue.empty();
    }

    Methcla_Time time() const
    {
        assert(!isEmpty());
        return m_queue.top().time();
    }

    const T& top() const
    {
        assert(!isEmpty());
        return m_queue.top().data();
    }

    void pop()
    {
        assert(!isEmpty());
        m_queue.pop();
    }
};

class EnvironmentImpl
{
public:
    struct ErrorData
    {
        int32_t requestId;
        char*   message;
    };

    static const size_t kQueueSize = 8192;

    Environment*                m_owner;

    LogHandler                  m_logHandler;
    PacketHandler               m_packetHandler;

    PluginManager               m_plugins;
    Memory::RTMemoryManager     m_rtMem;

    typedef Utility::MessageQueue<Request*> MessageQueue;
    typedef Utility::WorkerThread<Environment::Command> Worker;

    std::unique_ptr<Environment::MessageQueue> m_requests;

    // NOTE: Worker needs to be constructed before and destroyed after node map (m_nodes).
    std::unique_ptr<Environment::Worker> m_worker;

    struct ScheduledBundle
    {
        ScheduledBundle(Request* request, const OSCPP::Server::Bundle& bundle)
            : m_request(request)
            , m_bundle(bundle)
        { }

        Request*              m_request;
        OSCPP::Server::Bundle m_bundle;
    };

    Scheduler<ScheduledBundle>  m_scheduler;

    std::vector<Memory::shared_ptr<ExternalAudioBus>>   m_externalAudioInputs;
    std::vector<Memory::shared_ptr<ExternalAudioBus>>   m_externalAudioOutputs;
    std::vector<Memory::shared_ptr<AudioBus>>           m_internalAudioBuses;

    Epoch                                           m_epoch;
    Methcla_Time                                    m_currentTime;

    ResourceMap<NodeId,Node>                        m_nodes;
    ResourceRef<Group>                              m_rootNode;

    SynthDefMap                                     m_synthDefs;
    std::list<const Methcla_SoundFileAPI*>          m_soundFileAPIs;

    std::atomic<int>                                m_logFlags;

    EnvironmentImpl(Environment* owner, LogHandler logHandler, PacketHandler listener, const Environment::Options& options, Environment::MessageQueue* messageQueue, Environment::Worker* worker);
    ~EnvironmentImpl();

    // Initialization that has to take place after constructor returns
    void init(const Environment::Options& options);

    ResourceRef<Group> rootNode()
    {
        return m_rootNode;
    }

    Methcla_Time currentTime() const
    {
        return m_currentTime;
    }

    Memory::RTMemoryManager& rtMem()
    {
        return m_rtMem;
    }

    void registerSynthDef(const Methcla_SynthDef* def);
    const Memory::shared_ptr<SynthDef>& synthDef(const char* uri) const;

    void process(Methcla_Time currentTime, size_t numFrames, const sample_t* const* inputs, sample_t* const* outputs);

    void processRequests(Methcla_EngineLogFlags logFlags, const Methcla_Time currentTime);
    void processScheduler(Methcla_EngineLogFlags logFlags, const Methcla_Time currentTime, const Methcla_Time nextTime);
    void processBundle(Methcla_EngineLogFlags logFlags, Request* request, const OSCPP::Server::Bundle& bundle, const Methcla_Time scheduleTime, const Methcla_Time currentTime);
    void processMessage(Methcla_EngineLogFlags logFlags, const OSCPP::Server::Message& msg, const Methcla_Time scheduleTime, const Methcla_Time currentTime);

    void sendToWorker(PerformFunc f, void* data)
    {
        Environment::Command cmd;
        cmd.m_env = m_owner;
        cmd.m_perform = f;
        cmd.m_data = data;
        m_worker->sendToWorker(cmd);
    }

    void sendFromWorker(PerformFunc f, void* data)
    {
        Environment::Command cmd;
        cmd.m_env = m_owner;
        cmd.m_perform = f;
        cmd.m_data = data;
        m_worker->sendFromWorker(cmd);
    }

    template <class T> void sendToWorker(T* command)
    {
        sendToWorker(perform_perform<T>, command);
    }

    template <class T, class ... Args> void sendToWorker(Args&&...args)
    {
        sendToWorker(perform_perform<T>, rtMem().construct<T,Args...>(std::forward<Args>(args)...));
    }

    template <class T> void sendFromWorker(T* command)
    {
        sendFromWorker(perform_perform<T>, command);
    }

    //* Context: RT
    void freeNode(NodeId nodeId)
    {
        class CommandNotifyNodeEnded
        {
            NodeId m_nodeId;

        public:
            CommandNotifyNodeEnded(NodeId nodeId)
                : m_nodeId(nodeId)
            {}

            void perform(Environment* env)
            {
                static const char* address = "/node/ended";
                OSCPP::Client::DynamicPacket packet(
                    OSCPP::Size::message(address, 1)
                  + OSCPP::Size::int32(1)
                );
                packet.openMessage(address, 1);
                packet.int32(m_nodeId);
                packet.closeMessage();
                env->notify(packet);
                env->sendFromWorker(perform_rt_free, this);
            }
        };

        m_nodes.lookup(nodeId)->unlink();
        m_nodes.remove(nodeId);
        sendToWorker<CommandNotifyNodeEnded>(nodeId);
    }

    //* Context: NRT
    void reply(Methcla_RequestId requestId, const void* packet, size_t size)
    {
        m_packetHandler(requestId, packet, size);
    }

    //* Context: NRT
    void reply(Methcla_RequestId requestId, const OSCPP::Client::Packet& packet)
    {
        reply(requestId, packet.data(), packet.size());
    }

    //* Context: NRT
    void replyError(Methcla_RequestId requestId, const char* what)
    {
        // EnvironmentImpl::ErrorData* data =
        //     (EnvironmentImpl::ErrorData*)rtMem().alloc(sizeof(EnvironmentImpl::ErrorData)+strlen(msg)+1);
        // data->requestId = requestId;
        // data->message = (char*)data + sizeof(EnvironmentImpl::ErrorData);
        // strcpy(data->message, msg);
        // sendToWorker(perform_response_error, data);
        using namespace std::placeholders;
        auto out = nrt_log(kMethcla_LogError);
        out << "ERROR";
        if (requestId != kMethcla_Notification)
            out << "[" << requestId << "]";
        out << ": " << what;
    }

    //* Context: NRT
    void notify(const void* packet, size_t size)
    {
        m_packetHandler(kMethcla_Notification, packet, size);
    }

    //* Context: NRT
    void notify(const OSCPP::Client::Packet& packet)
    {
        notify(packet.data(), packet.size());
    }

    //* Context: RT
    void logLineRT(Methcla_LogLevel level, const char* message)
    {
        logLineNRT(level, message);
    }

    //* Context: NRT
    void logLineNRT(Methcla_LogLevel level, const char* message)
    {
        // std::cout << message << std::endl;
        m_logHandler(level, message);
    }

    LogStream rt_log(Methcla_LogLevel level)
    {
        using namespace std::placeholders;
        const Methcla_LogLevel currentLogLevel =
            m_logFlags.load() & kMethcla_EngineLogDebug
                ? kMethcla_LogDebug
                : kMethcla_LogWarn;
        return LogStream(
            std::bind(&EnvironmentImpl::logLineRT, this, _1, _2),
            level,
            currentLogLevel
        );
    }

    LogStream rt_log()
    {
        using namespace std::placeholders;
        return LogStream(
            std::bind(&EnvironmentImpl::logLineRT, this, _1, _2),
            kMethcla_LogDebug
        );
    }

    LogStream nrt_log(Methcla_LogLevel level)
    {
        using namespace std::placeholders;
        const Methcla_LogLevel currentLogLevel =
            m_logFlags.load() & kMethcla_EngineLogDebug
                ? kMethcla_LogDebug
                : kMethcla_LogWarn;
        return LogStream(
            std::bind(&EnvironmentImpl::logLineNRT, this, _1, _2),
            level,
            currentLogLevel
        );
    }

    LogStream nrt_log()
    {
        using namespace std::placeholders;
        return LogStream(
            std::bind(&EnvironmentImpl::logLineNRT, this, _1, _2),
            kMethcla_LogDebug
        );
    }
};

} // namespace Audio
} // namespace Methcla

#endif // METHCLA_AUDIO_ENGINE_IMPL_HPP_INCLUDED
