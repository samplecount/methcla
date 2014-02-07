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

#include "Methcla/Audio/Engine.hpp"
#include "Methcla/Audio/Group.hpp"
#include "Methcla/Audio/Synth.hpp"
#include "Methcla/Exception.hpp"
#include "Methcla/Memory/Manager.hpp"
#include "Methcla/Platform.hpp"
#include "Methcla/Utility/Macros.h"
#include "Methcla/Utility/MessageQueue.hpp"

METHCLA_WITHOUT_WARNINGS_BEGIN
# include <boost/heap/priority_queue.hpp>
METHCLA_WITHOUT_WARNINGS_END

#include <atomic>
#include <cassert>
#include <cstdlib>
#include <functional>
#include <iostream>

#include <oscpp/print.hpp>
#include <oscpp/util.hpp>

using namespace Methcla;
using namespace Methcla::Audio;
using namespace Methcla::Memory;

static void perform_nrt_free(Environment*, void* data)
{
    Methcla::Memory::free(data);
}

static void perform_rt_free(Environment* env, void* data)
{
    env->rtMem().free(data);
}

template <class T> static void perform_delete(Environment*, void* data)
{
    delete static_cast<T>(data);
}

template <class T> static void perform_perform(Environment* env, void* data)
{
    static_cast<T*>(data)->perform(env);
}

// OSC request with reference counting.
namespace Methcla { namespace Audio {
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
} }

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

class LogStream
{
    std::function<void(Methcla_LogLevel,const char*)> m_callback;
    Methcla_LogLevel                                  m_level;
    std::shared_ptr<std::stringstream>                m_stream;

public:
    LogStream(std::function<void(Methcla_LogLevel,const char*)> callback, Methcla_LogLevel currentLevel, Methcla_LogLevel messageLevel)
        : m_callback(messageLevel <= currentLevel ? callback : nullptr)
        , m_level(messageLevel)
        , m_stream(m_callback == nullptr ? nullptr : std::unique_ptr<std::stringstream>(new std::stringstream))
    {}

    LogStream(const LogStream&) = default;
    LogStream(LogStream&&) = default;

    ~LogStream()
    {
        if (m_callback)
            m_callback(m_level, m_stream->str().c_str());
    }

    template <class T> LogStream& operator<<(const T& x)
    {
        if (m_callback)
            *m_stream << x;
        return *this;
    }
};

class Methcla::Audio::EnvironmentImpl
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

    std::vector<shared_ptr<ExternalAudioBus>>       m_externalAudioInputs;
    std::vector<shared_ptr<ExternalAudioBus>>       m_externalAudioOutputs;
    std::vector<shared_ptr<AudioBus>>               m_internalAudioBuses;

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
    const shared_ptr<SynthDef>& synthDef(const char* uri) const;

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
        Methcla_LogLevel logLevel =
            m_logFlags.load() & kMethcla_EngineLogDebug
                ? kMethcla_LogDebug
                : kMethcla_LogWarn;
        return LogStream(std::bind(&EnvironmentImpl::logLineRT, this, _1, _2), logLevel, level);
    }

    LogStream rt_log()
    {
        using namespace std::placeholders;
        return LogStream(std::bind(&EnvironmentImpl::logLineRT, this, _1, _2), kMethcla_LogDebug, kMethcla_LogDebug);
    }

    LogStream nrt_log(Methcla_LogLevel level)
    {
        using namespace std::placeholders;
        Methcla_LogLevel logLevel =
            m_logFlags.load() & kMethcla_EngineLogDebug
                ? kMethcla_LogDebug
                : kMethcla_LogWarn;
        return LogStream(std::bind(&EnvironmentImpl::logLineNRT, this, _1, _2), logLevel, level);
    }

    LogStream nrt_log()
    {
        using namespace std::placeholders;
        return LogStream(std::bind(&EnvironmentImpl::logLineNRT, this, _1, _2), kMethcla_LogDebug, kMethcla_LogDebug);
    }
};

EnvironmentImpl::EnvironmentImpl(
    Environment* owner,
    LogHandler logHandler,
    PacketHandler listener,
    const Environment::Options& options,
    Environment::MessageQueue* messageQueue,
    Environment::Worker* worker
    )
    : m_owner(owner)
    , m_logHandler(logHandler)
    , m_packetHandler(listener)
    , m_rtMem(options.realtimeMemorySize)
    , m_requests(messageQueue == nullptr ? new Utility::MessageQueue<Request*>(kQueueSize) : messageQueue)
    , m_worker(worker ? worker : new Utility::WorkerThread<Environment::Command>(kQueueSize, 2))
    , m_scheduler(options.mode == Environment::kRealtimeMode ? kQueueSize : 0)
    , m_epoch(0)
    , m_currentTime(0)
    , m_nodes(options.maxNumNodes)
    , m_logFlags(kMethcla_EngineLogDefault)
{
    assert( m_logFlags.is_lock_free() );

    const Epoch prevEpoch = m_epoch - 1;

    m_externalAudioInputs.reserve(options.numHardwareInputChannels);
    for (size_t i=0; i < options.numHardwareInputChannels; i++)
    {
        m_externalAudioInputs.push_back(
            Memory::make_shared<ExternalAudioBus>(prevEpoch)
        );
    }

    m_externalAudioOutputs.reserve(options.numHardwareOutputChannels);
    for (size_t i=0; i < options.numHardwareOutputChannels; i++)
    {
        m_externalAudioOutputs.push_back(
            Memory::make_shared<ExternalAudioBus>(prevEpoch)
        );
    }

    for (size_t i=0; i < options.maxNumAudioBuses; i++)
    {
        m_internalAudioBuses.push_back(
            Memory::make_shared<InternalAudioBus>(options.blockSize, prevEpoch)
        );
    }
}

EnvironmentImpl::~EnvironmentImpl()
{
}

void EnvironmentImpl::init(const Environment::Options& options)
{
    // Create root group
    m_rootNode = Group::construct(*m_owner, NodeId(0));
    m_nodes.insert(m_rootNode->id(), m_rootNode);
    // Load plugins
    m_plugins.loadPlugins(*m_owner, options.pluginLibraries);
}

extern "C" {

static void methcla_api_host_notify(const Methcla_Host* host, const void* packet, size_t size)
{
    assert(host);
    assert(host->handle);
    static_cast<Environment*>(host->handle)->notify(packet, size);
}

static void methcla_api_host_log_line(const Methcla_Host* host, Methcla_LogLevel level, const char* message)
{
    assert(host);
    assert(host->handle);
    assert(message);
    static_cast<Environment*>(host->handle)->logLineNRT(level, message);
}

static void methcla_api_host_register_synthdef(const Methcla_Host* host, const Methcla_SynthDef* synthDef)
{
    assert(host && host->handle);
    assert(synthDef);
    static_cast<Environment*>(host->handle)->registerSynthDef(synthDef);
}

static void methcla_api_host_register_soundfile_api(const Methcla_Host* host, const Methcla_SoundFileAPI* api)
{
    assert(host && host->handle && api);
    static_cast<Environment*>(host->handle)->registerSoundFileAPI(api);
}

static Methcla_Error methcla_api_host_soundfile_open(const Methcla_Host* host, const char* path, Methcla_FileMode mode, Methcla_SoundFile** file, Methcla_SoundFileInfo* info)
{
    assert(host && host->handle);
    assert(path);
    assert(file);
    assert(info);

    auto& apis = static_cast<Environment*>(host->handle)->soundFileAPIs();

    if (apis.empty())
        return kMethcla_UnsupportedFileTypeError;

    // Open sound file with first API that doesn't return an error.
    for (auto it=apis.begin(); it != apis.end(); it++)
    {
        Methcla_Error result = (*it)->open(*it, path, mode, file, info);
        if (result == kMethcla_NoError)
        {
            assert(file != nullptr);
            return result;
        }
        else if (   result != kMethcla_UnsupportedFileTypeError
                 && result != kMethcla_UnsupportedDataFormatError)
        {
            return result;
        }
    }

    return kMethcla_UnsupportedFileTypeError;
}

static double methcla_api_world_samplerate(const Methcla_World* world)
{
    assert(world && world->handle);
    return static_cast<Environment*>(world->handle)->sampleRate();
}

static size_t methcla_api_world_block_size(const Methcla_World* world)
{
    assert(world && world->handle);
    return static_cast<Environment*>(world->handle)->blockSize();
}

static Methcla_Time methcla_api_world_current_time(const Methcla_World* world)
{
    assert(world != nullptr);
    assert(world->handle != nullptr);
    return static_cast<Environment*>(world->handle)->currentTime();
}

static void* methcla_api_world_alloc(const Methcla_World* world, size_t size)
{
    assert(world && world->handle);
    try {
        return static_cast<Environment*>(world->handle)->rtMem().alloc(size);
    } catch (std::invalid_argument) {
    } catch (std::bad_alloc) {
    }
    return nullptr;
}

static void* methcla_api_world_alloc_aligned(const Methcla_World* world, size_t alignment, size_t size)
{
    assert(world && world->handle);
    try {
        return static_cast<Environment*>(world->handle)->rtMem().allocAligned(alignment, size);
    } catch (std::invalid_argument) {
    } catch (std::bad_alloc) {
    }
    return nullptr;
}

static void methcla_api_world_free(const Methcla_World* world, void* ptr)
{
    assert(world && world->handle);
    return static_cast<Environment*>(world->handle)->rtMem().free(ptr);
}

static void methcla_api_world_log_line(const Methcla_World* world, Methcla_LogLevel level, const char* message)
{
    assert(world);
    assert(world->handle);
    static_cast<Environment*>(world->handle)->logLineRT(level, message);
}

static void methcla_api_world_synth_retain(const Methcla_World*, Methcla_Synth* synth)
{
    assert(synth);
    Synth::fromSynth(synth)->retain();
}

static void methcla_api_world_synth_release(const Methcla_World*, Methcla_Synth* synth)
{
    assert(synth);
    Synth::fromSynth(synth)->release();
}

static void methcla_api_world_synth_done(const Methcla_World*, Methcla_Synth* synth)
{
    assert(synth != nullptr);
    Synth::fromSynth(synth)->setDone();
}

static void methcla_api_host_perform_command(const Methcla_Host*, Methcla_WorldPerformFunction, void*);
static void methcla_api_world_perform_command(const Methcla_World*, Methcla_HostPerformFunction, void*);

} // extern "C"

Environment::Environment(
    LogHandler logHandler,
    PacketHandler packetHandler,
    const Options& options,
    MessageQueue* messageQueue,
    Worker* worker
    )
    : m_sampleRate(options.sampleRate)
    , m_blockSize(options.blockSize)
{
    // Initialize Methcla_Host interface
    m_host = {
        this,
        methcla_api_host_register_synthdef,
        methcla_api_host_register_soundfile_api,
        methcla_api_host_soundfile_open,
        methcla_api_host_perform_command,
        methcla_api_host_notify,
        methcla_api_host_log_line
    };

    // Initialize Methcla_World interface
    m_world = {
        this,
        methcla_api_world_samplerate,
        methcla_api_world_block_size,
        methcla_api_world_current_time,
        methcla_api_world_alloc,
        methcla_api_world_alloc_aligned,
        methcla_api_world_free,
        methcla_api_world_perform_command,
        methcla_api_world_log_line,
        methcla_api_world_synth_retain,
        methcla_api_world_synth_release,
        methcla_api_world_synth_done
    };

    m_impl = new EnvironmentImpl(this, logHandler, packetHandler, options, messageQueue, worker);
    m_impl->init(options);

    using namespace std::placeholders;

    m_impl->nrt_log(kMethcla_LogDebug)
        << "Methcla engine (version " << methcla_version() << ")";
}

Environment::~Environment()
{
    delete m_impl;
}

//* Convert environment to Methcla_Host.
Environment::operator const Methcla_Host* () const
{
    return &m_host;
}

ResourceRef<Group> Environment::rootNode()
{
    return m_impl->rootNode();
}

//* Convert environment to Methcla_World.
Environment::operator const Methcla_World* () const
{
    return &m_world;
}

size_t Environment::numAudioBuses() const
{
    return m_impl->m_internalAudioBuses.size();
}

AudioBus* Environment::audioBus(AudioBusId id)
{
    return m_impl->m_internalAudioBuses.at(id).get();
}

size_t Environment::numExternalAudioOutputs() const
{
    return m_impl->m_externalAudioOutputs.size();
}

size_t Environment::numExternalAudioInputs() const
{
    return m_impl->m_externalAudioInputs.size();
}

AudioBus* Environment::externalAudioOutput(AudioBusId id)
{
    return m_impl->m_externalAudioOutputs.at(id).get();
}

AudioBus* Environment::externalAudioInput(AudioBusId id)
{
    return m_impl->m_externalAudioInputs.at(id).get();
}

Memory::RTMemoryManager& Environment::rtMem()
{
    return m_impl->rtMem();
}

Epoch Environment::epoch() const
{
    return m_impl->m_epoch;
}

Methcla_Time Environment::currentTime() const
{
    return m_impl->currentTime();
}

void Environment::send(const void* packet, size_t size)
{
    m_impl->m_requests->send(new Request(this, packet, size));
}

bool Environment::hasPendingCommands() const
{
    return !m_impl->m_scheduler.isEmpty();
}

void Environment::sendToWorker(PerformFunc f, void* data)
{
    m_impl->sendToWorker(f, data);
}

void Environment::sendFromWorker(PerformFunc f, void* data)
{
    m_impl->sendFromWorker(f, data);
}

void Environment::process(Methcla_Time currentTime, size_t numFrames, const sample_t* const* inputs, sample_t* const* outputs)
{
    assert( numFrames <= blockSize() );
    m_impl->process(currentTime, numFrames, inputs, outputs);
}

void Environment::setLogFlags(Methcla_EngineLogFlags flags)
{
    m_impl->m_logFlags.store(flags);
}

void Environment::logLineRT(Methcla_LogLevel level, const char* message)
{
    m_impl->logLineRT(level, message);
}

void Environment::logLineNRT(Methcla_LogLevel level, const char* message)
{
    m_impl->logLineNRT(level, message);
}

void Environment::freeNode(NodeId nodeId)
{
    m_impl->freeNode(nodeId);
}

void Environment::reply(Methcla_RequestId requestId, const void* packet, size_t size)
{
    m_impl->reply(requestId, packet, size);
}

void Environment::reply(Methcla_RequestId requestId, const OSCPP::Client::Packet& packet)
{
    m_impl->reply(requestId, packet);
}

void Environment::replyError(Methcla_RequestId requestId, const char* msg)
{
    m_impl->replyError(requestId, msg);
}

void Environment::notify(const void* packet, size_t size)
{
    m_impl->notify(packet, size);
}

void Environment::notify(const OSCPP::Client::Packet& packet)
{
    m_impl->notify(packet);
}

void EnvironmentImpl::process(Methcla_Time currentTime, size_t numFrames, const sample_t* const* inputs, sample_t* const* outputs)
{
    // Update current time
    m_currentTime = currentTime;

    // Load log flags
    const Methcla_EngineLogFlags logFlags = (Methcla_EngineLogFlags)m_logFlags.load();

    // Process external requests
    processRequests(logFlags, currentTime);
    // Process scheduled requests
    processScheduler(logFlags, currentTime, currentTime + numFrames / m_owner->sampleRate());
    // std::cout << "Environment::process " << currentTime << std::endl;

    // Process non-realtime commands
    m_worker->perform();

    const size_t numExternalInputs = m_externalAudioInputs.size();
    const size_t numExternalOutputs = m_externalAudioOutputs.size();

    // Connect input and output buses
    for (size_t i=0; i < numExternalInputs; i++)
    {
        m_externalAudioInputs[i]->setData(const_cast<sample_t*>(inputs[i]));
        m_externalAudioInputs[i]->setEpoch(m_epoch);
    }

    for (size_t i=0; i < numExternalOutputs; i++)
    {
        m_externalAudioOutputs[i]->setData(outputs[i]);
    }

    // Run DSP graph
    m_rootNode->process(numFrames);

    // Zero outputs that haven't been written to
    for (size_t i=0; i < numExternalOutputs; i++)
    {
        if (m_externalAudioOutputs[i]->epoch() != m_epoch)
        {
            memset(outputs[i], 0, numFrames * sizeof(sample_t));
        }
    }

    m_epoch++;
}

void EnvironmentImpl::processRequests(Methcla_EngineLogFlags logFlags, const Methcla_Time currentTime)
{
    Request* request;
    while (m_requests->next(request))
    {
        try
        {
            OSCPP::Server::Packet packet(request->packet(), request->size());
            if (packet.isBundle())
            {
                OSCPP::Server::Bundle bundle(packet);
                Methcla_Time bundleTime = methcla_time_from_uint64(bundle.time());
                if (bundleTime == 0.)
                {
                    processBundle(logFlags, request, bundle, currentTime, currentTime);
                }
                else
                {
                    request->retain();
                    m_scheduler.push(bundleTime, ScheduledBundle(request, bundle));
                }
            }
            else
            {
                processMessage(logFlags, packet, currentTime, currentTime);
            }
            request->release();
        }
        catch (OSCPP::Error&)
        {
            replyError(kMethcla_Notification, "Couldn't parse request packet");
        }
        catch (std::exception& e)
        {
            replyError(kMethcla_Notification, e.what());
        }
    }
}

void EnvironmentImpl::processScheduler(Methcla_EngineLogFlags logFlags, const Methcla_Time currentTime, const Methcla_Time nextTime)
{
    while (!m_scheduler.isEmpty())
    {
        Methcla_Time scheduleTime = m_scheduler.time();
        if (scheduleTime < nextTime)
        {
#if DEBUG
            if (scheduleTime < currentTime)
                rt_log() << "Late " << scheduleTime << " " << currentTime << " " << nextTime;
#endif // DEBUG
            ScheduledBundle bundle = m_scheduler.top();
            assert( methcla_time_from_uint64(bundle.m_bundle.time()) == scheduleTime );
            processBundle(logFlags, bundle.m_request, bundle.m_bundle, scheduleTime, currentTime);
            m_scheduler.pop();
            bundle.m_request->release();
        }
        else
        {
            break;
        }
    }
}

void EnvironmentImpl::processBundle(Methcla_EngineLogFlags logFlags, Request* request, const OSCPP::Server::Bundle& bundle, const Methcla_Time scheduleTime, const Methcla_Time currentTime)
{
    auto packets = bundle.packets();
    while (!packets.atEnd())
    {
        auto packet = packets.next();
        if (packet.isBundle())
        {
            OSCPP::Server::Bundle innerBundle(packet);
            Methcla_Time innerBundleTime = methcla_time_from_uint64(innerBundle.time());
            if (innerBundleTime <= scheduleTime)
            {
                processBundle(logFlags, request, innerBundle, scheduleTime, currentTime);
            }
            else
            {
                request->retain();
                m_scheduler.push(innerBundleTime, ScheduledBundle(request, innerBundle));
            }
        }
        else
        {
            processMessage(logFlags, packet, scheduleTime, currentTime);
        }
    }
}

static void throwError(Methcla_Error code, const std::string& msg)
{
    throw Error(code, msg);
}

static void throwErrorWith(Methcla_Error code, std::function<void(std::stringstream&)> func)
{
    std::stringstream stream;
    func(stream);
    throwError(code, stream.str());
}

template <class T> const char* nodeTypeName()
{
    return "<unknown>";
}

template <> const char* nodeTypeName<Group>()
{
    return "group";
}

template <> const char* nodeTypeName<Synth>()
{
    return "synth";
}

Node* lookupNode(EnvironmentImpl* env, const char* prefix, NodeId nodeId)
{
    Node* node = env->m_nodes.lookup(nodeId).get();

    if (node == nullptr)
        throwErrorWith(kMethcla_NodeIdError, [&](std::stringstream& s) {
            s << prefix << " " << nodeId << " not found";
        });

    return node;
}

template <class T> T* lookupNodeAs(EnvironmentImpl* env, const char* prefix, NodeId nodeId)
{
    Node* node = lookupNode(env, prefix, nodeId);

    T* result = dynamic_cast<T*>(node);
    if (result == nullptr)
        throwErrorWith(kMethcla_NodeIdError, [&](std::stringstream& s) {
            s << nodeId << " is not a " << nodeTypeName<T>();
        });

    return result;
}

static void addNodeToTarget(Node* target, Node* node, Methcla_NodePlacement nodePlacement)
{
    switch (nodePlacement)
    {
        case kMethcla_NodePlacementHeadOfGroup:
            if (!target->isGroup())
                throwErrorWith(kMethcla_NodeIdError, [&](std::stringstream& s) {
                    s << "Target node " << target->id() << " is not a group";
                });
            dynamic_cast<Group*>(target)->addToHead(node);
            break;
        case kMethcla_NodePlacementTailOfGroup:
            if (!target->isGroup())
                throwErrorWith(kMethcla_NodeIdError, [&](std::stringstream& s) {
                    s << "Target node " << target->id() << " is not a group";
                });
            dynamic_cast<Group*>(target)->addToTail(node);
            break;
        case kMethcla_NodePlacementBeforeNode:
            if (target->parent() == nullptr)
                throwError(kMethcla_NodeIdError, "Cannot place node before root node");
            target->parent()->addBefore(target, node);
            break;
        case kMethcla_NodePlacementAfterNode:
            if (target->parent() == nullptr)
                throwError(kMethcla_NodeIdError, "Cannot place node after root node");
            target->parent()->addAfter(target, node);
            break;
        default:
            throwError(kMethcla_ArgumentError, "Invalid node placement specification");
    }
}

void EnvironmentImpl::processMessage(Methcla_EngineLogFlags logFlags, const OSCPP::Server::Message& msg, Methcla_Time scheduleTime, Methcla_Time currentTime)
{
    using namespace std::placeholders;

    if (logFlags & kMethcla_EngineLogRequests)
        rt_log() << "Request: " << msg;

    auto args = msg.args();
    // Methcla_RequestId requestId = args.int32();

    try
    {
        if (msg == "/group/new")
        {
            NodeId nodeId = NodeId(args.int32());
            NodeId targetId = NodeId(args.int32());
            Methcla_NodePlacement nodePlacement = Methcla_NodePlacement(args.int32());

            auto target = m_nodes.lookup(targetId);
            if (target == nullptr)
                throwErrorWith(kMethcla_NodeIdError, [&](std::stringstream& s) {
                    s << "Target node " << targetId << " not found";
                });

            auto group = Group::construct(*m_owner, nodeId);
            addNodeToTarget(target.get(), group.get(), nodePlacement);

            m_nodes.insert(group->id(), group);
        }
        else if (msg == "/group/freeAll")
        {
            NodeId nodeId = NodeId(args.int32());

            Node* node = m_nodes.lookup(nodeId).get();
            if (node == nullptr)
                throwErrorWith(kMethcla_NodeIdError, [&](std::stringstream& s) {
                    s << "Group " << nodeId << " not found";
                });
            else if (!node->isGroup())
                throwErrorWith(kMethcla_NodeIdError, [&](std::stringstream& s) {
                    s << "Node " << nodeId << " is not a group";
                });

            Group* group =  dynamic_cast<Group*>(node);

            group->freeAll();
        }
        else if (msg == "/synth/new")
        {
            const char* defName = args.string();
            NodeId nodeId = NodeId(args.int32());
            NodeId targetId = NodeId(args.int32());
            Methcla_NodePlacement nodePlacement = Methcla_NodePlacement(args.int32());

            const shared_ptr<SynthDef> def = m_owner->synthDef(defName);

            auto synthControls = args.atEnd() ? OSCPP::Server::ArgStream() : args.array();
            // FIXME: Cannot be checked before the synth is instantiated.
            // if (def->numControlInputs() != synthControls.size()) {
            //     throw std::runtime_error("Missing synth control initialisers");
            // }
            auto synthArgs = args.atEnd() ? OSCPP::Server::ArgStream() : args.array();

            auto target = m_nodes.lookup(targetId);
            if (target == nullptr)
                throwErrorWith(kMethcla_NodeIdError, [&](std::stringstream& s) {
                    s << "Target node " << targetId << " not found";
                });

            try
            {
                ResourceRef<Synth> synth = Synth::construct(
                    *m_owner,
                    nodeId,
                    *def,
                    synthControls,
                    synthArgs);

                addNodeToTarget(target.get(), synth.get(), nodePlacement);

                m_nodes.insert(synth->id(), synth);
            }
            catch (OSCPP::UnderrunError&)
            {
                throwErrorWith(kMethcla_ArgumentError, [&](std::stringstream& s) {
                    s << "Missing control initializer for synth " << nodeId;
                });
            }
            catch (OSCPP::ParseError&)
            {
                throwErrorWith(kMethcla_ArgumentError, [&](std::stringstream& s) {
                    s << "Invalid control initializer for synth " << nodeId;
                });
            }
        }
        else if (msg == "/synth/activate")
        {
            NodeId nodeId = NodeId(args.int32());
            Node* node = m_nodes.lookup(nodeId).get();
            if (node == nullptr)
                throwErrorWith(kMethcla_NodeIdError, [&](std::stringstream& s) {
                    s << "Synth " << nodeId << " not found";
                });
            else if (!node->isSynth())
                throwErrorWith(kMethcla_NodeIdError, [&](std::stringstream& s) {
                    s << "Node " << nodeId << " is not a synth";
                });
            Synth* synth = dynamic_cast<Synth*>(node);
            // TODO: Use sample rate estimate from driver
            const double sampleOffset = std::max(0., (scheduleTime - currentTime) * m_owner->sampleRate());
            synth->activate(sampleOffset);
        }
        else if (msg == "/synth/map/input")
        {
            NodeId nodeId = NodeId(args.int32());
            int32_t index = args.int32();
            int32_t busId = AudioBusId(args.int32());
            Methcla_BusMappingFlags flags = Methcla_BusMappingFlags(args.int32());

            if (busId < 0 || ((flags & kMethcla_BusMappingExternal) && (size_t)busId > m_externalAudioInputs.size())
                          || ((size_t)busId > m_internalAudioBuses.size()))
                throwErrorWith(kMethcla_ArgumentError, [&](std::stringstream& s) {
                    s << "Audio bus id " << busId << " out of range";
                });

            Node* node = m_nodes.lookup(nodeId).get();
            if (node == nullptr)
                throwErrorWith(kMethcla_NodeIdError, [&](std::stringstream& s) {
                    s << "Synth " << nodeId << " not found";
                });
            else if (!node->isSynth())
                throwErrorWith(kMethcla_NodeIdError, [&](std::stringstream& s) {
                    s << "Node " << nodeId << " is not a synth";
                });

            Synth* synth = dynamic_cast<Synth*>(node);

            if ((index < 0) || (index >= (int32_t)synth->numAudioInputs()))
                throwErrorWith(kMethcla_ArgumentError, [&](std::stringstream& s) {
                    s << "Audio input index " << index << " out of range for synth " << nodeId;
                });

            synth->mapInput(index, AudioBusId(busId), flags);
        }
        else if (msg == "/synth/map/output")
        {
            NodeId nodeId = NodeId(args.int32());
            int32_t index = args.int32();
            int32_t busId = args.int32();
            Methcla_BusMappingFlags flags = Methcla_BusMappingFlags(args.int32());

            if (busId < 0 || ((flags & kMethcla_BusMappingExternal) && (size_t)busId > m_externalAudioOutputs.size())
                          || ((size_t)busId > m_internalAudioBuses.size()))
                throwErrorWith(kMethcla_ArgumentError, [&](std::stringstream& s) {
                    s << "Audio bus id " << busId << " out of range";
                });

            Node* node = m_nodes.lookup(nodeId).get();
            if (node == nullptr)
                throwErrorWith(kMethcla_NodeIdError, [&](std::stringstream& s) {
                    s << "Synth " << nodeId << " not found";
                });
            else if (!node->isSynth())
                throwErrorWith(kMethcla_NodeIdError, [&](std::stringstream& s) {
                    s << "Node " << nodeId << " is not a synth";
                });

            Synth* synth = dynamic_cast<Synth*>(node);
            if ((index < 0) || (index >= (int32_t)synth->numAudioOutputs()))
                throwErrorWith(kMethcla_ArgumentError, [&](std::stringstream& s) {
                    s << "Audio output index " << index << " out of range for synth " << nodeId;
                });

            synth->mapOutput(index, AudioBusId(busId), flags);
        }
        else if (msg == "/synth/property/doneFlags/set")
        {
            NodeId nodeId = NodeId(args.int32());
            Methcla_NodeDoneFlags flags = Methcla_NodeDoneFlags(args.int32());
            Synth* synth = lookupNodeAs<Synth>(this, "Synth", nodeId);
            synth->setDoneFlags(flags);
        }
        else if (msg == "/node/free")
        {
            NodeId nodeId = NodeId(args.int32());

            // Check node id validity
            if (!m_nodes.contains(nodeId))
                throwErrorWith(kMethcla_NodeIdError, [&](std::stringstream& s) {
                    s << "Node " << nodeId << " not found";
                });
            else if (nodeId == m_rootNode->id())
                throwErrorWith(kMethcla_NodeIdError, [&](std::stringstream& s) {
                    s << "Cannot free root node " << nodeId;
                });

            // Drop node reference
            freeNode(nodeId);
        }
        else if (msg == "/node/set")
        {
            NodeId nodeId = NodeId(args.int32());
            int32_t index = args.int32();
            float value = args.float32();

            Node* node = m_nodes.lookup(nodeId).get();
            if (node == nullptr)
                throwErrorWith(kMethcla_NodeIdError, [&](std::stringstream& s) {
                    s << "Node " << nodeId << " not found";
                });
            else if (!node->isSynth())
                throwErrorWith(kMethcla_NodeIdError, [&](std::stringstream& s) {
                    s << "Node " << nodeId << " is not a synth";
                });

            Synth* synth = dynamic_cast<Synth*>(node);

            if ((index < 0) || (index >= (int32_t)synth->numControlInputs()))
                throwErrorWith(kMethcla_ArgumentError, [&](std::stringstream& s) {
                    s << "Control input index " << index << " out of range for synth " << nodeId;
                });

            synth->controlInput(index) = value;
        }
        else if (msg == "/node/tree/statistics")
        {
            class CommandNodeTreeStatistics
            {
            public:
                struct Statistics
                {
                    Statistics()
                        : numGroups(0)
                        , numSynths(0)
                    { }

                    size_t numGroups = 0;
                    size_t numSynths = 0;
                };

                static Statistics collectStatistics(const Group* group, Statistics stats=Statistics())
                {
                    stats.numGroups++;

                    const Node* cur = group->first();

                    while (cur != nullptr)
                    {
                        const Group* subGroup = dynamic_cast<const Group*>(cur);
                        if (subGroup == nullptr)
                        {
                            stats.numSynths++;
                        }
                        else
                        {
                            stats = collectStatistics(subGroup, stats);
                        }
                        cur = cur->next();
                    }

                    return stats;
                }

                CommandNodeTreeStatistics(Methcla_RequestId requestId, Statistics stats)
                    : m_requestId(requestId)
                    , m_stats(stats)
                {
                }

                void perform(Environment* env)
                {
                    static const char* address = "/node/tree/statistics";
                    OSCPP::Client::DynamicPacket packet(
                        OSCPP::Size::message(address, 2)
                      + OSCPP::Size::int32(2)
                    );
                    packet.openMessage(address, 2);
                    packet.int32(m_stats.numGroups);
                    packet.int32(m_stats.numSynths);
                    packet.closeMessage();
                    env->reply(m_requestId, packet);
                    env->sendFromWorker(perform_rt_free, this);
                }

            private:
                Methcla_RequestId m_requestId;
                Statistics        m_stats;
            };

            Methcla_RequestId requestId = args.int32();

            CommandNodeTreeStatistics::Statistics stats =
                CommandNodeTreeStatistics::collectStatistics(rootNode().get());

            sendToWorker<CommandNodeTreeStatistics>(requestId, stats);
        }
    }
    catch (std::exception& e)
    {
        std::stringstream s;
        s << msg.address() << ": " << e.what();
        replyError(kMethcla_Notification, s.str().c_str());
    }
}

void Environment::registerSynthDef(const Methcla_SynthDef* def)
{
    m_impl->registerSynthDef(def);
}

void EnvironmentImpl::registerSynthDef(const Methcla_SynthDef* def)
{
    auto synthDef = Memory::make_shared<SynthDef>(def);
    m_synthDefs[synthDef->uri()] = synthDef;
}

const shared_ptr<SynthDef>& Environment::synthDef(const char* uri) const
{
    return m_impl->synthDef(uri);
}

const shared_ptr<SynthDef>& EnvironmentImpl::synthDef(const char* uri) const
{
    auto it = m_synthDefs.find(uri);
    if (it == m_synthDefs.end())
        throw std::runtime_error("Synth definition not found");
    return it->second;
}

void Environment::registerSoundFileAPI(const Methcla_SoundFileAPI* api)
{
    m_impl->m_soundFileAPIs.push_front(api);
}

const std::list<const Methcla_SoundFileAPI*>& Environment::soundFileAPIs() const
{
    return m_impl->m_soundFileAPIs;
}

template <typename T> struct CallbackData
{
    T     func;
    void* arg;
};

static void perform_worldCommand(Environment* env, void* data)
{
    CallbackData<Methcla_WorldPerformFunction>* self = (CallbackData<Methcla_WorldPerformFunction>*)data;
    self->func(*env, self->arg);
    env->sendToWorker(perform_nrt_free, self);
}

static void methcla_api_host_perform_command(const Methcla_Host* host, Methcla_WorldPerformFunction perform, void* data)
{
    Environment* env = static_cast<Environment*>(host->handle);
    CallbackData<Methcla_WorldPerformFunction>* callbackData = Methcla::Memory::allocOf<CallbackData<Methcla_WorldPerformFunction>>();
    callbackData->func = perform;
    callbackData->arg = data;
    env->sendFromWorker(perform_worldCommand, callbackData);
}

static void perform_hostCommand(Environment* env, void* data)
{
    CallbackData<Methcla_HostPerformFunction>* self = (CallbackData<Methcla_HostPerformFunction>*)data;
    self->func(*env, self->arg);
    env->sendFromWorker(perform_rt_free, self);
}

static void methcla_api_world_perform_command(const Methcla_World* world, Methcla_HostPerformFunction perform, void* data)
{
    Environment* env = static_cast<Environment*>(world->handle);
    CallbackData<Methcla_HostPerformFunction>* callbackData = env->rtMem().allocOf<CallbackData<Methcla_HostPerformFunction>>();
    callbackData->func = perform;
    callbackData->arg = data;
    env->sendToWorker(perform_hostCommand, callbackData);
}

Methcla_LogHandler Methcla::Platform::defaultLogHandler()
{
    Methcla_LogHandler handler;
    handler.handle = nullptr;
    handler.log_line = [](void*, Methcla_LogLevel, const char* message){
        std::cout << message << std::endl;
    };
    return handler;
}
