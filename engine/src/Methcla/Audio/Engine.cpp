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
#include "Methcla/Utility/MessageQueue.hpp"

#if defined(__clang__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunused-parameter"
#endif
#include <boost/heap/priority_queue.hpp>
#if defined(__clang__)
# pragma clang diagnostic pop
#endif

#include <atomic>
#include <cstdlib>
#include <iostream>
#include <oscpp/print.hpp>

using namespace Methcla;
using namespace Methcla::Audio;
using namespace Methcla::Memory;

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
                    m_env->sendToWorker(perform_delete_request, this);
            }
        }

    private:
        static void perform_delete_request(Environment*, CommandData* data)
        {
            delete static_cast<Request*>(data);
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

    std::vector<std::shared_ptr<ExternalAudioBus>>  m_externalAudioInputs;
    std::vector<std::shared_ptr<ExternalAudioBus>>  m_externalAudioOutputs;
    std::vector<std::shared_ptr<AudioBus>>          m_internalAudioBuses;
    Epoch                                           m_epoch;

    ResourceMap<NodeId,Node>                        m_nodes;
    ResourceRef<Group>                              m_rootNode;

    SynthDefMap                                     m_synthDefs;
    std::list<const Methcla_SoundFileAPI*>          m_soundFileAPIs;

    std::atomic<Methcla_EngineLogFlags>             m_logFlags;

    EnvironmentImpl(Environment* owner, const Environment::Options& options, Environment::MessageQueue* messageQueue, Environment::Worker* worker);
    ~EnvironmentImpl();

    // Initialization that has to take place after constructor returns
    void init(const Environment::Options& options);

    void registerSynthDef(const Methcla_SynthDef* def);
    const std::shared_ptr<SynthDef>& synthDef(const char* uri) const;

    void process(Methcla_Time currentTime, size_t numFrames, const sample_t* const* inputs, sample_t* const* outputs);

    void processRequests(Methcla_EngineLogFlags logFlags, Methcla_Time currentTime);
    void processScheduler(Methcla_EngineLogFlags logFlags, Methcla_Time currentTime, Methcla_Time nextTime);
    void processBundle(Methcla_EngineLogFlags logFlags, Request* request, const OSCPP::Server::Bundle& bundle, Methcla_Time scheduleTime, Methcla_Time currentTime);
    void processMessage(Methcla_EngineLogFlags logFlags, const OSCPP::Server::Message& msg, Methcla_Time scheduleTime, Methcla_Time currentTime);

    void sendToWorker(const Environment::Command& cmd)
    {
        m_worker->sendToWorker(cmd);
    }

    void sendFromWorker(const Environment::Command& cmd)
    {
        m_worker->sendFromWorker(cmd);
    }

    void releaseNode(NodeId nodeId)
    {
        m_nodes.remove(nodeId);
    }
};

EnvironmentImpl::EnvironmentImpl(
    Environment* owner,
    const Environment::Options& options,
    Environment::MessageQueue* messageQueue,
    Environment::Worker* worker
    )
    : m_owner(owner)
    , m_rtMem(options.realtimeMemorySize)
    , m_requests(messageQueue == nullptr ? new Utility::MessageQueue<Request*>(kQueueSize) : messageQueue)
    , m_worker(worker ? worker : new Utility::WorkerThread<Environment::Command>(kQueueSize, 2))
    , m_scheduler(options.mode == Environment::kRealtimeMode ? kQueueSize : 0)
    , m_epoch(0)
    , m_nodes(options.maxNumNodes)
    , m_logFlags(kMethcla_EngineLogDefault)
{
    const Epoch prevEpoch = m_epoch - 1;

    m_externalAudioInputs.reserve(options.numHardwareInputChannels);
    for (size_t i=0; i < options.numHardwareInputChannels; i++)
    {
        m_externalAudioInputs.push_back(
            std::make_shared<ExternalAudioBus>(prevEpoch)
        );
    }

    m_externalAudioOutputs.reserve(options.numHardwareOutputChannels);
    for (size_t i=0; i < options.numHardwareOutputChannels; i++)
    {
        m_externalAudioOutputs.push_back(
            std::make_shared<ExternalAudioBus>(prevEpoch)
        );
    }

    for (size_t i=0; i < options.maxNumAudioBuses; i++)
    {
        m_internalAudioBuses.push_back(
            std::make_shared<InternalAudioBus>(options.blockSize, prevEpoch)
        );
    }
}

EnvironmentImpl::~EnvironmentImpl()
{
}

void EnvironmentImpl::init(const Environment::Options& options)
{
    // Create root group
    m_rootNode = Group::construct(*m_owner, NodeId(0), nullptr, Node::kAddToTail);
    m_nodes.insert(m_rootNode->id(), m_rootNode);
    // Load plugins
    m_plugins.loadPlugins(*m_owner, options.pluginLibraries);
}

extern "C" {

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

static void methcla_api_world_resource_retain(const Methcla_World*, Methcla_Resource* resource)
{
    assert(resource);
    static_cast<Reference*>(resource)->retain();
}

static void methcla_api_world_resource_release(const Methcla_World*, Methcla_Resource* resource)
{
    assert(resource);
    static_cast<Reference*>(resource)->release();
}

static void methcla_api_host_perform_command(const Methcla_Host*, Methcla_WorldPerformFunction, void*);
static void methcla_api_world_perform_command(const Methcla_World*, Methcla_HostPerformFunction, void*);

} // extern "C"

Environment::Environment(PacketHandler handler, const Options& options, MessageQueue* messageQueue, Worker* worker)
    : m_sampleRate(options.sampleRate)
    , m_blockSize(options.blockSize)
    , m_listener(handler)
{
    // Initialize Methcla_Host interface
    m_host = {
        this,
        methcla_api_host_register_synthdef,
        methcla_api_host_register_soundfile_api,
        methcla_api_host_soundfile_open,
        methcla_api_host_perform_command
    };

    // Initialize Methcla_World interface
    m_world = {
        this,
        methcla_api_world_samplerate,
        methcla_api_world_block_size,
        methcla_api_world_alloc,
        methcla_api_world_alloc_aligned,
        methcla_api_world_free,
        methcla_api_world_perform_command,
        methcla_api_world_resource_retain,
        methcla_api_world_resource_release
    };

    m_impl = new EnvironmentImpl(this, options, messageQueue, worker);
    m_impl->init(options);
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
    return m_impl->m_rootNode;
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
    return m_impl->m_rtMem;
}

Epoch Environment::epoch() const
{
    return m_impl->m_epoch;
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
    Command cmd;
    cmd.m_env = this;
    cmd.m_perform = f;
    cmd.m_data = data;
    m_impl->sendToWorker(cmd);
}

void Environment::sendFromWorker(PerformFunc f, void* data)
{
    Command cmd;
    cmd.m_env = this;
    cmd.m_perform = f;
    cmd.m_data = data;
    m_impl->sendFromWorker(cmd);
}

void Environment::process(Methcla_Time currentTime, size_t numFrames, const sample_t* const* inputs, sample_t* const* outputs)
{
    BOOST_ASSERT_MSG( numFrames <= blockSize(), "numFrames exceeds blockSize()" );
    m_impl->process(currentTime, numFrames, inputs, outputs);
}

void Environment::setLogFlags(Methcla_EngineLogFlags flags)
{
    m_impl->m_logFlags = flags;
}

static void perform_nrt_free(Environment*, CommandData* data)
{
    Methcla::Memory::free(data);
}

static void perform_rt_free(Environment* env, CommandData* data)
{
    env->rtMem().free(data);
}

void Environment::perform_response_ack(Environment* env, CommandData* data)
{
    const char address[] = "/ack";
    const size_t numArgs = 1;
    const size_t packetSize = OSCPP::Size::message(address, numArgs)
                                + OSCPP::Size::int32();
    const int32_t requestId = *static_cast<int32_t*>(data);
    OSCPP::Client::StaticPacket<packetSize> packet;
    // OSCPP::Client::DynamicPacket packet(kPacketSize);
    packet
        .openMessage(address, numArgs)
            .int32(requestId)
        .closeMessage();
    env->reply(requestId, packet);
    env->sendFromWorker(perform_rt_free, data);
}

void Environment::perform_response_nodeId(Environment* env, CommandData* data)
{
    const char address[] = "/ack";
    const size_t numArgs = 2;
    const size_t packetSize = OSCPP::Size::message(address, numArgs)
                                + 2 * OSCPP::Size::int32();
    const int32_t requestId = ((int32_t*)data)[0];
    const int32_t nodeId = ((int32_t*)data)[1];
    OSCPP::Client::StaticPacket<packetSize> packet;
    // OSCPP::Client::DynamicPacket packet(kPacketSize);
    packet
        .openMessage(address, numArgs)
            .int32(requestId)
            .int32(nodeId)
        .closeMessage();
    env->reply(requestId, packet);
    env->sendFromWorker(perform_rt_free, data);
}

void Environment::perform_response_query_external_inputs(Environment* env, CommandData* data)
{
    // const char address[] = "/ack";
    // const size_t numBuses = env->numExternalAudioInputs();
    // const size_t numArgs = 1 + numBuses;
    // const size_t packetSize = OSCPP::Size::message(address, numArgs) + numArgs * OSCPP::Size::int32();
    // OSCPP::Client::DynamicPacket packet(packetSize);
    // packet.openMessage(address, numArgs);
    // packet.int32(data->response.requestId);
    // for (size_t i=0; i < numBuses; i++) {
    //     packet.int32(env->externalAudioInput(i).id());
    // }
    // packet.closeMessage();
    // env->reply(data->response.requestId, packet);
}

void Environment::perform_response_query_external_outputs(Environment* env, CommandData* data)
{
    // const char address[] = "/ack";
    // const size_t numBuses = env->numExternalAudioOutputs();
    // const size_t numArgs = 1 + numBuses;
    // const size_t packetSize = OSCPP::Size::message(address, numArgs) +  numArgs * OSCPP::Size::int32();
    // OSCPP::Client::DynamicPacket packet(packetSize);
    // packet.openMessage(address, numArgs);
    // packet.int32(data->response.requestId);
    // for (size_t i=0; i < numBuses; i++) {
    //     packet.int32(env->externalAudioOutput(i).id());
    // }
    // packet.closeMessage();
    // env->reply(data->response.requestId, packet);
}

void Environment::perform_response_error(Environment* env, CommandData* commandData)
{
    EnvironmentImpl::ErrorData* data = (EnvironmentImpl::ErrorData*)commandData;
    const char address[] = "/error";
    const size_t numArgs = 2;
    const size_t packetSize = OSCPP::Size::message(address, numArgs)
                            + OSCPP::Size::int32()
                            + OSCPP::Size::string(strlen(data->message));
    OSCPP::Client::DynamicPacket packet(packetSize);
    packet
        .openMessage(address, numArgs)
            .int32(data->requestId)
            .string(data->message)
        .closeMessage();
    env->reply(data->requestId, packet);
    env->sendFromWorker(perform_rt_free, data);
}

void Environment::replyError(Methcla_RequestId requestId, const char* msg)
{
#if 0
    EnvironmentImpl::ErrorData* data =
        (EnvironmentImpl::ErrorData*)rtMem().alloc(sizeof(EnvironmentImpl::ErrorData)+strlen(msg)+1);
    data->requestId = requestId;
    data->message = (char*)data + sizeof(EnvironmentImpl::ErrorData);
    strcpy(data->message, msg);
    sendToWorker(perform_response_error, data);
#endif
    std::cerr << "ERROR: " << msg << std::endl;
}

void Environment::releaseNode(NodeId nodeId)
{
    m_impl->releaseNode(nodeId);
}

void EnvironmentImpl::process(Methcla_Time currentTime, size_t numFrames, const sample_t* const* inputs, sample_t* const* outputs)
{
    const Methcla_EngineLogFlags logFlags(m_logFlags);
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

void EnvironmentImpl::processRequests(Methcla_EngineLogFlags logFlags, Methcla_Time currentTime)
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
        catch (std::exception& e)
        {
            std::cerr << "Unhandled exception in `processRequests': " << e.what() << std::endl;
        }
    }
}

void EnvironmentImpl::processScheduler(Methcla_EngineLogFlags logFlags, Methcla_Time currentTime, Methcla_Time nextTime)
{
    while (!m_scheduler.isEmpty())
    {
        Methcla_Time scheduleTime = m_scheduler.time();
        if (scheduleTime <= nextTime)
        {
            ScheduledBundle bundle = m_scheduler.top();
            BOOST_ASSERT_MSG( methcla_time_from_uint64(bundle.m_bundle.time()) == scheduleTime
                            , "Scheduled bundle timestamp inconsistency" );
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

void EnvironmentImpl::processBundle(Methcla_EngineLogFlags logFlags, Request* request, const OSCPP::Server::Bundle& bundle, Methcla_Time scheduleTime, Methcla_Time currentTime)
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

static void throwError(const char* command, Methcla_Error code, std::function<void(std::stringstream&)> func)
{
    std::stringstream stream;
    stream << command << ": ";
    func(stream);
    throw Error(code, stream.str());
}

void EnvironmentImpl::processMessage(Methcla_EngineLogFlags logFlags, const OSCPP::Server::Message& msg, Methcla_Time scheduleTime, Methcla_Time currentTime)
{
    if (logFlags & kMethcla_EngineLogRequests)
        std::cerr << "Request: " << msg << std::endl;

    auto args = msg.args();
    // Methcla_RequestId requestId = args.int32();

    try {
        if (msg == "/group/new")
        {
            NodeId nodeId = NodeId(args.int32());
            NodeId targetId = NodeId(args.int32());
            args.drop(); // int32_t addAction = args.int32();

            auto targetNode = m_nodes.lookup(targetId);
            if (targetNode == nullptr)
                throwError("/group/new", kMethcla_NodeIdError, [&](std::stringstream& s) {
                    s << "Target node " << targetId << " not found";
                });

            auto targetGroup = targetNode->isGroup() ? dynamic_cast<Group*>(targetNode.get())
                                                     : dynamic_cast<Synth*>(targetNode.get())->parent();
            auto group = Group::construct(*m_owner, nodeId, targetGroup, Node::kAddToTail);
            m_nodes.insert(group->id(), group);
        }
        else if (msg == "/group/freeAll")
        {
            NodeId nodeId = NodeId(args.int32());

            Node* node = m_nodes.lookup(nodeId).get();
            if (node == nullptr)
                throwError("/group/freeAll", kMethcla_NodeIdError, [&](std::stringstream& s) {
                    s << "Group " << nodeId << " not found";
                });
            else if (!node->isGroup())
                throwError("/group/freeAll", kMethcla_NodeIdError, [&](std::stringstream& s) {
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
            args.drop(); // int32_t addAction = args.int32();

            const std::shared_ptr<SynthDef> def = m_owner->synthDef(defName);

            auto synthControls = args.atEnd() ? OSCPP::Server::ArgStream() : args.array();
            // FIXME: Cannot be checked before the synth is instantiated.
            // if (def->numControlInputs() != synthControls.size()) {
            //     throw std::runtime_error("Missing synth control initialisers");
            // }
            auto synthArgs = args.atEnd() ? OSCPP::Server::ArgStream() : args.array();

            Node* targetNode = m_nodes.lookup(targetId).get();
            if (targetNode == nullptr)
                throwError("/synth/new", kMethcla_NodeIdError, [&](std::stringstream& s) {
                    s << "Target node " << targetId << " not found";
                });

            Group* targetGroup = targetNode->isGroup() ? dynamic_cast<Group*>(targetNode)
                                                       : dynamic_cast<Synth*>(targetNode)->parent();

            try
            {
                ResourceRef<Synth> synth = Synth::construct(
                    *m_owner,
                    nodeId,
                    targetGroup,
                    Node::kAddToTail,
                    *def,
                    synthControls,
                    synthArgs);

                m_nodes.insert(synth->id(), synth);
            }
            catch (OSCPP::UnderrunError&)
            {
                throwError("/synth/new", kMethcla_ArgumentError, [&](std::stringstream& s) {
                    s << "Missing control initializer for synth " << nodeId;
                });
            }
            catch (OSCPP::ParseError&)
            {
                throwError("/synth/new", kMethcla_ArgumentError, [&](std::stringstream& s) {
                    s << "Invalid control initializer for synth " << nodeId;
                });
            }
        }
        else if (msg == "/synth/activate")
        {
            NodeId nodeId = NodeId(args.int32());
            Node* node = m_nodes.lookup(nodeId).get();
            if (node == nullptr)
                throwError("/synth/activate", kMethcla_NodeIdError, [&](std::stringstream& s) {
                    s << "Synth " << nodeId << " not found";
                });
            else if (!node->isSynth())
                throwError("/synth/activate", kMethcla_NodeIdError, [&](std::stringstream& s) {
                    s << "Node " << nodeId << " is not a synth";
                });
            Synth* synth = dynamic_cast<Synth*>(node);
            // TODO: Use sample rate estimate from driver
            const float sampleOffset = std::max(0., (scheduleTime - currentTime) * m_owner->sampleRate());
            synth->activate(sampleOffset);
        }
        else if (msg == "/node/free")
        {
            NodeId nodeId = NodeId(args.int32());

            // Check node id validity
            if (!m_nodes.contains(nodeId))
                throwError("/node/free", kMethcla_NodeIdError, [&](std::stringstream& s) {
                    s << "Node " << nodeId << " not found";
                });
            else if (nodeId == m_rootNode->id())
                throwError("/node/free", kMethcla_NodeIdError, [&](std::stringstream& s) {
                    s << "Cannot free root node " << nodeId;
                });

            // Remove node from parent (asynchronous commands might still refer to the node)
            m_nodes.lookup(nodeId)->unlink();

            // Drop node reference
            releaseNode(nodeId);
        }
        else if (msg == "/node/set")
        {
            NodeId nodeId = NodeId(args.int32());
            int32_t index = args.int32();
            float value = args.float32();

            Node* node = m_nodes.lookup(nodeId).get();
            if (node == nullptr)
                throwError("/node/set", kMethcla_NodeIdError, [&](std::stringstream& s) {
                    s << "Node " << nodeId << " not found";
                });
            else if (!node->isSynth())
                throwError("/node/set", kMethcla_NodeIdError, [&](std::stringstream& s) {
                    s << "Node " << nodeId << " is not a synth";
                });

            Synth* synth = dynamic_cast<Synth*>(node);

            if ((index < 0) || (index >= (int32_t)synth->numControlInputs()))
                throwError("/node/set", kMethcla_ArgumentError, [&](std::stringstream& s) {
                    s << "Control input index " << index << " out of range for synth " << nodeId;
                });

            synth->controlInput(index) = value;
        }
        else if (msg == "/synth/map/input")
        {
            NodeId nodeId = NodeId(args.int32());
            int32_t index = args.int32();
            int32_t busId = AudioBusId(args.int32());
            Methcla_BusMappingFlags flags = Methcla_BusMappingFlags(args.int32());

            if (busId < 0 || ((flags & kMethcla_BusMappingExternal) && (size_t)busId > m_externalAudioInputs.size())
                          || ((size_t)busId > m_internalAudioBuses.size()))
                throwError("/synth/map/input", kMethcla_ArgumentError, [&](std::stringstream& s) {
                    s << "Audio bus id " << busId << " out of range";
                });

            Node* node = m_nodes.lookup(nodeId).get();
            if (node == nullptr)
                throwError("/synth/map/input", kMethcla_NodeIdError, [&](std::stringstream& s) {
                    s << "Synth " << nodeId << " not found";
                });
            else if (!node->isSynth())
                throwError("/synth/map/input", kMethcla_NodeIdError, [&](std::stringstream& s) {
                    s << "Node " << nodeId << " is not a synth";
                });

            Synth* synth = dynamic_cast<Synth*>(node);

            if ((index < 0) || (index >= (int32_t)synth->numAudioInputs()))
                throwError("/synth/map/input", kMethcla_ArgumentError, [&](std::stringstream& s) {
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
                throwError("/synth/map/output", kMethcla_ArgumentError, [&](std::stringstream& s) {
                    s << "Audio bus id " << busId << " out of range";
                });

            Node* node = m_nodes.lookup(nodeId).get();
            if (node == nullptr)
                throwError("/synth/map/output", kMethcla_NodeIdError, [&](std::stringstream& s) {
                    s << "Synth " << nodeId << " not found";
                });
            else if (!node->isSynth())
                throwError("/synth/map/output", kMethcla_NodeIdError, [&](std::stringstream& s) {
                    s << "Node " << nodeId << " is not a synth";
                });

            Synth* synth = dynamic_cast<Synth*>(node);
            if ((index < 0) || (index >= (int32_t)synth->numAudioOutputs()))
                throwError("/synth/map/output", kMethcla_ArgumentError, [&](std::stringstream& s) {
                    s << "Audio output index " << index << " out of range for synth " << nodeId;
                });

            synth->mapOutput(index, AudioBusId(busId), flags);
        }
        else if (msg == "/query/external_inputs")
        {
    // Methcla_RequestId requestId = args.int32();
            // sendToWorker(Command(this, perform_response_query_external_inputs, requestId));
        }
        else if (msg == "/query/external_outputs")
        {
    // Methcla_RequestId requestId = args.int32();
            // sendToWorker(Command(this, perform_response_query_external_outputs, requestId));
        }
    } catch (Exception& e) {
        // TODO: Reply with error code
        m_owner->replyError(kMethcla_Notification, e.what());
    } catch (std::exception& e) {
        m_owner->replyError(kMethcla_Notification, e.what());
    }
}

void Environment::registerSynthDef(const Methcla_SynthDef* def)
{
    m_impl->registerSynthDef(def);
}

void EnvironmentImpl::registerSynthDef(const Methcla_SynthDef* def)
{
    auto synthDef = std::make_shared<SynthDef>(def);
    m_synthDefs[synthDef->uri()] = synthDef;
}

const std::shared_ptr<SynthDef>& Environment::synthDef(const char* uri) const
{
    return m_impl->synthDef(uri);
}

const std::shared_ptr<SynthDef>& EnvironmentImpl::synthDef(const char* uri) const
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

static void perform_worldCommand(Environment* env, CommandData* data)
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

static void perform_hostCommand(Environment* env, CommandData* data)
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

#if defined(METHCLA_USE_DUMMY_DRIVER)
# include "Methcla/Audio/IO/DummyDriver.hpp"
#endif

Engine::Engine(PacketHandler handler, const Environment::Options& engineOptions, const IO::Driver::Options& driverOptions)
{
#if defined(METHCLA_USE_DUMMY_DRIVER)
    m_driver = new IO::DummyDriver(driverOptions);
#else
    m_driver = IO::defaultPlatformDriver(driverOptions);
#endif
    m_driver->setProcessCallback(processCallback, this);

    std::cout << "Starting methcla engine (version " << methcla_version() << ")" << std::endl
              << "  sampleRate = " << m_driver->sampleRate() << std::endl
              << "  numInputs = "  << m_driver->numInputs() << std::endl
              << "  numOutputs = " << m_driver->numOutputs() << std::endl
              << "  bufferSize = " << m_driver->bufferSize() << std::endl;

    Environment::Options options(engineOptions);
    options.sampleRate = m_driver->sampleRate();
    options.blockSize = m_driver->bufferSize();
    options.numHardwareInputChannels = m_driver->numInputs();
    options.numHardwareOutputChannels = m_driver->numOutputs();
    m_env = new Environment(handler, options);
}

Engine::~Engine()
{
    stop();
    delete m_env;
    delete m_driver;
}

void Engine::start()
{
    driver()->start();
}

void Engine::stop()
{
    driver()->stop();
}

void Engine::processCallback(
    void* data,
    Methcla_Time currentTime,
    size_t numFrames,
    const sample_t* const* inputs,
    sample_t* const* outputs)
{
    static_cast<Engine*>(data)->m_env->process(currentTime, numFrames, inputs, outputs);
}
