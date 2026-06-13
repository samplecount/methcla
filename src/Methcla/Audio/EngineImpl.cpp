// Copyright (C) 2012-2014 Samplecount S.L.
// Copyright (C) 2026 Methcla contributors
//
// SPDX-License-Identifier: Apache-2.0

#include "Methcla/Audio/Engine.hpp"
#include "Methcla/Audio/EngineImpl.hpp"
#include "Methcla/Audio/Group.hpp"
#include "Methcla/Audio/Synth.hpp"
#include "Methcla/Exception.hpp"
#include "Methcla/Memory.hpp"
#include "Methcla/Memory/Manager.hpp"
#include "Methcla/Platform.hpp"
#include "Methcla/Utility/MessageQueue.hpp"

#include <methcla/log.hpp>

#include <boost/heap/priority_queue.hpp>

#include <cassert>
#include <functional>
#include <stdexcept>

#include <oscpp/print.hpp>
#include <oscpp/util.hpp>

using namespace Methcla;
using namespace Methcla::Audio;
using Methcla::Memory::RTMemoryManager;

static void throwError(Methcla_ErrorCode code, const std::string& msg)
{
    throw Error(code, msg);
}

static void throwErrorWith(Methcla_ErrorCode                       code,
                           std::function<void(std::stringstream&)> func)
{
    std::stringstream stream;
    func(stream);
    throwError(code, stream.str());
}

template <class T> const char* nodeTypeName()
{
    return "node";
}

template <> const char* nodeTypeName<Group>()
{
    return "group";
}

template <> const char* nodeTypeName<Synth>()
{
    return "synth";
}

static inline bool isValidNodeId(const std::vector<Node*>& nodes, NodeId nodeId)
{
    return nodeId.id() >= 0 && static_cast<size_t>(nodeId.id()) < nodes.size();
}

static inline void checkNodeIdIsValid(const std::vector<Node*>& nodes,
                                      NodeId                    nodeId)
{
    if (!isValidNodeId(nodes, nodeId))
    {
        throwErrorWith(kMethcla_NodeIdError, [&](std::stringstream& s) {
            s << "Node id " << nodeId << " out of range";
        });
    }
}

static inline void checkNodeIdIsFree(const std::vector<Node*>& nodes,
                                     NodeId                    nodeId)
{
    checkNodeIdIsValid(nodes, nodeId);

    if (nodes[nodeId.id()] != nullptr)
    {
        throwErrorWith(kMethcla_NodeIdError, [&](std::stringstream& s) {
            s << "Node id " << nodeId << " already in use";
        });
    }
}

static inline void addNode(std::vector<Node*>& nodes, Node* node)
{
    NodeId nodeId(node->id());
    checkNodeIdIsFree(nodes, nodeId);
    nodes[nodeId.id()] = node;
}

static inline Node* lookupNode(std::vector<Node*>& nodes, const char* prefix,
                               NodeId nodeId)
{
    checkNodeIdIsValid(nodes, nodeId);

    Node* node = nodes[nodeId.id()];

    if (node == nullptr)
    {
        throwErrorWith(kMethcla_NodeIdError, [&](std::stringstream& s) {
            s << prefix << " " << nodeId << " not found";
        });
    }

    return node;
}

template <class T>
T* lookupNodeAs(std::vector<Node*>& nodes, const char* prefix, NodeId nodeId)
{
    Node* node = lookupNode(nodes, prefix, nodeId);

    T* result = dynamic_cast<T*>(node);

    if (result == nullptr)
    {
        throwErrorWith(kMethcla_NodeIdError, [&](std::stringstream& s) {
            s << nodeId << " is not a " << nodeTypeName<T>();
        });
    }

    return result;
}

static inline void addNodeToTarget(Node* target, Node* node,
                                   Methcla_NodePlacement nodePlacement)
{
    switch (nodePlacement)
    {
        case kMethcla_NodePlacementHeadOfGroup: {
            Group* group = dynamic_cast<Group*>(target);
            if (group != nullptr)
            {
                group->addToHead(node);
            }
            else
            {
                throwErrorWith(kMethcla_NodeIdError, [&](std::stringstream& s) {
                    s << "Target node " << target->id() << " is not a group";
                });
            }
        }
        break;
        case kMethcla_NodePlacementTailOfGroup: {
            Group* group = dynamic_cast<Group*>(target);
            if (group != nullptr)
            {
                group->addToTail(node);
            }
            else
            {
                throwErrorWith(kMethcla_NodeIdError, [&](std::stringstream& s) {
                    s << "Target node " << target->id() << " is not a group";
                });
            }
        }
        break;
        case kMethcla_NodePlacementBeforeNode:
            if (target->parent() == nullptr)
            {
                throwError(kMethcla_NodeIdError,
                           "Cannot place node before root node");
            }
            target->parent()->addBefore(target, node);
            break;
        case kMethcla_NodePlacementAfterNode:
            if (target->parent() == nullptr)
            {
                throwError(kMethcla_NodeIdError,
                           "Cannot place node after root node");
            }
            target->parent()->addAfter(target, node);
            break;
        default:
            throwError(kMethcla_ArgumentError,
                       "Invalid node placement specification");
    }

    // POST: Node should be linked into node tree.
    assert(node->parent() != nullptr);
}

void Methcla::Audio::perform_nrt_free(Environment*, void* data)
{
    Methcla::Memory::free(data);
}

void Methcla::Audio::perform_rt_free(Environment* env, void* data)
{
    env->rtMem().free(data);
}

EnvironmentImpl::EnvironmentImpl(Environment* owner, LogHandler logHandler,
                                 PacketHandler               listener,
                                 const Environment::Options& options,
                                 Environment::MessageQueue*  messageQueue,
                                 Environment::Worker*        worker)
: m_owner(owner)
, m_logHandler(logHandler)
, m_packetHandler(listener)
, m_plugins(Methcla::Plugin::defaultLoader())
, m_rtMem(options.realtimeMemorySize)
, m_requests(messageQueue == nullptr
                 ? new Utility::MessageQueue<Request*>(kQueueSize)
                 : messageQueue)
, m_worker(worker ? worker
                  : new Utility::WorkerThread<Environment::Command>(
                        kQueueSize, kNumWorkerThreads))
, m_scheduler(options.mode == Environment::kRealtimeMode ? kQueueSize : 0)
, m_epoch(0)
, m_currentTime(0)
, m_nodes(options.maxNumNodes, nullptr)
, m_resources(options.maxNumResources)
, m_logLevel(options.logLevel)
, m_logFlags(kMethcla_EngineLogDefault)
{
    assert(m_logLevel.is_lock_free());
    assert(m_logFlags.is_lock_free());

    const Epoch prevEpoch = m_epoch - 1;

    m_externalAudioInputs.reserve(options.numHardwareInputChannels);
    for (size_t i = 0; i < options.numHardwareInputChannels; i++)
    {
        m_externalAudioInputs.push_back(
            std::make_shared<ExternalAudioBus>(prevEpoch));
    }

    m_externalAudioOutputs.reserve(options.numHardwareOutputChannels);
    for (size_t i = 0; i < options.numHardwareOutputChannels; i++)
    {
        m_externalAudioOutputs.push_back(
            std::make_shared<ExternalAudioBus>(prevEpoch));
    }

    for (size_t i = 0; i < options.maxNumAudioBuses; i++)
    {
        m_internalAudioBuses.push_back(
            std::make_shared<InternalAudioBus>(options.blockSize, prevEpoch));
    }
}

EnvironmentImpl::~EnvironmentImpl()
{
    m_rootNode->free();
    // Stop worker thread(s). Note that relying on the destructor here doesn't
    // cut it, because asynchronous commands in the worker thread queue might
    // reference a partially destroyed Environment.
    m_worker->stop();
}

void EnvironmentImpl::init(const Environment::Options& options)
{
    // Create root group
    m_rootNode = Group::construct(*m_owner, NodeId(0));
    addNode(m_nodes, m_rootNode);
    // Load plugins
    Methcla_Host host(*m_owner);
    for (const auto& dir : options.pluginDirectories)
    {
        m_plugins.loadPlugins(&host, dir);
    }
    m_plugins.loadPlugins(&host, options.pluginLibraries);
}

void EnvironmentImpl::process(Methcla_Time currentTime, size_t numFrames,
                              const sample_t* const* inputs,
                              sample_t* const*       outputs)
{
    // Update current time
    m_currentTime = currentTime;

    // Load log flags
    const Methcla_EngineLogFlags logFlags =
        (Methcla_EngineLogFlags)m_logFlags.load();

    // Process external requests
    processRequests(logFlags, currentTime);
    // Process scheduled requests
    processScheduler(logFlags, currentTime,
                     currentTime + numFrames / m_owner->sampleRate());
    // std::cout << "Environment::process " << currentTime << std::endl;

    // Process non-realtime commands
    m_worker->perform();

    const size_t numExternalInputs = m_externalAudioInputs.size();
    const size_t numExternalOutputs = m_externalAudioOutputs.size();

    // Connect input and output buses
    for (size_t i = 0; i < numExternalInputs; i++)
    {
        m_externalAudioInputs[i]->setData(const_cast<sample_t*>(inputs[i]));
        m_externalAudioInputs[i]->setEpoch(m_epoch);
    }

    for (size_t i = 0; i < numExternalOutputs; i++)
    {
        m_externalAudioOutputs[i]->setData(outputs[i]);
    }

    // Run DSP graph
    m_rootNode->process(numFrames);

    // Zero outputs that haven't been written to
    for (size_t i = 0; i < numExternalOutputs; i++)
    {
        if (m_externalAudioOutputs[i]->epoch() != m_epoch)
        {
            memset(outputs[i], 0, numFrames * sizeof(sample_t));
        }
    }

    m_epoch++;
}

void EnvironmentImpl::processRequests(Methcla_EngineLogFlags logFlags,
                                      const Methcla_Time     currentTime)
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
                Methcla_Time          bundleTime =
                    methcla_time_from_uint64(bundle.time());
                if (bundleTime == 0.)
                {
                    processBundle(logFlags, request, bundle, currentTime,
                                  currentTime);
                }
                else
                {
                    request->retain();
                    m_scheduler.push(bundleTime,
                                     ScheduledBundle(request, bundle));
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

void EnvironmentImpl::processScheduler(Methcla_EngineLogFlags logFlags,
                                       const Methcla_Time     currentTime,
                                       const Methcla_Time     nextTime)
{
    while (!m_scheduler.isEmpty())
    {
        Methcla_Time scheduleTime = m_scheduler.time();
        if (scheduleTime < nextTime)
        {
#if DEBUG
            if (scheduleTime < currentTime)
                rt_log() << "Late " << scheduleTime << " " << currentTime << " "
                         << nextTime;
#endif // DEBUG
            ScheduledBundle bundle = m_scheduler.top();
            assert(methcla_time_from_uint64(bundle.m_bundle.time()) ==
                   scheduleTime);
            processBundle(logFlags, bundle.m_request, bundle.m_bundle,
                          scheduleTime, currentTime);
            m_scheduler.pop();
            bundle.m_request->release();
        }
        else
        {
            break;
        }
    }
}

void EnvironmentImpl::processBundle(Methcla_EngineLogFlags       logFlags,
                                    Request*                     request,
                                    const OSCPP::Server::Bundle& bundle,
                                    const Methcla_Time           scheduleTime,
                                    const Methcla_Time           currentTime)
{
    auto packets = bundle.packets();
    while (!packets.atEnd())
    {
        auto packet = packets.next();
        if (packet.isBundle())
        {
            OSCPP::Server::Bundle innerBundle(packet);
            Methcla_Time          innerBundleTime =
                methcla_time_from_uint64(innerBundle.time());
            if (innerBundleTime <= scheduleTime)
            {
                processBundle(logFlags, request, innerBundle, scheduleTime,
                              currentTime);
            }
            else
            {
                request->retain();
                m_scheduler.push(innerBundleTime,
                                 ScheduledBundle(request, innerBundle));
            }
        }
        else
        {
            processMessage(logFlags, packet, scheduleTime, currentTime);
        }
    }
}

namespace {

    class ResourceErrorNotification : public EnvironmentImpl::Notification
    {
        int32_t       m_resourceId;
        Methcla_Error m_error;

    public:
        ResourceErrorNotification(int32_t resourceId, Methcla_Error error)
        : m_resourceId(resourceId)
        , m_error(error)
        {}

    private:
        void notify(Environment* env) override
        {
            constexpr const char* address = "/resource/error";
            const char*           msg = methcla_error_message(m_error);
            if (!msg)
                msg =
                    methcla_error_code_description(methcla_error_code(m_error));
            OSCPP::Client::DynamicPacket packet(
                OSCPP::Size::message(address, 3) + OSCPP::Size::int32(2) +
                OSCPP::Size::string(msg));
            packet.openMessage(address, 3);
            packet.int32(m_resourceId);
            packet.int32(static_cast<int32_t>(methcla_error_code(m_error)));
            packet.string(msg);
            packet.closeMessage();
            env->notify(packet);
            methcla_error_free(m_error);
        }
    };

    class ResourceConstructCommand
    {
        EnvironmentImpl*           m_impl;
        int32_t                    m_resourceId;
        const Methcla_ResourceDef* m_def;
        void*                      m_options;
        void*                      m_result;
        Methcla_Error              m_error;

        static void completeOnRT(Environment* env, void* data)
        {
            auto* self = static_cast<ResourceConstructCommand*>(data);

            if (methcla_is_error(self->m_error))
            {
                self->m_impl->m_resources[self->m_resourceId] =
                    EnvironmentImpl::ResourceEntry{};
                self->m_impl->sendToWorker<ResourceErrorNotification>(
                    self->m_resourceId, self->m_error);
            }
            else
            {
                auto& entry = self->m_impl->m_resources[self->m_resourceId];
                entry.def = self->m_def;
                entry.data = self->m_result;

                if (entry.freePending)
                {
                    entry.state =
                        EnvironmentImpl::ResourceEntry::State::Destroying;
                    self->m_impl->scheduleResourceDestroy(self->m_resourceId);
                }
                else
                {
                    entry.state = EnvironmentImpl::ResourceEntry::State::Live;
                    self->m_impl->notifyResourceReady(self->m_resourceId);
                }
            }
            if (self->m_options)
                env->rtMem().free(self->m_options);
            env->rtMem().free(self);
        }

    public:
        ResourceConstructCommand(EnvironmentImpl* impl, int32_t resourceId,
                                 const Methcla_ResourceDef* def, void* options)
        : m_impl(impl)
        , m_resourceId(resourceId)
        , m_def(def)
        , m_options(options)
        , m_result(nullptr)
        , m_error(methcla_no_error())
        {}

        void perform(Environment* env)
        {
            Methcla_Host host(*env);
            m_result = Memory::alloc(m_def->instance_size);
            if (m_def->construct)
                m_error = m_def->construct(&host, m_def, m_options, m_result);
            if (methcla_is_error(m_error))
            {
                Memory::free(m_result);
                m_result = nullptr;
            }
            env->sendFromWorker(completeOnRT, this);
        }
    };

    // Plays two roles in sequence to avoid an extra RT memory allocation:
    // first dispatched as ResourceDestroyCommand (NRT destruction work), then
    // re-queued as Notification (sends /resource/destroyed). perform() hides
    // Notification::perform() intentionally; completeOnRT re-queues via
    // static_cast<Notification*> to select the second role.
    class ResourceDestroyCommand : public EnvironmentImpl::Notification
    {
        EnvironmentImpl*           m_impl;
        int32_t                    m_resourceId;
        const Methcla_ResourceDef* m_def;
        void*                      m_data;

        static void completeOnRT(Environment*, void* data)
        {
            auto* self = static_cast<ResourceDestroyCommand*>(data);
            self->m_impl->m_resources[self->m_resourceId] =
                EnvironmentImpl::ResourceEntry{};
            self->m_impl->sendToWorker(
                static_cast<EnvironmentImpl::Notification*>(self));
        }

        void notify(Environment* env) override
        {
            static const char*           address = "/resource/destroyed";
            OSCPP::Client::DynamicPacket packet(
                OSCPP::Size::message(address, 1) + OSCPP::Size::int32(1));
            packet.openMessage(address, 1);
            packet.int32(m_resourceId);
            packet.closeMessage();
            env->notify(packet);
        }

    public:
        ResourceDestroyCommand(EnvironmentImpl* impl, int32_t resourceId,
                               const Methcla_ResourceDef* def, void* data)
        : m_impl(impl)
        , m_resourceId(resourceId)
        , m_def(def)
        , m_data(data)
        {}

        void perform(Environment* env)
        {
            Methcla_Host host(*env);
            if (m_def->destroy)
                m_def->destroy(&host, m_data);
            Memory::free(m_data);
            env->sendFromWorker(completeOnRT, this);
        }
    };

    class NodeTreeStatisticsCommand
    {
    public:
        struct Statistics
        {
            Statistics()
            : numGroups(0)
            , numSynths(0)
            {}
            size_t numGroups;
            size_t numSynths;
        };

        static Statistics collectStatistics(const Group* group,
                                            Statistics   stats = Statistics())
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

        NodeTreeStatisticsCommand(Methcla_RequestId requestId, Statistics stats)
        : m_requestId(requestId)
        , m_stats(stats)
        {}

        void perform(Environment* env)
        {
            static const char*           address = "/node/tree/statistics";
            OSCPP::Client::DynamicPacket packet(
                OSCPP::Size::message(address, 2) + OSCPP::Size::int32(2));
            packet.openMessage(address, 2);
            packet.int32(static_cast<int32_t>(m_stats.numGroups));
            packet.int32(static_cast<int32_t>(m_stats.numSynths));
            packet.closeMessage();
            env->reply(m_requestId, packet);
            env->sendFromWorker(perform_rt_free, this);
        }

    private:
        Methcla_RequestId m_requestId;
        Statistics        m_stats;
    };

    class RTMemoryStatisticsCommand
    {
    public:
        RTMemoryStatisticsCommand(Methcla_RequestId                  requestId,
                                  const RTMemoryManager::Statistics& stats)
        : m_requestId(requestId)
        , m_stats(stats)
        {}

        void perform(Environment* env)
        {
            static const char* address = "/engine/realtime-memory/statistics";
            OSCPP::Client::DynamicPacket packet(
                OSCPP::Size::message(address, 2) + OSCPP::Size::int32(2));
            packet.openMessage(address, 2);
            packet.int32(static_cast<int32_t>(m_stats.freeNumBytes));
            packet.int32(static_cast<int32_t>(m_stats.usedNumBytes));
            packet.closeMessage();
            env->reply(m_requestId, packet);
            env->sendFromWorker(perform_rt_free, this);
        }

    private:
        Methcla_RequestId           m_requestId;
        RTMemoryManager::Statistics m_stats;
    };

    class ResourceReadyNotification : public EnvironmentImpl::Notification
    {
        int32_t m_resourceId;

    public:
        explicit ResourceReadyNotification(int32_t resourceId)
        : m_resourceId(resourceId)
        {}

    private:
        void notify(Environment* env) override
        {
            static const char*           address = "/resource/ready";
            OSCPP::Client::DynamicPacket packet(
                OSCPP::Size::message(address, 1) + OSCPP::Size::int32(1));
            packet.openMessage(address, 1);
            packet.int32(m_resourceId);
            packet.closeMessage();
            env->notify(packet);
        }
    };

} // namespace

void EnvironmentImpl::processMessage(Methcla_EngineLogFlags        logFlags,
                                     const OSCPP::Server::Message& msg,
                                     Methcla_Time                  scheduleTime,
                                     Methcla_Time                  currentTime)
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
            checkNodeIdIsFree(m_nodes, nodeId);

            NodeId                targetId = NodeId(args.int32());
            Methcla_NodePlacement nodePlacement =
                Methcla_NodePlacement(args.int32());

            Node* target = lookupNode(m_nodes, "Target node", targetId);

            Group* group = Group::construct(*m_owner, nodeId);
            addNode(m_nodes, group);
            addNodeToTarget(target, group, nodePlacement);
        }
        else if (msg == "/group/freeAll")
        {
            NodeId nodeId = NodeId(args.int32());
            Group* group = lookupNodeAs<Group>(m_nodes, "Group", nodeId);
            group->freeAll();
        }
        else if (msg == "/synth/new")
        {
            const char* defName = args.string();

            NodeId nodeId = NodeId(args.int32());
            checkNodeIdIsFree(m_nodes, nodeId);

            NodeId                targetId = NodeId(args.int32());
            Methcla_NodePlacement nodePlacement =
                Methcla_NodePlacement(args.int32());

            const std::shared_ptr<SynthDef> def = m_owner->synthDef(defName);

            auto synthControls =
                args.atEnd() ? OSCPP::Server::ArgStream() : args.array();
            // FIXME: Cannot be checked before the synth is instantiated.
            // if (def->numControlInputs() != synthControls.size()) {
            //     throw std::runtime_error("Missing synth control
            //     initialisers");
            // }
            auto synthArgs =
                args.atEnd() ? OSCPP::Server::ArgStream() : args.array();

            Node* target = lookupNode(m_nodes, "Target node", targetId);

            try
            {
                Synth* synth = Synth::construct(*m_owner, nodeId, *def,
                                                synthControls, synthArgs);

                addNode(m_nodes, synth);
                addNodeToTarget(target, synth, nodePlacement);
            }
            catch (OSCPP::UnderrunError&)
            {
                throwErrorWith(
                    kMethcla_ArgumentError, [&](std::stringstream& s) {
                        s << "Missing control initializer for synth " << nodeId;
                    });
            }
            catch (OSCPP::ParseError&)
            {
                throwErrorWith(
                    kMethcla_ArgumentError, [&](std::stringstream& s) {
                        s << "Invalid control initializer for synth " << nodeId;
                    });
            }
        }
        else if (msg == "/synth/activate")
        {
            NodeId nodeId = NodeId(args.int32());
            Synth* synth = lookupNodeAs<Synth>(m_nodes, "Synth", nodeId);
            // TODO: Use sample rate estimate from driver
            const double sampleOffset = std::max(
                0., (scheduleTime - currentTime) * m_owner->sampleRate());
            synth->activate(sampleOffset);
        }
        else if (msg == "/synth/map/input")
        {
            NodeId                  nodeId = NodeId(args.int32());
            int32_t                 index = args.int32();
            int32_t                 busId = args.int32();
            Methcla_BusMappingFlags flags =
                Methcla_BusMappingFlags(args.int32());

            if ((flags & kMethcla_BusMappingExternal) &&
                (busId < 0 || (size_t)busId >= m_externalAudioInputs.size()))
            {
                throwErrorWith(kMethcla_ArgumentError,
                               [&](std::stringstream& s) {
                                   s << "External audio bus id " << busId
                                     << " out of range";
                               });
            }
            else if ((flags & kMethcla_BusMappingInternal) &&
                     (busId < 0 ||
                      (size_t)busId >= m_internalAudioBuses.size()))
            {
                throwErrorWith(kMethcla_ArgumentError,
                               [&](std::stringstream& s) {
                                   s << "Internal audio bus id " << busId
                                     << " out of range";
                               });
            }

            Synth* synth = lookupNodeAs<Synth>(m_nodes, "Synth", nodeId);

            if ((index < 0) || (index >= (int32_t)synth->numAudioInputs()))
            {
                throwErrorWith(kMethcla_ArgumentError,
                               [&](std::stringstream& s) {
                                   s << "Audio input index " << index
                                     << " out of range for synth " << nodeId;
                               });
            }

            synth->mapInput(static_cast<Methcla_PortCount>(index),
                            AudioBusId(busId), flags);
        }
        else if (msg == "/synth/map/output")
        {
            NodeId                  nodeId = NodeId(args.int32());
            int32_t                 index = args.int32();
            int32_t                 busId = args.int32();
            Methcla_BusMappingFlags flags =
                Methcla_BusMappingFlags(args.int32());

            if ((flags & kMethcla_BusMappingExternal) &&
                (busId < 0 || (size_t)busId >= m_externalAudioOutputs.size()))
            {
                throwErrorWith(kMethcla_ArgumentError,
                               [&](std::stringstream& s) {
                                   s << "External audio bus id " << busId
                                     << " out of range";
                               });
            }
            else if ((flags & kMethcla_BusMappingInternal) &&
                     (busId < 0 ||
                      (size_t)busId >= m_internalAudioBuses.size()))
            {
                throwErrorWith(kMethcla_ArgumentError,
                               [&](std::stringstream& s) {
                                   s << "Internal audio bus id " << busId
                                     << " out of range";
                               });
            }

            Synth* synth = lookupNodeAs<Synth>(m_nodes, "Synth", nodeId);

            if ((index < 0) || (index >= (int32_t)synth->numAudioOutputs()))
            {
                throwErrorWith(kMethcla_ArgumentError,
                               [&](std::stringstream& s) {
                                   s << "Audio output index " << index
                                     << " out of range for synth " << nodeId;
                               });
            }

            synth->mapOutput(static_cast<Methcla_PortCount>(index),
                             AudioBusId(busId), flags);
        }
        else if (msg == "/synth/property/doneFlags/set")
        {
            NodeId                nodeId = NodeId(args.int32());
            Methcla_NodeDoneFlags flags = Methcla_NodeDoneFlags(args.int32());
            Synth* synth = lookupNodeAs<Synth>(m_nodes, "Synth", nodeId);
            synth->setDoneFlags(flags);
        }
        else if (msg == "/node/free")
        {
            NodeId nodeId = NodeId(args.int32());
            Node*  node = lookupNode(m_nodes, "Node", nodeId);

            if (node == m_rootNode)
            {
                throwErrorWith(kMethcla_NodeIdError, [&](std::stringstream& s) {
                    s << "Cannot free root node " << nodeId;
                });
            }

            node->free();
        }
        else if (msg == "/node/set")
        {
            NodeId  nodeId = NodeId(args.int32());
            int32_t index = args.int32();
            float   value = args.float32();

            Synth* synth = lookupNodeAs<Synth>(m_nodes, "Synth", nodeId);

            if ((index < 0) || (index >= (int32_t)synth->numControlInputs()))
            {
                throwErrorWith(kMethcla_ArgumentError,
                               [&](std::stringstream& s) {
                                   s << "Control input index " << index
                                     << " out of range for synth " << nodeId;
                               });
            }

            synth->controlInput(static_cast<Methcla_PortCount>(index)) = value;
        }
        else if (msg == "/node/tree/statistics")
        {
            Methcla_RequestId requestId = args.int32();
            sendToWorker<NodeTreeStatisticsCommand>(
                requestId,
                NodeTreeStatisticsCommand::collectStatistics(rootNode()));
        }
        else if (msg == "/resource/new")
        {
            /* args: resourceId:i  uri:s  [options...] */
            const int32_t resourceId = args.int32();
            const char*   uri = args.string();

            auto it = m_resourceDefs.find(uri);
            if (it == m_resourceDefs.end())
            {
                throwErrorWith(kMethcla_UnsupportedResourceTypeError,
                               [&](std::stringstream& s) {
                                   s << "Unknown resource type: " << uri;
                               });
            }
            else
            {
                const Methcla_ResourceDef* def = it->second;

                void* options = nullptr;
                if (def->options_size > 0)
                    options = rtMem().alloc(def->options_size);

                if (def->configure)
                {
                    auto              state = args.state();
                    Methcla_ErrorCode code = def->configure(
                        std::get<0>(state).pos(),
                        std::get<0>(state).consumable(),
                        std::get<1>(state).pos(),
                        std::get<1>(state).consumable(), options);
                    if (code != kMethcla_NoError)
                    {
                        if (options)
                            rtMem().free(options);
                        throwErrorWith(code, [&](std::stringstream& s) {
                            s << "configure failed for " << uri;
                        });
                    }
                }

                auto& entry = m_resources[resourceId] = ResourceEntry{};
                entry.state = ResourceEntry::State::Constructing;
                sendToWorker<ResourceConstructCommand>(this, resourceId, def,
                                                       options);
            }
        }
        else if (msg == "/resource/free")
        {
            const int32_t resourceId = args.int32();
            if (resourceId < 0 ||
                static_cast<size_t>(resourceId) >= m_resources.size())
            {
                throwErrorWith(
                    kMethcla_ArgumentError, [&](std::stringstream& s) {
                        s << "Resource id " << resourceId << " out of range";
                    });
            }
            else
            {
                auto& entry = m_resources[resourceId];
                if (entry.state == ResourceEntry::State::Constructing)
                {
                    entry.freePending = true;
                }
                else if (entry.state == ResourceEntry::State::Live)
                {
                    if (entry.refCount == 0)
                    {
                        entry.state = ResourceEntry::State::Destroying;
                        scheduleResourceDestroy(resourceId);
                    }
                    else
                    {
                        entry.freePending = true;
                    }
                }
            }
        }
        else if (msg == "/engine/realtime-memory/statistics")
        {
            const Methcla_RequestId requestId = args.int32();
            sendToWorker<RTMemoryStatisticsCommand>(requestId,
                                                    rtMem().statistics());
        }
    }
    catch (std::exception& e)
    {
        std::stringstream s;
        s << msg.address() << ": " << e.what();
        replyError(kMethcla_Notification, s.str().c_str());
    }
}

void EnvironmentImpl::registerSynthDef(const Methcla_SynthDef* def)
{
    auto synthDef = std::make_shared<SynthDef>(def);
    m_synthDefs[synthDef->uri()] = synthDef;
}

void EnvironmentImpl::registerResourceDef(const Methcla_ResourceDef* def)
{
    m_resourceDefs[def->uri] = def;
}

void EnvironmentImpl::notifyResourceReady(int32_t resourceId)
{
    sendToWorker<ResourceReadyNotification>(resourceId);
}

void EnvironmentImpl::scheduleResourceDestroy(int32_t resourceId)
{
    auto& entry = m_resources[resourceId];
    sendToWorker<ResourceDestroyCommand>(this, resourceId, entry.def,
                                         entry.data);
}

const std::shared_ptr<SynthDef>&
EnvironmentImpl::synthDef(const char* uri) const
{
    auto it = m_synthDefs.find(uri);
    if (it == m_synthDefs.end())
    {
        throwErrorWith(kMethcla_SynthDefNotFoundError,
                       [&](std::stringstream& s) {
                           s << "Synth definition " << uri << " not found";
                       });
    }
    return it->second;
}
