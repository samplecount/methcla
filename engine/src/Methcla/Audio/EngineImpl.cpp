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

#include "Methcla/Audio/EngineImpl.hpp"
#include "Methcla/Audio/Engine.hpp"
#include "Methcla/Audio/Group.hpp"
#include "Methcla/Audio/Synth.hpp"
#include "Methcla/Exception.hpp"
#include "Methcla/Memory.hpp"
#include "Methcla/Memory/Manager.hpp"
#include "Methcla/Platform.hpp"
#include "Methcla/Utility/Macros.h"
#include "Methcla/Utility/MessageQueue.hpp"

#include <methcla/log.hpp>

METHCLA_WITHOUT_WARNINGS_BEGIN
# include <boost/heap/priority_queue.hpp>
METHCLA_WITHOUT_WARNINGS_END

#include <oscpp/print.hpp>
#include <oscpp/util.hpp>

using namespace Methcla;
using namespace Methcla::Audio;
using namespace Methcla::Memory;

static void throwError(Methcla_ErrorCode code, const std::string& msg)
{
    throw Error(code, msg);
}

static void throwErrorWith(Methcla_ErrorCode code, std::function<void(std::stringstream&)> func)
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
    return nodeId >= 0 && (size_t)nodeId < nodes.size();
}

static inline bool isUsedNodeId(const std::vector<Node*>& nodes, NodeId nodeId)
{
    return isValidNodeId(nodes, nodeId) && nodes[nodeId] != nullptr;
}

static inline void checkNodeIdIsValid(const std::vector<Node*>& nodes, NodeId nodeId)
{
    if (!isValidNodeId(nodes, nodeId))
    {
        throwErrorWith(kMethcla_NodeIdError, [&](std::stringstream& s) {
            s << "Node id " << nodeId << " out of range";
        });
    }
}

static inline void checkNodeIdIsFree(const std::vector<Node*>& nodes, NodeId nodeId)
{
    checkNodeIdIsValid(nodes, nodeId);

    if (nodes[nodeId] != nullptr)
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
    nodes[nodeId] = node;
}

static inline Node* lookupNode(std::vector<Node*>& nodes, const char* prefix, NodeId nodeId)
{
    checkNodeIdIsValid(nodes, nodeId);

    Node* node = nodes[nodeId];

    if (node == nullptr)
    {
        throwErrorWith(kMethcla_NodeIdError, [&](std::stringstream& s) {
            s << prefix << " " << nodeId << " not found";
        });
    }

    return node;
}

template <class T> T* lookupNodeAs(std::vector<Node*>& nodes, const char* prefix, NodeId nodeId)
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

static inline void addNodeToTarget(Node* target, Node* node, Methcla_NodePlacement nodePlacement)
{
    switch (nodePlacement)
    {
        case kMethcla_NodePlacementHeadOfGroup:
            {
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
        case kMethcla_NodePlacementTailOfGroup:
            {
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
                throwError(kMethcla_NodeIdError, "Cannot place node before root node");
            }
            target->parent()->addBefore(target, node);
            break;
        case kMethcla_NodePlacementAfterNode:
            if (target->parent() == nullptr)
            {
                throwError(kMethcla_NodeIdError, "Cannot place node after root node");
            }
            target->parent()->addAfter(target, node);
            break;
        default:
            throwError(kMethcla_ArgumentError, "Invalid node placement specification");
    }
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
    , m_nodes(options.maxNumNodes, nullptr)
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
    m_plugins.loadPlugins(*m_owner, options.pluginLibraries);
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
            checkNodeIdIsFree(m_nodes, nodeId);

            NodeId targetId = NodeId(args.int32());
            Methcla_NodePlacement nodePlacement = Methcla_NodePlacement(args.int32());

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

            NodeId targetId = NodeId(args.int32());
            Methcla_NodePlacement nodePlacement = Methcla_NodePlacement(args.int32());

            const shared_ptr<SynthDef> def = m_owner->synthDef(defName);

            auto synthControls = args.atEnd() ? OSCPP::Server::ArgStream() : args.array();
            // FIXME: Cannot be checked before the synth is instantiated.
            // if (def->numControlInputs() != synthControls.size()) {
            //     throw std::runtime_error("Missing synth control initialisers");
            // }
            auto synthArgs = args.atEnd() ? OSCPP::Server::ArgStream() : args.array();

            Node* target = lookupNode(m_nodes, "Target node", targetId);

            try
            {
                Synth* synth = Synth::construct(
                    *m_owner,
                    nodeId,
                    *def,
                    synthControls,
                    synthArgs);

                addNode(m_nodes, synth);
                addNodeToTarget(target, synth, nodePlacement);
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
            Synth* synth = lookupNodeAs<Synth>(m_nodes, "Synth", nodeId);
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

            if ((flags & kMethcla_BusMappingExternal) && (busId < 0 || (size_t)busId >= m_externalAudioOutputs.size()))
            {
                throwErrorWith(kMethcla_ArgumentError, [&](std::stringstream& s) {
                    s << "External audio bus id " << busId << " out of range";
                });
            }
            else if ((flags & kMethcla_BusMappingInternal) && (busId < 0 || (size_t)busId >= m_internalAudioBuses.size()))
            {
                throwErrorWith(kMethcla_ArgumentError, [&](std::stringstream& s) {
                    s << "Internal audio bus id " << busId << " out of range";
                });
            }

            Synth* synth = lookupNodeAs<Synth>(m_nodes, "Synth", nodeId);

            if ((index < 0) || (index >= (int32_t)synth->numAudioInputs()))
            {
                throwErrorWith(kMethcla_ArgumentError, [&](std::stringstream& s) {
                    s << "Audio input index " << index << " out of range for synth " << nodeId;
                });
            }

            synth->mapInput(index, AudioBusId(busId), flags);
        }
        else if (msg == "/synth/map/output")
        {
            NodeId nodeId = NodeId(args.int32());
            int32_t index = args.int32();
            int32_t busId = args.int32();
            Methcla_BusMappingFlags flags = Methcla_BusMappingFlags(args.int32());

            if ((flags & kMethcla_BusMappingExternal) && (busId < 0 || (size_t)busId >= m_externalAudioOutputs.size()))
            {
                throwErrorWith(kMethcla_ArgumentError, [&](std::stringstream& s) {
                    s << "External audio bus id " << busId << " out of range";
                });
            }
            else if ((flags & kMethcla_BusMappingInternal) && (busId < 0 || (size_t)busId >= m_internalAudioBuses.size()))
            {
                throwErrorWith(kMethcla_ArgumentError, [&](std::stringstream& s) {
                    s << "Internal audio bus id " << busId << " out of range";
                });
            }

            Synth* synth = lookupNodeAs<Synth>(m_nodes, "Synth", nodeId);

            if ((index < 0) || (index >= (int32_t)synth->numAudioOutputs()))
            {
                throwErrorWith(kMethcla_ArgumentError, [&](std::stringstream& s) {
                    s << "Audio output index " << index << " out of range for synth " << nodeId;
                });
            }

            synth->mapOutput(index, AudioBusId(busId), flags);
        }
        else if (msg == "/synth/property/doneFlags/set")
        {
            NodeId nodeId = NodeId(args.int32());
            Methcla_NodeDoneFlags flags = Methcla_NodeDoneFlags(args.int32());
            Synth* synth = lookupNodeAs<Synth>(m_nodes, "Synth", nodeId);
            synth->setDoneFlags(flags);
        }
        else if (msg == "/node/free")
        {
            NodeId nodeId = NodeId(args.int32());
            Node* node = lookupNode(m_nodes, "Node", nodeId);

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
            NodeId nodeId = NodeId(args.int32());
            int32_t index = args.int32();
            float value = args.float32();

            Synth* synth = lookupNodeAs<Synth>(m_nodes, "Synth", nodeId);

            if ((index < 0) || (index >= (int32_t)synth->numControlInputs()))
            {
                throwErrorWith(kMethcla_ArgumentError, [&](std::stringstream& s) {
                    s << "Control input index " << index << " out of range for synth " << nodeId;
                });
            }

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
                CommandNodeTreeStatistics::collectStatistics(rootNode());

            sendToWorker<CommandNodeTreeStatistics>(requestId, stats);
        }
        else if (msg == "/engine/realtime-memory/statistics")
        {
            class CommandRealtimeMemoryStatistics
            {
            public:
                CommandRealtimeMemoryStatistics(Methcla_RequestId requestId, const RTMemoryManager::Statistics& stats)
                    : m_requestId(requestId)
                    , m_stats(stats)
                {
                }

                void perform(Environment* env)
                {
                    static const char* address = "/engine/realtime-memory/statistics";
                    OSCPP::Client::DynamicPacket packet(
                        OSCPP::Size::message(address, 2)
                      + OSCPP::Size::int32(2)
                    );
                    packet.openMessage(address, 2);
                    packet.int32(m_stats.freeNumBytes);
                    packet.int32(m_stats.usedNumBytes);
                    packet.closeMessage();
                    env->reply(m_requestId, packet);
                    env->sendFromWorker(perform_rt_free, this);
                }

            private:
                Methcla_RequestId           m_requestId;
                RTMemoryManager::Statistics m_stats;
            };

            const Methcla_RequestId requestId = args.int32();
            RTMemoryManager::Statistics stats(rtMem().statistics());
            sendToWorker<CommandRealtimeMemoryStatistics>(requestId, stats);
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
    auto synthDef = Memory::make_shared<SynthDef>(def);
    m_synthDefs[synthDef->uri()] = synthDef;
}

const shared_ptr<SynthDef>& EnvironmentImpl::synthDef(const char* uri) const
{
    auto it = m_synthDefs.find(uri);
    if (it == m_synthDefs.end())
        throw std::runtime_error("Synth definition not found");
    return it->second;
}
