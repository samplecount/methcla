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
#include "Methcla/Utility/MessageQueue.hpp"

#include <boost/current_function.hpp>
#include <cstdlib>
#include <iostream>
#include <oscpp/print.hpp>

using namespace Methcla;
using namespace Methcla::Audio;
using namespace Methcla::Memory;

class Methcla::Audio::EnvironmentImpl
{
public:
    struct ErrorData
    {
        int32_t requestId;
        char*   message;
    };

    static const size_t kQueueSize = 8192;

    EnvironmentImpl()
        : m_worker(kQueueSize, 2)
    { }

    typedef Utility::MessageQueue<Environment::Request,kQueueSize> MessageQueue;
    typedef Utility::WorkerThread<Command> Worker;

    MessageQueue m_requests;
    Worker       m_worker;
};

static void methcla_api_host_register_synthdef(const Methcla_Host* host, const Methcla_SynthDef* synthDef)
{
    return static_cast<Environment*>(host->handle)->registerSynthDef(synthDef);
}

static const Methcla_SoundFileAPI* methcla_api_host_get_soundfile_api(const Methcla_Host* host, const char* mimeType)
{
    return static_cast<Environment*>(host->handle)->soundFileAPI(mimeType);
}

static double methcla_api_world_samplerate(const Methcla_World* world)
{
    return static_cast<Environment*>(world->handle)->sampleRate();
}

static void* methcla_api_world_alloc(const Methcla_World* world, size_t size)
{
    return static_cast<Environment*>(world->handle)->rtMem().alloc(size);
}

static void* methcla_api_world_alloc_aligned(const Methcla_World* world, size_t alignment, size_t size)
{
    return static_cast<Environment*>(world->handle)->rtMem().allocAligned(alignment, size);
}

static void methcla_api_world_free(const Methcla_World* world, void* ptr)
{
    return static_cast<Environment*>(world->handle)->rtMem().free(ptr);
}

static void methcla_api_world_resource_retain(const Methcla_World*, Methcla_Resource resource)
{
    static_cast<Reference*>(resource)->retain();
}

static void methcla_api_world_resource_release(const Methcla_World*, Methcla_Resource resource)
{
    static_cast<Reference*>(resource)->release();
}

static Methcla_Resource methcla_api_world_synth_get_resource(const Methcla_World*, Methcla_Synth* synth)
{
    return Synth::asSynth(synth);
}

static Methcla_Synth* methcla_api_host_resource_get_synth(const Methcla_Host*, Methcla_Resource resource)
{
    return static_cast<Synth*>(resource)->asHandle();
}

Environment::Environment(PluginManager& pluginManager, PacketHandler handler, const Options& options)
    : m_sampleRate(options.sampleRate)
    , m_blockSize(options.blockSize)
    , m_rtMem(options.realtimeMemorySize)
    , m_plugins(pluginManager)
    , m_listener(handler)
    , m_audioBuses    (options.numHardwareInputChannels+options.numHardwareOutputChannels+options.maxNumAudioBuses)
    , m_freeAudioBuses(options.numHardwareInputChannels+options.numHardwareOutputChannels+options.maxNumAudioBuses)
    , m_nodes(options.maxNumNodes)
    , m_epoch(0)
{
    m_impl = new EnvironmentImpl();

    m_rootNode = Group::construct(*this, NodeId(0), nullptr, Node::kAddToTail);
    m_nodes.insert(m_rootNode->id(), m_rootNode);

    const Epoch prevEpoch = epoch() - 1;

    m_audioInputChannels.reserve(options.numHardwareInputChannels);
    for (uint32_t i=0; i < options.numHardwareInputChannels; i++) {
        ExternalAudioBus* bus = new ExternalAudioBus(*this, AudioBusId(i), blockSize(), prevEpoch);
        m_audioBuses.insert(bus->id(), bus);
        m_audioInputChannels.push_back(bus);
    }

    m_audioOutputChannels.reserve(options.numHardwareOutputChannels);
    for (uint32_t i=options.numHardwareInputChannels;
         i < options.numHardwareInputChannels+options.numHardwareOutputChannels;
         i++)
    {
        ExternalAudioBus* bus = new ExternalAudioBus(*this, AudioBusId(i), blockSize(), prevEpoch);
        m_audioBuses.insert(bus->id(), bus);
        m_audioOutputChannels.push_back(bus);
    }

    for (uint32_t i=options.numHardwareInputChannels+options.numHardwareOutputChannels;
         i < m_freeAudioBuses.size();
         i++)
    {
        AudioBus* bus = new InternalAudioBus(*this, AudioBusId(i), blockSize(), prevEpoch);
        m_freeAudioBuses.insert(bus->id(), bus);
    }

    // Initialize Methcla_Host interface
    m_host.handle = this;
    m_host.register_synthdef = methcla_api_host_register_synthdef;
    m_host.get_soundfile_api = methcla_api_host_get_soundfile_api;
    m_host.perform_command = methcla_api_host_perform_command;
    m_host.resource_get_synth = methcla_api_host_resource_get_synth;

    // Initialize Methcla_World interface
    m_world.handle = this;
    m_world.samplerate = methcla_api_world_samplerate;
    m_world.alloc = methcla_api_world_alloc;
    m_world.alloc_aligned = methcla_api_world_alloc_aligned;
    m_world.free = methcla_api_world_free;
    m_world.perform_command = methcla_api_world_perform_command;
    m_world.retain = methcla_api_world_resource_retain;
    m_world.release = methcla_api_world_resource_release;
    m_world.synth_get_resource = methcla_api_world_synth_get_resource;
}

Environment::~Environment()
{
}

AudioBus* Environment::audioBus(const AudioBusId& id)
{
    return m_audioBuses.lookup(id).get();
}

AudioBus& Environment::externalAudioOutput(size_t index)
{
    return *m_audioOutputChannels[index];
}

AudioBus& Environment::externalAudioInput(size_t index)
{
    return *m_audioInputChannels[index];
}

void Environment::send(const void* packet, size_t size)
{
    char* myPacket = Memory::allocAlignedOf<char>(OSC::kAlignment, size);
    memcpy(myPacket, packet, size);
    Request req;
    req.packet = myPacket;
    req.size = size;
    m_impl->m_requests.send(req);
}

void Environment::sendToWorker(const Command& cmd)
{
    m_impl->m_worker.sendToWorker(cmd);
}

void Environment::sendFromWorker(const Command& cmd)
{
    m_impl->m_worker.sendFromWorker(cmd);
}

void Environment::process(size_t numFrames, const sample_t* const* inputs, sample_t* const* outputs)
{
    BOOST_ASSERT_MSG( numFrames <= blockSize(), "numFrames exceeds blockSize()" );

    // Process external requests
    processRequests();

    // Process non-realtime commands
    m_impl->m_worker.perform();

    const size_t numInputs = m_audioInputChannels.size();
    const size_t numOutputs = m_audioOutputChannels.size();

    // Connect input and output buses
    for (size_t i=0; i < numInputs; i++) {
        m_audioInputChannels[i]->setData(const_cast<sample_t*>(inputs[i]));
        m_audioInputChannels[i]->setEpoch(epoch());
    }
    for (size_t i=0; i < numOutputs; i++) {
        m_audioOutputChannels[i]->setData(outputs[i]);
    }

    // Run DSP graph
    m_rootNode->process(numFrames);

    // Zero outputs that haven't been written to
    for (size_t i=0; i < numOutputs; i++) {
        if (m_audioOutputChannels[i]->epoch() != epoch()) {
            memset(outputs[i], 0, numFrames * sizeof(sample_t));
        }
    }

    m_epoch++;
}

static void perform_nrt_free(Environment* env, CommandData* data)
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
    const size_t packetSize = OSC::Size::message(address, numArgs)
                                + OSC::Size::int32();
    const int32_t requestId = *static_cast<int32_t*>(data);
    OSC::Client::StaticPacket<packetSize> packet;
    // OSC::Client::DynamicPacket packet(kPacketSize);
    packet
        .openMessage(address, numArgs)
            .int32(requestId)
        .closeMessage();
    env->reply(requestId, packet);
    env->sendFromWorker(Command(env, perform_rt_free, data));
}

void Environment::perform_response_nodeId(Environment* env, CommandData* data)
{
    const char address[] = "/ack";
    const size_t numArgs = 2;
    const size_t packetSize = OSC::Size::message(address, numArgs)
                                + 2 * OSC::Size::int32();
    const int32_t requestId = ((int32_t*)data)[0];
    const int32_t nodeId = ((int32_t*)data)[1];
    OSC::Client::StaticPacket<packetSize> packet;
    // OSC::Client::DynamicPacket packet(kPacketSize);
    packet
        .openMessage(address, numArgs)
            .int32(requestId)
            .int32(nodeId)
        .closeMessage();
    env->reply(requestId, packet);
    env->sendFromWorker(Command(env, perform_rt_free, data));
}

void Environment::perform_response_query_external_inputs(Environment* env, CommandData* data)
{
    // const char address[] = "/ack";
    // const size_t numBuses = env->numExternalAudioInputs();
    // const size_t numArgs = 1 + numBuses;
    // const size_t packetSize = OSC::Size::message(address, numArgs) + numArgs * OSC::Size::int32();
    // OSC::Client::DynamicPacket packet(packetSize);
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
    // const size_t packetSize = OSC::Size::message(address, numArgs) +  numArgs * OSC::Size::int32();
    // OSC::Client::DynamicPacket packet(packetSize);
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
    const size_t packetSize = OSC::Size::message(address, numArgs)
                             + OSC::Size::int32()
                             + OSC::Size::string(strlen(data->message));
    OSC::Client::DynamicPacket packet(packetSize);
    packet
        .openMessage(address, numArgs)
            .int32(data->requestId)
            .string(data->message)
        .closeMessage();
    env->reply(data->requestId, packet);
    env->sendFromWorker(Command(env, perform_rt_free, data));
}

void Environment::replyError(Methcla_RequestId requestId, const char* msg)
{
    EnvironmentImpl::ErrorData* data =
        (EnvironmentImpl::ErrorData*)rtMem().alloc(sizeof(EnvironmentImpl::ErrorData)+strlen(msg)+1);
    data->requestId = requestId;
    data->message = (char*)data + sizeof(EnvironmentImpl::ErrorData);
    strcpy(data->message, msg);
    sendToWorker(Command(this, perform_response_error, data));
}

void Environment::processRequests()
{
    Request msg;
    while (m_impl->m_requests.next(msg)) {
        try {
            OSC::Server::Packet packet(msg.packet, msg.size);
            if (packet.isBundle()) {
                processBundle(packet);
            } else {
                processMessage(packet);
            }
        } catch (std::exception& e) {
            std::cerr << "Unhandled exception in `processRequests': " << e.what() << std::endl;
        }
        // Free packet in NRT thread
        sendToWorker(Command(this, perform_nrt_free, msg.packet));
    }
}

void Environment::processMessage(const OSC::Server::Message& msg)
{
#if 0
    std::cerr << "Request (recv): " << msg << std::endl;
#endif

    auto args = msg.args();
    // Methcla_RequestId requestId = args.int32();

    try {
        if (msg == "/s_new") {
            const char* defName = args.string();
            NodeId nodeId = NodeId(args.int32());
            NodeId targetId = NodeId(args.int32());
            int32_t addAction = args.int32();

            const std::shared_ptr<SynthDef> def = synthDef(defName);

            auto synthControls = args.atEnd() ? OSC::Server::ArgStream() : args.array();
            // FIXME: Cannot be checked before the synth is instantiated.
            // if (def->numControlInputs() != synthControls.size()) {
            //     throw std::runtime_error("Missing synth control initialisers");
            // }
            auto synthArgs = args.atEnd() ? OSC::Server::ArgStream() : args.array();

            Node* targetNode = m_nodes.lookup(targetId).get();
            Group* targetGroup = targetNode->isGroup() ? dynamic_cast<Group*>(targetNode)
                                                       : dynamic_cast<Synth*>(targetNode)->parent();
            Synth* synth = Synth::construct(
                *this,
                nodeId,
                targetGroup,
                Node::kAddToTail,
                *def,
                synthControls,
                synthArgs);
            nodes().insert(synth->id(), synth);

            // int32_t* data = rtMem().allocOf<int32_t>(2);
            // data[0] = requestId;
            // data[1] = synth->id();
            // sendToWorker(Command(this, perform_response_nodeId, data));
        } else if (msg == "/g_new") {
            NodeId nodeId = NodeId(args.int32());
            NodeId targetId = NodeId(args.int32());
            int32_t addAction = args.int32();

            Node* targetNode = m_nodes.lookup(targetId).get();
            Group* targetGroup = targetNode->isGroup() ? dynamic_cast<Group*>(targetNode)
                                                       : dynamic_cast<Synth*>(targetNode)->parent();
            Group* group = Group::construct(*this, nodeId, targetGroup, Node::kAddToTail);
            nodes().insert(group->id(), group);

            // int32_t* data = rtMem().allocOf<int32_t>(2);
            // data[0] = requestId;
            // data[1] = group->id();
            // sendToWorker(Command(this, perform_response_nodeId, data));
        } else if (msg == "/n_free") {
            NodeId nodeId = NodeId(args.int32());

            // Check node id validity
            if (!nodes().contains(nodeId)) {
                throw std::runtime_error("Invalid node id");
            } else if (nodeId == rootNode()->id()) {
                throw std::runtime_error("Cannot free root node");
            }

            // Drop reference from node map
            m_nodes.remove(nodeId);

            // Send reply
            // int32_t* data = rtMem().allocOf<int32_t>(1);
            // data[0] = requestId;
            // sendToWorker(Command(this, perform_response_ack, data));
        } else if (msg == "/n_set") {
            NodeId nodeId = NodeId(args.int32());
            int32_t index = args.int32();
            float value = args.float32();
            Node* node = m_nodes.lookup(nodeId).get();
            if (!node->isSynth())
                throw std::runtime_error("Node is not a synth");
            Synth* synth = dynamic_cast<Synth*>(node);
            if ((index < 0) || (index >= synth->numControlInputs())) {
                throw std::runtime_error("Control input index out of range");
            }
            synth->controlInput(index) = value;
            // int32_t* data = rtMem().allocOf<int32_t>(1);
            // data[0] = requestId;
            // sendToWorker(Command(this, perform_response_ack, data));
        } else if (msg == "/synth/map/output") {
            NodeId nodeId = NodeId(args.int32());
            int32_t index = args.int32();
            AudioBusId busId = AudioBusId(args.int32());

            Node* node = m_nodes.lookup(nodeId).get();
            // Could traverse all synths in a group
            if (!node->isSynth())
                throw std::runtime_error("Node is not a synth");

            Synth* synth = dynamic_cast<Synth*>(node);
            if ((index < 0) || (index >= synth->numAudioOutputs()))
                throw std::runtime_error("Synth output index out of range");

            synth->mapOutput(index, busId, kOut);

            // Could reply with previous bus mapping
            // int32_t* data = rtMem().allocOf<int32_t>(1);
            // data[0] = requestId;
            // sendToWorker(Command(this, perform_response_ack, data));
        } else if (msg == "/query/external_inputs") {
    // Methcla_RequestId requestId = args.int32();
            // sendToWorker(Command(this, perform_response_query_external_inputs, requestId));
        } else if (msg == "/query/external_outputs") {
    // Methcla_RequestId requestId = args.int32();
            // sendToWorker(Command(this, perform_response_query_external_outputs, requestId));
        }
    } catch (Exception& e) {
        const std::string* errorInfo = boost::get_error_info<ErrorInfoString>(e);
        const char* errorMessage = errorInfo == nullptr ? "Unknown error" : errorInfo->c_str();
        replyError(kMethcla_Notification, errorMessage);
    } catch (std::exception& e) {
        replyError(kMethcla_Notification, e.what());
    }
}

void Environment::processBundle(const OSC::Server::Bundle& bundle)
{
    // throw std::runtime_error("Bundle support not implemented yet");
    for (auto p : bundle) {
        if (p.isBundle()) {
            processBundle(p);
        } else {
            processMessage(p);
        }
    }
}

void Environment::registerSynthDef(const Methcla_SynthDef* def)
{
    auto synthDef = std::make_shared<SynthDef>(def);
    m_synthDefs[synthDef->uri()] = synthDef;
}

const std::shared_ptr<SynthDef>& Environment::synthDef(const char* uri) const
{
    auto it = m_synthDefs.find(uri);
    if (it == m_synthDefs.end())
        throw std::runtime_error("Synth definition not found");
    return it->second;
}

void Environment::registerSoundFileAPI(const char* mimeType, const Methcla_SoundFileAPI* api)
{
    m_soundFileAPIs.push_back(api);
}

const Methcla_SoundFileAPI* Environment::soundFileAPI(const char* mimeType) const
{
    return m_soundFileAPIs.empty() ? nullptr : m_soundFileAPIs.front();
}

template <typename T> struct CallbackData
{
    T     func;
    void* arg;
};

void Environment::perform_worldCommand(Environment* env, CommandData* data)
{
    CallbackData<Methcla_WorldPerformFunction>* self = (CallbackData<Methcla_WorldPerformFunction>*)data;
    self->func(env->asWorld(), self->arg);
    env->sendToWorker(Command(env, perform_nrt_free, self));
}

void Environment::perform_hostCommand(Environment* env, CommandData* data)
{
    CallbackData<Methcla_HostPerformFunction>* self = (CallbackData<Methcla_HostPerformFunction>*)data;
    self->func(env->asHost(), self->arg);
    env->sendFromWorker(Command(env, perform_rt_free, self));
}

void Environment::methcla_api_host_perform_command(const Methcla_Host* host, Methcla_WorldPerformFunction perform, void* data)
{
    Environment* env = static_cast<Environment*>(host->handle);
    auto callbackData = Methcla::Memory::allocOf<CallbackData<Methcla_WorldPerformFunction>>();
    callbackData->func = perform;
    callbackData->arg = data;
    env->sendFromWorker(Command(env, perform_worldCommand, callbackData));
}

void Environment::methcla_api_world_perform_command(const Methcla_World* world, Methcla_HostPerformFunction perform, void* data)
{
    Environment* env = static_cast<Environment*>(world->handle);
    auto callbackData = env->rtMem().allocOf<CallbackData<Methcla_HostPerformFunction>>();
    callbackData->func = perform;
    callbackData->arg = data;
    env->sendToWorker(Command(env, perform_hostCommand, callbackData));
}

Engine::Engine(PluginManager& pluginManager, const PacketHandler& handler, const std::string& pluginDirectory)
{
    m_driver = IO::defaultPlatformDriver();
    m_driver->setProcessCallback(processCallback, this);

    Environment::Options options;
    options.sampleRate = m_driver->sampleRate();
    options.blockSize = m_driver->bufferSize();
    options.numHardwareInputChannels = m_driver->numInputs();
    options.numHardwareOutputChannels = m_driver->numOutputs();
    m_env = new Environment(pluginManager, handler, options);

    pluginManager.loadPlugins(m_env->asHost(), pluginDirectory);
}

Engine::~Engine()
{
    stop();
    delete m_env;
    delete m_driver;
}

void Engine::start()
{
    m_driver->start();
}

void Engine::stop()
{
    m_driver->stop();
}

void Engine::processCallback(void* data, size_t numFrames, const sample_t* const* inputs, sample_t* const* outputs)
{
    static_cast<Engine*>(data)->m_env->process(numFrames, inputs, outputs);
}

void Engine::makeSine()
{
    // const std::shared_ptr<SynthDef> def = env().synthDef("http://methc.la/plugins/sine");
    // 
    // for (auto freq : { 440, 660, 880, 1320 }) {
    //     OSC::Client::DynamicPacket packet(128);
    //     packet.openMessage("/foo", 1);
    //     packet.float32(freq);
    //     packet.closeMessage();
    //     auto synthControls = static_cast<OSC::Server::Message>(OSC::Server::Packet(packet.data(), packet.size())).args();
    //     auto synthOptions = OSC::Server::ArgStream();
    // 
    //     Synth* synth = Synth::construct(
    //         env(),
    //         env().nodes().nextId(),
    //         env().rootNode(),
    //         Node::kAddToTail,
    //         *def,
    //         synthControls,
    //         synthOptions);
    //     env().nodes().insert(synth->id(), synth);
    // 
    //     synth->mapOutput(0, AudioBusId(1), kOut);
    // }
}
