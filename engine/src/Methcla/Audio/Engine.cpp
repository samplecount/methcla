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

#include <boost/current_function.hpp>
#include <cstdlib>
#include <iostream>
#include <oscpp/print.hpp>

using namespace Methcla;
using namespace Methcla::Audio;
using namespace Methcla::Memory;
using namespace std;

void NodeMap::insert(Node* node)
{
    NodeId id = node->id();
    if (m_nodes[id] != 0)
        BOOST_THROW_EXCEPTION(DuplicateNodeId() << ErrorInfoNodeId(id));
    m_nodes[id] = node;
}

void NodeMap::release(const NodeId& nodeId)
{
    if (m_nodes[nodeId] == 0)
        BOOST_THROW_EXCEPTION(InvalidNodeId() << ErrorInfoNodeId(nodeId));
    m_nodes[nodeId] = 0;
}

static double worldSampleRate(Methcla_WorldHandle handle)
{
    return static_cast<Environment*>(handle)->sampleRate();
}

Environment::Environment(PluginManager& pluginManager, const PacketHandler& handler, const Options& options)
    : m_sampleRate(options.sampleRate)
    , m_blockSize(options.blockSize)
    , m_rtMem(options.realtimeMemorySize)
    , m_plugins(pluginManager)
    , m_listener(handler)
    , m_audioBuses    (options.numHardwareInputChannels+options.numHardwareOutputChannels+options.maxNumAudioBuses)
    , m_freeAudioBuses(options.numHardwareInputChannels+options.numHardwareOutputChannels+options.maxNumAudioBuses)
    , m_nodes(options.maxNumNodes)
    , m_rootNode(Group::construct(*this, nullptr, Node::kAddToTail))
    , m_epoch(0)
{
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

    // Initialize Methcla_World interface
    m_world.handle = this;
    m_world.sampleRate = worldSampleRate;
}

Environment::~Environment()
{
}

AudioBus* Environment::audioBus(const AudioBusId& id)
{
    return m_audioBuses.lookup(id);
}

AudioBus& Environment::externalAudioOutput(size_t index)
{
    return *m_audioOutputChannels[index];
}

AudioBus& Environment::externalAudioInput(size_t index)
{
    return *m_audioInputChannels[index];
}

static void freePacket(void* packet)
{
    Memory::free(packet);
}

void Environment::send(const void* packet, size_t size)
{
    char* myPacket = Memory::allocAlignedOf<char,OSC::kAlignment>(size);
    memcpy(myPacket, packet, size);
    Request req;
    req.packet = myPacket;
    req.size = size;
    req.free = freePacket;
    m_requests.send(req);
}

void Environment::process(size_t numFrames, sample_t** inputs, sample_t** outputs)
{
    BOOST_ASSERT_MSG( numFrames <= blockSize(), "numFrames exceeds blockSize()" );

    // Process external requests
    processRequests();

    // Process non-realtime commands
    m_worker.perform();

    const size_t numInputs = m_audioInputChannels.size();
    const size_t numOutputs = m_audioOutputChannels.size();

    // Connect input and output buses
    for (size_t i=0; i < numInputs; i++) {
        m_audioInputChannels[i]->setData(inputs[i]);
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

void Environment::perform_free(Command& cmd, Command::Channel&)
{
    cmd.data.free.func(cmd.data.free.ptr);
}

void Environment::perform_response_ack(Command& cmd, Command::Channel&)
{
    const char address[] = "/ack";
    const size_t numArgs = 1;
    const size_t packetSize = OSC::Size::message(address, numArgs)
                                + OSC::Size::int32();
    OSC::Client::StaticPacket<packetSize> packet;
    // OSC::Client::DynamicPacket packet(kPacketSize);
    packet
        .openMessage(address, numArgs)
            .int32(cmd.data.response.requestId)
        .closeMessage();
    cmd.env->reply(cmd.data.response.requestId, packet);
}

void Environment::perform_response_nodeId(Command& cmd, Command::Channel&)
{
    const char address[] = "/ack";
    const size_t numArgs = 2;
    const size_t packetSize = OSC::Size::message(address, numArgs)
                                + 2 * OSC::Size::int32();
    OSC::Client::StaticPacket<packetSize> packet;
    // OSC::Client::DynamicPacket packet(kPacketSize);
    packet
        .openMessage(address, numArgs)
            .int32(cmd.data.response.requestId)
            .int32(cmd.data.response.data.nodeId)
        .closeMessage();
    cmd.env->reply(cmd.data.response.requestId, packet);
}

void Environment::perform_response_error(Command& cmd, Command::Channel&)
{
    const char address[] = "/error";
    const size_t numArgs = 2;
    const size_t packetSize = OSC::Size::message(address, numArgs)
                                + OSC::Size::int32()
                                + OSC::Size::string(sizeof(cmd.data.response.data.error));
    OSC::Client::StaticPacket<packetSize> packet;
    // OSC::Client::DynamicPacket packet(kPacketSize);
    packet
        .openMessage(address, numArgs)
            .int32(cmd.data.response.requestId)
            .string(cmd.data.response.data.error)
        .closeMessage();
    cmd.env->reply(cmd.data.response.requestId, packet);
}

void Environment::perform_response_query_external_inputs(Command& cmd, Command::Channel&)
{
    Environment* env = cmd.env;
    const char address[] = "/ack";
    const size_t numBuses = env->numExternalAudioInputs();
    const size_t numArgs = 1 + numBuses;
    const size_t packetSize = OSC::Size::message(address, numArgs) + numArgs * OSC::Size::int32();
    OSC::Client::DynamicPacket packet(packetSize);
    packet.openMessage(address, numArgs);
    packet.int32(cmd.data.response.requestId);
    for (size_t i=0; i < numBuses; i++) {
        packet.int32(env->externalAudioInput(i).id());
    }
    packet.closeMessage();
    env->reply(cmd.data.response.requestId, packet);
}

void Environment::perform_response_query_external_outputs(Command& cmd, Command::Channel&)
{
    Environment* env = cmd.env;
    const char address[] = "/ack";
    const size_t numBuses = env->numExternalAudioOutputs();
    const size_t numArgs = 1 + numBuses;
    const size_t packetSize = OSC::Size::message(address, numArgs) +  numArgs * OSC::Size::int32();
    OSC::Client::DynamicPacket packet(packetSize);
    packet.openMessage(address, numArgs);
    packet.int32(cmd.data.response.requestId);
    for (size_t i=0; i < numBuses; i++) {
        packet.int32(env->externalAudioOutput(i).id());
    }
    packet.closeMessage();
    env->reply(cmd.data.response.requestId, packet);
}

void Environment::replyError(Methcla_RequestId requestId, const char* msg)
{
    Command cmd(this, perform_response_error, requestId);
    strncpy(cmd.data.response.data.error, msg, sizeof(cmd.data.response.data.error));
    send(cmd);
}

void Environment::processRequests()
{
    Request msg;
    while (m_requests.next(msg)) {
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
        Command cmd(this, perform_free);
        cmd.data.free.func = msg.free;
        cmd.data.free.ptr  = msg.packet;
        send(cmd);
    }
}

void Environment::processMessage(const OSC::Server::Message& msg)
{
    std::cerr << "Request (recv): " << msg << std::endl;

    auto args = msg.args();
    Methcla_RequestId requestId = args.int32();

    try {
        if (msg == "/s_new") {
            const char* defName = args.string();
            NodeId targetId = NodeId(args.int32());
            int32_t addAction = args.int32();

            const std::shared_ptr<SynthDef> def = plugins().lookup(defName);

            auto synthControls = args.atEnd() ? OSC::Server::ArgStream() : args.array();
            if (def->numControlInputs() != synthControls.size()) {
                throw std::runtime_error("Missing synth control initialisers");
            }
            auto synthArgs = args.atEnd() ? OSC::Server::ArgStream() : args.array();

            Node* targetNode = m_nodes.lookup(targetId);
            Group* targetGroup = targetNode->isGroup() ? dynamic_cast<Group*>(targetNode)
                                                       : dynamic_cast<Synth*>(targetNode)->parent();
            Synth* synth = Synth::construct(*this, targetGroup, Node::kAddToTail, *def, synthControls, synthArgs);

            Command cmd(this, perform_response_nodeId, requestId);
            cmd.data.response.data.nodeId = synth->id();
            send(cmd);
        } else if (msg == "/g_new") {
            NodeId targetId = NodeId(args.int32());
            int32_t addAction = args.int32();

            Node* targetNode = m_nodes.lookup(targetId);
            Group* targetGroup = targetNode->isGroup() ? dynamic_cast<Group*>(targetNode)
                                                       : dynamic_cast<Synth*>(targetNode)->parent();
            Node* node = Group::construct(*this, targetGroup, Node::kAddToTail);

            Command cmd(this, perform_response_nodeId, requestId);
            cmd.data.response.data.nodeId = node->id();
            send(cmd);
        } else if (msg == "/n_free") {
            NodeId nodeId = NodeId(args.int32());
            m_nodes.lookup(nodeId)->free();

            Command cmd(this, perform_response_nodeId, requestId);
            cmd.data.response.data.nodeId = nodeId;
            send(cmd);
        } else if (msg == "/n_set") {
        } else if (msg == "/synth/map/output") {
            NodeId nodeId = NodeId(args.int32());
            int32_t index = args.int32();
            AudioBusId busId = AudioBusId(args.int32());

            Node* node = m_nodes.lookup(nodeId);
            // Could traverse all synths in a group
            if (!node->isSynth())
                throw std::runtime_error("Node is not a synth");

            Synth* synth = dynamic_cast<Synth*>(node);
            if ((index < 0) || (index >= synth->numAudioOutputs()))
                throw std::runtime_error("Synth output index out of range");

            synth->mapOutput(index, busId, kOut);

            // Could reply with previous bus mapping
            send(Command(this, perform_response_ack, requestId));
        } else if (msg == "/query/external_inputs") {
            send(Command(this, perform_response_query_external_inputs, requestId));
        } else if (msg == "/query/external_outputs") {
            send(Command(this, perform_response_query_external_outputs, requestId));
        }
    } catch (Exception& e) {
        const std::string* errorInfo = boost::get_error_info<ErrorInfoString>(e);
        const char* errorMessage = errorInfo == nullptr ? "Unknown error" : errorInfo->c_str();
        replyError(requestId, errorMessage);
    } catch (std::exception& e) {
        replyError(requestId, e.what());
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

    pluginManager.loadPlugins(pluginDirectory);
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

void Engine::processCallback(void* data, size_t numFrames, sample_t** inputs, sample_t** outputs)
{
    static_cast<Engine*>(data)->m_env->process(numFrames, inputs, outputs);
}
