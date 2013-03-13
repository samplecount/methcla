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
#include "Methcla/LV2/Atom.hpp"

#include <boost/current_function.hpp>
#include <cstdlib>
#include <iostream>

#include "lv2/lv2plug.in/ns/ext/atom/util.h"

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

Environment::Environment(PluginManager& pluginManager, const Options& options)
    : m_sampleRate(options.sampleRate)
    , m_blockSize(options.blockSize)
    , m_plugins(pluginManager)
    , m_audioBuses    (options.numHardwareInputChannels+options.numHardwareOutputChannels+options.maxNumAudioBuses)
    , m_freeAudioBuses(options.numHardwareInputChannels+options.numHardwareOutputChannels+options.maxNumAudioBuses)
    , m_nodes(options.maxNumNodes)
    , m_rootNode(Group::construct(*this, nullptr, Node::kAddToTail))
    , m_epoch(0)
    , m_worker(uriMap())
    , m_uris(uriMap())
    , m_parser(uriMap().lv2Map())
    , m_printer(uriMap().lv2Map(), uriMap().lv2Unmap())
{
    const Epoch prevEpoch = epoch() - 1;

    m_audioInputChannels.reserve(options.numHardwareInputChannels);
    for (uint32_t i=0; i < options.numHardwareInputChannels; i++) {
        ExternalAudioBus* bus = new ExternalAudioBus(*this, AudioBusId(i), blockSize(), prevEpoch);
        m_audioBuses.insert(bus->id(), bus);
        m_audioInputChannels.push_back(bus);
    }

    m_audioOutputChannels.reserve(options.numHardwareOutputChannels);
    for (uint32_t i=0; i < options.numHardwareOutputChannels; i++) {
        ExternalAudioBus* bus = new ExternalAudioBus(*this, AudioBusId(i), blockSize(), prevEpoch);
        m_audioBuses.insert(bus->id(), bus);
        m_audioOutputChannels.push_back(bus);
    }

    for (uint32_t i=options.numHardwareInputChannels+options.numHardwareOutputChannels; i < m_freeAudioBuses.size(); i++) {
        AudioBus* bus = new InternalAudioBus(*this, AudioBusId(i), blockSize(), prevEpoch);
        m_freeAudioBuses.insert(bus->id(), bus);
    }
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

void Environment::request(MessageQueue::Respond respond, void* data, const LV2_Atom* msg)
{
    m_requests.send(respond, data, msg);
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

static void forgeReturnEnvelope(::LV2::Forge& forge, const Uris& uris, const Environment::MessageQueue::Message& msg)
{
    forge.atom(sizeof(msg), uris.atom_Chunk);
    forge.write(&msg, sizeof(msg));
}

static void forgeError(::LV2::Forge& forge, const Uris& uris, const char* errorMessage)
{
    ::LV2::ObjectFrame frame(forge, 0, uris.patch_Error);
    forge << ::LV2::Property(uris.methcla_errorMessage)
          << errorMessage;
}

static void forgeException(::LV2::Forge& forge, const Uris& uris, const Exception& e)
{
    const std::string* errorInfo = boost::get_error_info<ErrorInfoString>(e);
    const char* errorMessage = errorInfo == nullptr ? "Unknown error" : errorInfo->c_str();
    forgeError(forge, uris, errorMessage);
}

static void sendReply(void* data, const LV2_Atom* payload, Environment::Worker::Writer& /* writer */)
{
    const LV2::Parser& parser = *static_cast<const LV2::Parser*>(data);
    auto tuple = parser.cast<const LV2_Atom_Tuple*>(payload);
    auto iter = lv2_atom_tuple_begin(tuple);
    auto msg = parser.cast<const Environment::MessageQueue::Message*>(iter);
    auto response = lv2_atom_tuple_next(iter);
    msg->respond(response);
}

void Environment::processRequests()
{
    MessageQueue::Message msg;
    while (m_requests.next(msg)) {
        try {
            handleRequest(msg);
        } catch(Exception& e) {
            // Send Error response
            ::LV2::Forge forge(*prepare(sendReply, &m_parser));
            {
                ::LV2::TupleFrame frame(forge);
                forgeReturnEnvelope(frame, uris(), msg);
                forgeException(frame, uris(), e);
            }
            commit();
        } catch(std::exception& e) {
            // Send Error response
            ::LV2::Forge forge(*prepare(sendReply, &m_parser));
            {
                ::LV2::TupleFrame frame(forge);
                forgeReturnEnvelope(frame, uris(), msg);
                forgeError(frame, uris(), e.what());
            }
            commit();
        }
    }
}

/*
*/
void Environment::handleRequest(MessageQueue::Message& request)
{
    const LV2_Atom* atom = request.payload();

    std::cout << BOOST_CURRENT_FUNCTION << std::endl;
    m_printer.print(std::cout, atom, 4);

    if (m_parser.isObject(atom))
        handleMessageRequest(request, m_parser.cast<const LV2_Atom_Object*>(atom));
    else if (m_parser.isSequence(atom))
        handleSequenceRequest(request, m_parser.cast<const LV2_Atom_Sequence*>(atom));
    else
        BOOST_THROW_EXCEPTION(Exception() << ErrorInfoString("Invalid request type"));
}

static void missingProperty(const char* name)
{
    BOOST_THROW_EXCEPTION( Exception() << ErrorInfoString(std::string("missing property ") + name) );
}

static void checkProperty(const LV2_Atom* atom, const char* name)
{
    if (atom == nullptr) missingProperty(name);
}

void Environment::handleMessageRequest(MessageQueue::Message& request, const LV2_Atom_Object* msg)
{
    const LV2_Atom* subjectAtom = nullptr;
    const LV2_Atom* bodyAtom = nullptr;
    const LV2_URID requestType = msg->body.otype;

    int matches = lv2_atom_object_get(
                    msg
                  , uris().patch_subject, &subjectAtom
                  , uris().patch_body, &bodyAtom
                  , nullptr );

    checkProperty(subjectAtom, LV2_PATCH__subject);
    checkProperty(bodyAtom, LV2_PATCH__body);

    const LV2_Atom_Object* subject = m_parser.cast<const LV2_Atom_Object*>(subjectAtom);
    const LV2_Atom_Object* body = m_parser.cast<const LV2_Atom_Object*>(bodyAtom);

    if (LV2::isa(subject, uris().methcla_Node)) {
        // Get target node
        const LV2_Atom* targetAtom = nullptr;
        lv2_atom_object_get(subject, uris().methcla_id, &targetAtom, nullptr);
        checkProperty(targetAtom, METHCLA_ENGINE_PREFIX "id");

        NodeId targetId(m_parser.cast<int32_t>(targetAtom));
        Node* targetNode = m_nodes.lookup(targetId);
        if (targetNode == nullptr)
            BOOST_THROW_EXCEPTION( Exception() << ErrorInfoString("target node not found")
                                               << ErrorInfoNodeId(targetId) );

        Synth* targetSynth = dynamic_cast<Synth*>(targetNode);
        Group* targetGroup = targetSynth == nullptr ? dynamic_cast<Group*>(targetNode) : targetSynth->parent();

        if (requestType == uris().patch_Insert) {
            Node* node = nullptr;

            if (LV2::isa(body, uris().methcla_Synth)) {
                // Get plugin URI
                const LV2_Atom* pluginAtom = nullptr;
                lv2_atom_object_get(body, uris().methcla_plugin, &pluginAtom, nullptr);
                checkProperty(pluginAtom, METHCLA_ENGINE_PREFIX "plugin");
                LV2_URID pluginURID = m_parser.cast<LV2_URID>(pluginAtom);

                // get params and bus mappings from body

                const std::shared_ptr<Plugin> def = plugins().lookup(pluginURID);

                node = Synth::construct(*this, targetGroup, Node::kAddToTail, *def);
            } else if (LV2::isa(body, uris().methcla_Group)) {
                node = Group::construct(*this, targetGroup, Node::kAddToTail);
            } else {
                BOOST_THROW_EXCEPTION( Exception() << ErrorInfoString("invalid body type for " LV2_PATCH__Insert) );
            }

            // Send reply with node id (from NRT thread)
            ::LV2::Forge forge(*prepare(sendReply, &m_parser));
            {
                ::LV2::TupleFrame frame(forge);
                forgeReturnEnvelope(frame, uris(), request);
                {
                    ::LV2::ObjectFrame frame(forge, 0, uris().patch_Ack);
                    forge << ::LV2::Property(uris().patch_subject);
                    {
                        ::LV2::ObjectFrame frame(forge, 0, uris().methcla_Node); // TODO: Use concrete node type
                        forge << ::LV2::Property(uris().methcla_id)
                              << (int32_t)node->id();
                    }
                }
            }
            commit();
        } else if (requestType == uris().patch_Delete) {
            targetNode->free();
        } else if (requestType == uris().patch_Set) {
            // get params and bus mappings from body
        }
    } else {
        BOOST_THROW_EXCEPTION( Exception() << ErrorInfoString("unknown subject type for " LV2_PATCH__Request) );
    }
}

void Environment::handleSequenceRequest(MessageQueue::Message& request, const LV2_Atom_Sequence* bdl)
{
    std::cerr << "Sequence requests not supported yet\n";
}


Engine::Engine(PluginManager& pluginManager, const boost::filesystem::path& lv2Directory)
{
    m_driver = IO::defaultPlatformDriver();
    m_driver->setProcessCallback(processCallback, this);

    Environment::Options options;
    options.sampleRate = m_driver->sampleRate();
    options.blockSize = m_driver->bufferSize();
    options.numHardwareInputChannels = m_driver->numInputs();
    options.numHardwareOutputChannels = m_driver->numOutputs();
    m_env = new Environment(pluginManager, options);

    pluginManager.loadPlugins(lv2Directory);
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
