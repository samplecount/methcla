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

void Environment::processRequests()
{
    MessageQueue::Message msg;
    while (m_requests.next(msg)) {
        try {
            handleRequest(msg);
        } catch(Exception& e) {
            // TODO: Send Error response
            std::cerr << "Exception caught in handleRequest()\n";
        }
    }
}

/*
*/
void Environment::handleRequest(MessageQueue::Message& request)
{
    const LV2_Atom* atom = request.payload();
    cout << "Message: " << atom << endl
         << "    atom size: " << atom->size << endl
         << "    atom type: " << atom->type << endl
         << "    atom uri:  " << unmapUri(atom->type) << endl;
    if (uris().isObject(atom))
        handleMessageRequest(request, reinterpret_cast<const LV2_Atom_Object*>(atom));
    else if (atom->type == uris().atom_Sequence)
        handleSequenceRequest(request, reinterpret_cast<const LV2_Atom_Sequence*>(atom));
    else
        BOOST_THROW_EXCEPTION(Exception() << ErrorInfoString("Invalid request type"));
}

void Environment::handleMessageRequest(MessageQueue::Message& request, const LV2_Atom_Object* msg)
{
    // const char* atom_type = unmapUri(msg->atom.type);
    // const char* uri_type = unmapUri(msg->body.otype);
    // if (msg->atom.type == uris().atom_Blank) {
    //     cout << atom_type << " " << msg->body.id << " " << uri_type << endl;
    // } else {
    //     const char* uri_id = unmapUri(msg->body.id);
    //     cout << atom_type << " " << uri_id << " " << uri_type << endl;
    // }
    // LV2_ATOM_OBJECT_FOREACH(msg, prop) {
    //     cout << "  " << unmapUri(prop->key) << " " << prop->context << ": " << unmapUri(prop->value.type) << endl;
    // }
    const LV2_Atom* subjectAtom = nullptr;
    const LV2_Atom* bodyAtom = nullptr;
    LV2_URID requestType = msg->body.otype;

    int matches = lv2_atom_object_get(
                    msg
                  , uris().patch_subject, &subjectAtom
                  , uris().patch_body, &bodyAtom
                  , nullptr );

    BOOST_ASSERT_MSG( subjectAtom != nullptr, "Message must have subject property" );

    const LV2_Atom_Object* subject = uris().toObject(subjectAtom);
    BOOST_ASSERT_MSG( subject != nullptr, "Subject must be an object" );
    const LV2_Atom_Object* body = uris().toObject(bodyAtom);

    if (requestType == uris().patch_Insert) {
        if (subject->body.otype == uris().methcla_Target) {
            // get add target specification

            // get plugin URI
            const LV2_Atom* pluginAtom = nullptr;
            lv2_atom_object_get(body, uris().methcla_plugin, &pluginAtom);
            BOOST_ASSERT_MSG( pluginAtom != nullptr, "methcla:plugin property not found" );
            BOOST_ASSERT_MSG( pluginAtom->type == uris().atom_URID, "methcla:plugin property value must be a URID" );

            // get params from body

            // uris().methcla_plugin
            const std::shared_ptr<Plugin> def = plugins().lookup(reinterpret_cast<const LV2_Atom_URID*>(pluginAtom)->body);
            Synth* synth = Synth::construct(*this, rootNode(), Node::kAddToTail, *def);

            // send reply with synth ID (from NRT thread)
            // tja ...
        } else {
            std::cerr << "Insert: subject must be a Target\n";
        }
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
