#include <Mescaline/Audio/Engine.hpp>
#include <Mescaline/Audio/Group.hpp>

#include <boost/foreach.hpp>
#include <cstdlib>
#include <iostream>

#include "lv2/lv2plug.in/ns/ext/patch/patch.h"
#include "lv2/lv2plug.in/ns/ext/atom/util.h"

using namespace Mescaline;
using namespace Mescaline::Audio;
using namespace Mescaline::Memory;
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

Environment::Environment(Plugin::Manager& pluginManager, const Options& options)
    : m_sampleRate(options.sampleRate)
    , m_blockSize(options.blockSize)
    , m_plugins(pluginManager)
    , m_audioBuses    (options.numHardwareInputChannels+options.numHardwareOutputChannels+options.maxNumAudioBuses)
    , m_freeAudioBuses(options.numHardwareInputChannels+options.numHardwareOutputChannels+options.maxNumAudioBuses)
    , m_nodes(options.maxNumNodes)
    , m_rootNode(Group::construct(*this, nullptr, Node::kAddToTail))
    , m_epoch(0)
    , m_worker(uriMap())
{
    m_uris.atom_Blank = mapUri(LV2_ATOM__Blank);
    m_uris.atom_Resource = mapUri(LV2_ATOM__Resource);
    m_uris.atom_Sequence = mapUri(LV2_ATOM__Sequence);
    m_uris.patch_Insert = mapUri(LV2_PATCH_PREFIX "Insert");
    m_uris.patch_subject = mapUri(LV2_PATCH__subject);
    m_uris.patch_body = mapUri(LV2_PATCH__body);

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

void Environment::request(const LV2_Atom* msg, const MessageQueue::Respond& respond, void* data)
{
    m_requests.send(msg, respond, data);
}

void Environment::process(size_t numFrames, sample_t** inputs, sample_t** outputs)
{
    BOOST_ASSERT_MSG( numFrames <= blockSize(), "numFrames exceeds blockSize()" );

    // Process external requests
    processRequests();

    // Process non-realtime commands
    m_worker.perform();

    size_t numInputs = m_audioInputChannels.size();
    size_t numOutputs = m_audioOutputChannels.size();

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
        handleRequest(msg);
    }
}

void Environment::handleRequest(MessageQueue::Message& request)
{
    const LV2_Atom* atom = request.payload();
    cout << "Message: " << atom << endl
         << "    atom size: " << atom->size << endl
         << "    atom type: " << atom->type << endl
         << "    atom uri:  " << unmapUri(atom->type) << endl;
    if (   (atom->type == uris().atom_Blank)
        || (atom->type == uris().atom_Resource))
        handleMessageRequest(request, reinterpret_cast<const LV2_Atom_Object*>(atom));
    else if (atom->type == uris().atom_Sequence)
        handleSequenceRequest(request, reinterpret_cast<const LV2_Atom_Sequence*>(atom));
    else
        BOOST_THROW_EXCEPTION(Exception() << ErrorInfoString("Invalid request type"));
}

void Environment::handleMessageRequest(MessageQueue::Message& request, const LV2_Atom_Object* msg)
{
    const char* atom_type = unmapUri(msg->atom.type);
    const char* uri_type = unmapUri(msg->body.otype);
    if (msg->atom.type == uris().atom_Blank) {
        cout << atom_type << " " << msg->body.id << " " << uri_type << endl;
    } else {
        const char* uri_id = unmapUri(msg->body.id);
        cout << atom_type << " " << uri_id << " " << uri_type << endl;
    }
    LV2_ATOM_OBJECT_FOREACH(msg, prop) {
        cout << "  " << unmapUri(prop->key) << " " << prop->context << ": " << unmapUri(prop->value.type) << endl;
    }
}

void Environment::handleSequenceRequest(MessageQueue::Message& request, const LV2_Atom_Sequence* bdl)
{
}

Engine::Engine(Plugin::Loader* pluginLoader)
    : m_pluginLoader(pluginLoader)
    , m_pluginManager(*pluginLoader)
    , m_env(0)
{
    m_pluginManager.loadPlugins();
}

Engine::~Engine()
{
    delete m_env;
    delete m_pluginLoader;
}

void Engine::configure(const IO::Driver& driver)
{
    delete m_env;
    Environment::Options options;
    options.sampleRate = driver.sampleRate();
    options.blockSize = driver.bufferSize();
    options.numHardwareInputChannels = driver.numInputs();
    options.numHardwareOutputChannels = driver.numOutputs();
    m_env = new Environment(m_pluginManager, options);
}

void Engine::process(size_t numFrames, sample_t** inputs, sample_t** outputs)
{
    if (m_env) m_env->process(numFrames, inputs, outputs);
}
