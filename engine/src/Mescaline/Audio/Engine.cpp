#include <Mescaline/Audio/Engine.hpp>
#include <Mescaline/Audio/Group.hpp>
#include <Mescaline/Audio/SynthDef.hpp>
#include <boost/foreach.hpp>
#include <cstdlib>
#include <oscpp/client.hpp>
#include <oscpp/server.hpp>

using namespace Mescaline;
using namespace Mescaline::Audio;
using namespace Mescaline::Memory;

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

NonRealtimeEngine::NonRealtimeEngine(Environment& env)
    : m_env(env)
    , m_toNrtFifo(8192)
    , m_fromNrtFifo(8192)
{ }

bool NonRealtimeEngine::send(const Message& msg)
{
    return m_toNrtFifo.enqueue(msg);
}

size_t NonRealtimeEngine::perform(size_t maxNumMessages)
{
    Message msg;
    size_t numMessages = 0;
    while (numMessages < maxNumMessages && m_fromNrtFifo.dequeue(msg)) {
        msg.perform(m_env);
        numMessages++;
    }
    return numMessages;
}

//    mutex::scoped_lock(m_nrtMutex);
//    do {
//        /* SPIN */
//    } while (!m_nrtFifo.enqueue(msg));

MessageQueue::MessageQueue(size_t size)
    : m_fifo(size)
{ }

void MessageQueue::send(const Message& msg)
{
    boost::mutex::scoped_lock(m_mutex);
    do {
        /* SPIN */
    } while (!m_fifo.enqueue(msg));
}

size_t MessageQueue::perform(Environment& env)
{
    Message msg;
    size_t n = 0;
    while (m_fifo.dequeue(msg)) {
        msg.perform(env);
        n++;
    }
    return n;
}

Environment::Environment(Plugin::Manager& pluginManager, const Options& options)
    : m_sampleRate(options.sampleRate)
    , m_blockSize(options.blockSize)
    , m_synthDefs(pluginManager)
    , m_rootNode(0)
    , m_nodes(options.maxNumNodes)
    , m_audioBuses(options.maxNumAudioBuses)
    , m_audioInputChannels(options.numHardwareInputChannels)
    , m_audioOutputChannels(options.numHardwareOutputChannels)
    , m_epoch(0)
    , m_msgQueue(8192)
{
    lv2_atom_forge_init(&m_forge, pluginManager.lv2UridMap());
    m_uris.atom_String = pluginManager.uriMap().map(LV2_ATOM__String);

    m_pluginInterface = new PluginInterface(*this);
    m_rootNode = Group::construct(*this, 0, 0);

    const Epoch prevEpoch = epoch() - 1;

    for (size_t i=0; i < options.maxNumAudioBuses; i++) {
        m_audioBuses.push_back(new InternalAudioBus(blockSize(), prevEpoch));
    }
    for (size_t i=0; i < options.numHardwareInputChannels; i++) {
        m_audioInputChannels.push_back(new ExternalAudioBus(blockSize(), prevEpoch));
    }
    for (size_t i=0; i < options.numHardwareOutputChannels; i++) {
        m_audioOutputChannels.push_back(new ExternalAudioBus(blockSize(), prevEpoch));
    }
}

Environment::~Environment()
{
    delete m_pluginInterface;
}

{
void Environment::sendMessage(LV2_Atom* atom)
{
    m_msgQueue.send(Message(Environment_performMessage, this, atom));
}

void Environment::process(size_t numFrames, sample_t** inputs, sample_t** outputs)
{
    BOOST_ASSERT_MSG( numFrames <= blockSize(), "numFrames exceeds blockSize()" );

    m_msgQueue.perform(*this);

    size_t numInputs = m_audioInputChannels.size();
    size_t numOutputs = m_audioOutputChannels.size();

    // Connect input and output buses
    for (size_t i=0; i < numInputs; i++) {
        m_audioInputChannels[i].setData(inputs[i]);
        m_audioInputChannels[i].setEpoch(epoch());
    }
    for (size_t i=0; i < numOutputs; i++) {
        m_audioOutputChannels[i].setData(outputs[i]);
    }
    
    // Run DSP graph
    m_rootNode->process(numFrames);
    
    // Zero outputs that haven't been written to
    for (size_t i=0; i < numOutputs; i++) {
        if (m_audioOutputChannels[i].epoch() != epoch()) {
            memset(outputs[i], 0, numFrames * sizeof(sample_t));
        }
    }

    m_epoch++;
}

void Environment::insertNode(Node* node)
{
    m_nodes.insert(node);
}

void Environment::releaseNodeId(const NodeId& nodeId)
{
    m_nodes.release(nodeId);
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
