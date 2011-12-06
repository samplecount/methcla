#include <Mescaline/Audio/Engine.hpp>
#include <Mescaline/Audio/Group.hpp>
#include <Mescaline/Audio/SynthDef.hpp>
#include <boost/foreach.hpp>
#include <cstdlib>
#include <oscpp/client.hpp>
#include <oscpp/server.hpp>

using namespace Mescaline::Audio;
using namespace Mescaline::Memory;

void NodeMap::insert(Node* node)
{
    NodeId id = node->id();
    if (m_nodes[id] != 0)
        m_nodes[id]->free();
    m_nodes[id] = node;
}

Environment::Environment(const Options& options)
    : m_sampleRate(options.sampleRate)
    , m_blockSize(options.blockSize)
    , m_rootNode(0)
    , m_nodes(options.maxNumNodes)
    , m_audioBuses(options.maxNumAudioBuses)
    , m_audioInputChannels(options.numHardwareInputChannels)
    , m_audioOutputChannels(options.numHardwareOutputChannels)
    , m_epoch(0)
{
    m_pluginInterface = new PluginInterface(*this);
    m_rootNode = Group::construct(*this, 0, 0);
    m_nodes.insert(m_rootNode);

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

void Environment::process(size_t numFrames, sample_t** inputs, sample_t** outputs)
{
    BOOST_ASSERT(numFrames <= blockSize());

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

MescalineHost* Environment::pluginInterface()
{
	return m_pluginInterface;
}

Engine::Engine()
    : m_env(0)
{ }

void Engine::configure(const IO::Driver& driver)
{
    if (m_env != 0) {
        delete m_env;
    }
    Environment::Options options;
    options.sampleRate = driver.sampleRate();
    options.blockSize = driver.bufferSize();
    options.numHardwareInputChannels = driver.numInputs();
    options.numHardwareOutputChannels = driver.numOutputs();
    m_env = new Environment(options);
}

void Engine::process(size_t numFrames, sample_t** inputs, sample_t** outputs)
{
    if (m_env) m_env->process(numFrames, inputs, outputs);
}
