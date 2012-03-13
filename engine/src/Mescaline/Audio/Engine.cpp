#include <Mescaline/Audio/Engine.hpp>
#include <Mescaline/Audio/Group.hpp>

#include <boost/foreach.hpp>
#include <cstdlib>

#include "lv2/lv2plug.in/ns/ext/atom/util.h"

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

APICommand::APICommand(Environment& env, LV2_Atom* atom, const API::ResponseHandler& handler)
    : Command(env, kNonRealtime)
    , API::Request(atom, handler)
{ }

void APICommand::perform(Context context)
{
    BOOST_ASSERT( context == kRealtime );
    const LV2_Atom* atom = request();
    cout << "Message: " << atom << endl
         << "    atom size: " << atom->size << endl
         << "    atom type: " << atom->type << endl;
    if (atom->type == env().uris().atom_String) {
        const char* str = (const char*)LV2_ATOM_BODY(atom);
        cout << "    string: " << str << endl;
    }
    env().free(context, this);
}

// void APICommand::respond(Context context, const LV2_Atom* atom)
// {
// 	
// }

Environment::Environment(Plugin::Manager& pluginManager, const Options& options)
    : m_sampleRate(options.sampleRate)
    , m_blockSize(options.blockSize)
    , m_plugins(pluginManager)
    , m_rootNode(0)
//    , m_nodes(options.maxNumNodes)
    , m_audioBuses(options.maxNumAudioBuses)
    , m_audioInputChannels(options.numHardwareInputChannels)
    , m_audioOutputChannels(options.numHardwareOutputChannels)
    , m_epoch(0)
    , m_commandChannel(8192)
    , m_commandEngine(8192)
{
    lv2_atom_forge_init(&m_forge, pluginManager.lv2UridMap());
    m_uris.atom_String = pluginManager.uriMap().map(LV2_ATOM__String);

    m_rootNode = Group::construct(*this, nextResourceId(), 0);

    const Epoch prevEpoch = epoch() - 1;

    for (size_t i=0; i < options.numHardwareInputChannels; i++) {
        ExternalAudioBus* bus = new ExternalAudioBus(*this, nextResourceId(), blockSize(), prevEpoch);
        m_audioInputChannels.push_back(bus);
        addResource(*bus);
    }
    for (size_t i=0; i < options.numHardwareOutputChannels; i++) {
        ExternalAudioBus* bus = new ExternalAudioBus(*this, nextResourceId(), blockSize(), prevEpoch);
        m_audioOutputChannels.push_back(bus);
        addResource(*bus);
    }
}

Environment::~Environment()
{
}

template <class T> class RealtimeCommand : public Command
                                         , public AllocatedBase<T, RTMemoryManager>
{
public:
    RealtimeCommand(Environment& env)
        : Command(env, kRealtime)
    { }
};


void Environment::sendRequest(const LV2_Atom* atom, const API::ResponseHandler& handler)
{
	const size_t atomStart = lv2_atom_pad_size(sizeof(APICommand));
	const size_t atomSize = lv2_atom_total_size(atom);
	void* mem = alloc(atomStart + atomSize);
	LV2_Atom* atomCopy = reinterpret_cast<LV2_Atom*>(static_cast<char*>(mem) + atomStart);
	memcpy(atomCopy, atom, atomSize);
	APICommand* cmd = new (mem) APICommand(*this, atomCopy, handler);
    m_commandChannel.enqueue(cmd);
}

void Environment::enqueue(Context context, Command* cmd)
{
    m_commandEngine.enqueue(context, cmd);
}

void Environment::free(Context context, Command* cmd)
{
    m_commandEngine.free(context, cmd);
}

void Environment::process(size_t numFrames, sample_t** inputs, sample_t** outputs)
{
    BOOST_ASSERT_MSG( numFrames <= blockSize(), "numFrames exceeds blockSize()" );

    // Process external and non-realtime commands
    m_commandChannel.perform();
    m_commandEngine.perform(kRealtime);

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

void Environment::addResource(Resource& resource)
{
    m_resources.insert(resource);
}

void Environment::removeResource(Resource& resource)
{
    m_resources.remove(resource);
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
