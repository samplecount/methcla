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

//CommandServer::CommandServer(Environment& env)
//    : m_env(env)
//    , m_continue(true)
//    , m_toNrtFifo(8192)
//    , m_fromNrtFifo(8192)
//{
//    m_thread = boost::thread(&CommandServer::process, this);
//}
//
//CommandServer::~CommandServer()
//{
//    m_continue = false;
//    m_semaphore.post();
//    m_thread.join();
//}
//
//size_t CommandServer::execute(size_t maxNumMessages)
//{
//    Command* cmd;
//    size_t numMessages = 0;
//    while (numMessages < maxNumMessages && m_fromNrtFifo.dequeue(cmd)) {
//        cmd->execute(kRealtime);
//        numMessages++;
//    }
//    return numMessages;
//}
//
//void CommandServer::enqueue(Context context, Command* cmd)
//{
//    if (context == kRealtime) {
//        bool success = m_toNrtFifo.enqueue(cmd);
//        // TODO: Error handling
//        BOOST_ASSERT( success );
//        m_semaphore.post();
//    } else if (context == kNonRealtime) {
//        do {
//            /* SPIN */
//        } while (!m_fromNrtFifo.enqueue(cmd));
//    } else {
//        BOOST_ASSERT_MSG( false, "Invalid execution context" );
//    }
//}
//
//void CommandServer::process()
//{
//    cout << "NonRealtimeEngine::process() >>" << endl;
//    while (m_continue) {
//        m_semaphore.wait();
//        Command* cmd;
//        while (m_toNrtFifo.dequeue(cmd)) {
//            cout << "NonRealtimeEngine::process() " << cmd << endl;
//            cmd->execute(kNonRealtime);
//        }
//    }
//    cout << "<< NonRealtimeEngine::process()" << endl;    
//}
//
//CommandServer* CommandServer::Command::owner()
//{
//    return *reinterpret_cast<CommandServer**>((reinterpret_cast<char*>(this) - sizeof(CommandServer*)));
//}
//
//void* CommandServer::Command::alloc(CommandServer* owner, size_t size)
//{
//    CommandServer** ptr =
//        static_cast<CommandServer**>(
//            owner->env().rtMem().malloc(sizeof(CommandServer*) + size));
//    ptr[0] = owner;
//    return &ptr[1];
//}
//
//void CommandServer::Command::free(void* ptr)
//{
//    CommandServer** ptr_ = static_cast<CommandServer**>(ptr) - 1;
//    ptr_[0]->env().rtMem().free(ptr_);
//}

void LV2Command::perform(Context context)
{
    BOOST_ASSERT( context == kRealtime );
    LV2_Atom* atom = m_atom;
    cout << "Message: " << atom << endl
         << "    atom size: " << atom->size << endl
         << "    atom type: " << atom->type << endl;
    if (atom->type == env().uris().atom_String) {
        const char* str = (const char*)LV2_ATOM_BODY(atom);
        cout << "    string: " << str << endl;
    }
    env().free(context, this);
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
    , m_commandChannel(8192)
    , m_commandEngine(8192)
{
    lv2_atom_forge_init(&m_forge, pluginManager.lv2UridMap());
    m_uris.atom_String = pluginManager.uriMap().map(LV2_ATOM__String);

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
}

template <class T> class RealtimeCommand : public Command
                                         , public AllocatedBase<T, RTMemoryManager>
{
public:
    RealtimeCommand(Environment& env)
        : Command(env, kRealtime)
    { }
};


void Environment::sendMessage(LV2_Atom* atom)
{
    m_commandChannel.enqueue(new LV2Command(*this, atom));
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
