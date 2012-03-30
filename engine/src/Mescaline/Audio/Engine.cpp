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

APICommand::APICommand(Environment& env, const LV2_Atom* msg, const API::HandleResponse& handler, void* handlerData)
    : Command(env, kNonRealtime)
    , API::Request(msg, handler, handlerData)
{ }

APICommand::~APICommand()
{ }

void APICommand::perform(Context context)
{
    BOOST_ASSERT_MSG( context == kRealtime, "APICommand::perform should only be called in the RT context" );
    env().performRequest(this);
}

void APICommand::respond(Context context, const LV2_Atom* msg)
{
    BOOST_ASSERT_MSG( context == kNonRealtime, "APICommand::respond should only be called in the NRT context" );
    API::Request::respond(msg);
    env().free(context, this);
}

Environment::Environment(Plugin::Manager& pluginManager, const Options& options)
    : m_sampleRate(options.sampleRate)
    , m_blockSize(options.blockSize)
    , m_plugins(pluginManager)
    , m_rootNode(0)
    , m_epoch(0)
    , m_commandChannel(8192)
    , m_commandEngine(8192)
{
    lv2_atom_forge_init(&m_forge, pluginManager.lv2UridMap());
    m_uris.atom_Blank = mapUri(LV2_ATOM__Blank);
    m_uris.atom_Resource = mapUri(LV2_ATOM__Resource);
    m_uris.atom_Sequence = mapUri(LV2_ATOM__Sequence);
    m_uris.patch_Insert = mapUri(LV2_PATCH_PREFIX "Insert");
    m_uris.patch_subject = mapUri(LV2_PATCH__subject);
    m_uris.patch_body = mapUri(LV2_PATCH__body);

    m_rootNode = Group::construct(*this, nextResourceId(), 0);

    const Epoch prevEpoch = epoch() - 1;

    m_audioInputChannels.reserve(options.numHardwareInputChannels);
    for (size_t i=0; i < options.numHardwareInputChannels; i++) {
        ExternalAudioBus* bus = new ExternalAudioBus(*this, nextResourceId(), blockSize(), prevEpoch);
        addResource(*bus);
        m_audioInputChannels.push_back(bus);
    }

    m_audioOutputChannels.reserve(options.numHardwareOutputChannels);
    for (size_t i=0; i < options.numHardwareOutputChannels; i++) {
        ExternalAudioBus* bus = new ExternalAudioBus(*this, nextResourceId(), blockSize(), prevEpoch);
        addResource(*bus);
        m_audioOutputChannels.push_back(bus);
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


void Environment::request(const LV2_Atom* msg, const API::HandleResponse& handler, void* handlerData)
{
    // std::cout << "Environment::request " << msg << " " << handler << " " << handlerData << "\n";
    const size_t start = lv2_atom_pad_size(sizeof(APICommand));
    const size_t size = lv2_atom_total_size(msg);
    void* mem = alloc(start + size);
    LV2_Atom* atom = reinterpret_cast<LV2_Atom*>(static_cast<char*>(mem) + start);
    memcpy(atom, msg, size);
    APICommand* cmd = new (mem) APICommand(*this, atom, handler, handlerData);
    // APICommand* cmd = new APICommand(*this, msg, handler, handlerData);
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

void Environment::addResource(Resource& resource)
{
    m_resources.insert(resource);
}

void Environment::removeResource(Resource& resource)
{
    m_resources.remove(resource);
}

void Environment::performRequest(API::Request* request)
{
    const LV2_Atom* atom = request->request();
    cout << "Message: " << atom << endl
         << "    atom size: " << atom->size << endl
         << "    atom type: " << atom->type << endl
         << "    atom uri:  " << unmapUri(atom->type) << endl;
    if (   (atom->type == uris().atom_Blank)
        || (atom->type == uris().atom_Resource))
        performMessage(request, reinterpret_cast<const LV2_Atom_Object*>(atom));
    else if (atom->type == uris().atom_Sequence)
        performBundle(request, reinterpret_cast<const LV2_Atom_Sequence*>(atom));
    else
        BOOST_THROW_EXCEPTION(Exception() << ErrorInfoString("Invalid request type"));
}

void Environment::performMessage(API::Request* request, const LV2_Atom_Object* msg)
{
    const char* atom_type = unmapUri(msg->atom.type);
    const char* uri_type = unmapUri(msg->body.otype);
    if (msg->atom.type == uris().atom_Blank) {
        cout << atom_type << " " << msg->body.id << " " << uri_type << endl;
    } else {
        const char* uri_id = unmapUri(msg->body.id);
        cout << atom_type << " " << uri_id << " " << uri_type << endl;
    }
    LV2_OBJECT_FOREACH(msg, prop) {
        cout << "  " << unmapUri(prop->key) << " " << prop->context << ": " << unmapUri(prop->value.type) << endl;
    }
}

void Environment::performBundle(API::Request* request, const LV2_Atom_Sequence* bdl)
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
