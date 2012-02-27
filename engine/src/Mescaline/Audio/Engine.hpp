#ifndef MESCALINE_AUDIO_ENGINE_H_INCLUDED
#define MESCALINE_AUDIO_ENGINE_H_INCLUDED

#include <Mescaline/Audio.hpp>
#include <Mescaline/Audio/AudioBus.hpp>
#include <Mescaline/Audio/IO/Client.hpp>
#include <Mescaline/Audio/Node.hpp>
#include <Mescaline/Audio/Plugin/API.h>
#include <Mescaline/Audio/SynthDef.hpp>
#include <Mescaline/Exception.hpp>
#include <Mescaline/Memory/Manager.hpp>

#include <boost/container/vector.hpp>
#include <boost/cstdint.hpp>
#include <boost/lockfree/fifo.hpp>
#include <boost/lockfree/ringbuffer.hpp>
#include <boost/ptr_container/ptr_vector.hpp>
#include <boost/thread/mutex.hpp>
#include <boost/thread/thread.hpp>
#include <boost/utility.hpp>

#include <oscpp/server.hpp>

#include <bitset>
#include <string>
#include <vector>

#include <lv2/lv2plug.in/ns/ext/atom/atom.h>
#include <lv2/lv2plug.in/ns/ext/atom/forge.h>
#include <lv2/lv2plug.in/ns/ext/atom/util.h>

namespace Mescaline { namespace Audio
{
    using namespace boost::container;

    using namespace Memory;

    // typedef int32_t ControlBusId;

    // class ControlBusId
    // {
    // public:
    //     ControlBusId()
    //         : m_id(-1)
    //     { }
    //     ControlBusId(
    //     uint32_t id() const { return m_id; }
    //     operator bool () const { return m_id != -1; }
    // 
    // private:
    //     uint32_t m_id;
    // };

    // class ControlBus : boost::noncopyable
    // {
    // public:
    //     typedef sample_t ValueType;
    // 
    // private:
    //     BusId       m_id;
    //     Epoch       m_epoch;
    //     ValueType   m_data;
    // };

    struct EngineException : virtual Mescaline::Exception { };
    struct InvalidNodeId : virtual EngineException { };
    struct DuplicateNodeId : virtual EngineException { };
    struct ErrorInfoNodeIdTag {};
    typedef boost::error_info<ErrorInfoNodeIdTag, NodeId> ErrorInfoNodeId;

    class Node;

    class NodeMap
    {
        typedef std::vector<Node*> Nodes;

    public:
        typedef Nodes::const_reference const_reference;

        NodeMap(size_t maxNumNodes)
            : m_nodes(maxNumNodes, 0)
        { }

        void insert(Node* node);
        const_reference lookup(const NodeId& nodeId) const { return m_nodes.at(nodeId); }
        void release(const NodeId& nodeId);

    private:
        Nodes m_nodes;
    };

    class Environment;

    class Message
    {
    public:
        static const size_t kMaxPayloadBytes = 128;

        typedef void (*Perform)(void* context, Environment& env, Message& message);

        Message()
            : m_perform(0)
            , m_context(0)
            , m_atomPtr(0)
        {
            memset(&m_atom, 0, sizeof(m_atom));
#ifndef NDEBUG
            memset(m_payload, 0, kMaxPayloadBytes);
#endif
        }

        Message(Perform perform, void* context)
            : m_perform(perform)
            , m_context(context)
            , m_atomPtr(0)
        {
            memset(&m_atom, 0, sizeof(m_atom));
#ifndef NDEBUG
            memset(m_payload, 0, kMaxPayloadBytes);
#endif
        }
        Message(Perform perform, void* context, LV2_Atom* atom)
            : m_perform(perform)
            , m_context(context)
            , m_atomPtr(atom)
        {
#ifndef NDEBUG
            memset(&m_atom, 0, sizeof(m_atom));
            memset(m_payload, 0, kMaxPayloadBytes);
#endif
        }
        Message(const Message& other)
        {
            m_perform = other.m_perform;
            m_context = other.m_context;
            if (other.m_atomPtr != 0) {
                m_atomPtr = other.m_atomPtr;
            } else {
                memcpy(&m_atom, &other.m_atom, lv2_atom_pad_size(lv2_atom_total_size(&m_atom)));
            }
        }

        void perform(Environment& env)
        {
            if (m_perform != 0) m_perform(m_context, env, *this);
        }

        LV2_Atom* atom()
        {
            return m_atomPtr != 0 ? m_atomPtr : &m_atom;
        }

        bool needsFree() const
        {
            return m_atomPtr != 0;
        }

    private:
        Perform     m_perform;
        void*       m_context;
        LV2_Atom*   m_atomPtr;
        LV2_Atom    m_atom;
        uint8_t     m_payload[kMaxPayloadBytes];
    };

    typedef boost::lockfree::ringbuffer<Message,0> MessageFIFO;

    class NonRealtimeEngine
    {
    public:
        NonRealtimeEngine(Environment& env);

        bool send(const Message& msg);
        size_t perform(size_t maxNumMessages);

    private:
        
    private:
        Environment&    m_env;
        MessageFIFO     m_toNrtFifo;
        MessageFIFO     m_fromNrtFifo;
        boost::mutex    m_nrtSendMutex;
        boost::mutex    m_nrtRecvMutex;
    };

    class MessageQueue
    {
    public:
        MessageQueue(size_t size);
    
        void send(const Message& msg);
        size_t perform(Environment& env);

    private:
        MessageFIFO     m_fifo;
        boost::mutex    m_mutex;
    };

    class Group;
    class PluginInterface;

    class Environment : public boost::noncopyable
    {
    public:
        struct Options
        {
            Options()
                : maxNumNodes(1024)
                , maxNumAudioBuses(128)
                , maxNumControlBuses(4096)
                , sampleRate(44100)
                , blockSize(64)
                , numHardwareInputChannels(2)
                , numHardwareOutputChannels(2)
            { }

            size_t maxNumNodes;
            size_t maxNumAudioBuses;
            size_t maxNumControlBuses;
            size_t sampleRate;
            size_t blockSize;
            size_t numHardwareInputChannels;
            size_t numHardwareOutputChannels;
        };

        Environment(Plugin::Manager& pluginManager, const Options& options);
        ~Environment();

        const Plugin::Manager::PluginHandle& lookupSynthDef(const char* name) { return m_synthDefs.lookup(name); }
        void registerSynthDef(SynthDef* synthDef) { /* m_synthDefs.insert(synthDef); */ }
        
        Group* rootNode() { return m_rootNode; }
        const NodeMap& nodes() const { return m_nodes; }

        size_t sampleRate() const { return m_sampleRate; }
        size_t blockSize() const { return m_blockSize; }

        AudioBus& audioBus(AudioBusId busId)
        {
            switch (busId.scope()) {
                case AudioBusId::kInput:
                    return m_audioInputChannels[busId.id()];
                case AudioBusId::kOutput:
                    return m_audioOutputChannels[busId.id()];
                case AudioBusId::kInternal:
                    return m_audioBuses[busId.id()];
            }
            BOOST_THROW_EXCEPTION(InvalidInput());
        }

        RTMemoryManager& rtMem() { return m_rtMem; }

        const Epoch& epoch() const { return m_epoch; }

        void process(size_t numFrames, sample_t** inputs, sample_t** outputs);

        const LV2_Atom_Forge& atomForge() const { return m_forge; }
        void sendMessage(LV2_Atom* msg);

    protected:
        friend class Node;

        void insertNode(Node* node);
        void releaseNodeId(const NodeId& nodeId);

    private:
        const size_t                m_sampleRate;
        const size_t                m_blockSize;
        PluginInterface*            m_pluginInterface;
        RTMemoryManager             m_rtMem;
//        SynthDefMap                 m_synthDefs;
        Plugin::Manager&            m_synthDefs;
        Group*                      m_rootNode;
        NodeMap                     m_nodes;
        boost::ptr_vector<ExternalAudioBus> m_audioInputChannels;
        boost::ptr_vector<ExternalAudioBus> m_audioOutputChannels;
        boost::ptr_vector<InternalAudioBus> m_audioBuses;
        Epoch                       m_epoch;
        MessageQueue                m_msgQueue;
    };
    
    // This is the interface that plugins use (via its function pointers,
    // inherited from MescalineHost).    
    class PluginInterface : public MescalineHost
    {
    public:
        PluginInterface(Environment& env)
            : m_env(env)
        {
            fGetSampleRate = &GetSampleRate;
            fRegisterSynthDef = &RegisterSynthDef;
        }

    private:
        static unsigned int GetSampleRate(const MescalineHost* self)
            { return static_cast<const PluginInterface*>(self)->m_env.sampleRate(); }
        static void RegisterSynthDef(MescalineHost* self, MescalineSynthDef* synthDef)
            { static_cast<PluginInterface*>(self)->m_env.registerSynthDef(new SynthDef(self, synthDef)); }

    private:
        Environment& m_env;
    };
    
    class Engine : public IO::Client
    {
    public:
        Engine(Plugin::Loader* pluginLoader);
        virtual ~Engine();

        virtual void configure(const IO::Driver& driver);
        virtual void process(size_t numFrames, sample_t** inputs, sample_t** outputs);
    
        Environment& environment()
        {
            BOOST_ASSERT( m_env != 0 );
            return *m_env;
        }
        const Environment& environment() const
        {
            BOOST_ASSERT( m_env != 0 );
            return *m_env;
        }

    private:
        Plugin::Loader*     m_pluginLoader;
        Plugin::Manager     m_pluginManager;
        Environment*        m_env;
    };
}; };

#endif // MESCALINE_AUDIO_ENGINE_H_INCLUDED
