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
#include <boost/thread/thread.hpp>
#include <boost/utility.hpp>

#include <oscpp/server.hpp>

#include <boost/asio.hpp>

#include <bitset>
#include <string>
#include <vector>

using namespace boost::container;

// using boost::asio::ip::udp;
// 
// enum { max_length = 1024 };
// 
// void server(boost::asio::io_service& io_service, short port)
// {
//   udp::socket sock(io_service, udp::endpoint(udp::v4(), port));
//   for (;;)
//   {
//     char data[max_length];
//     udp::endpoint sender_endpoint;
//     size_t length = sock.receive_from(
//         boost::asio::buffer(data, max_length), sender_endpoint);
//     sock.send_to(boost::asio::buffer(data, length), sender_endpoint);
//   }
// }
// 
// int main(int argc, char* argv[])
// {
//   try
//   {
//     if (argc != 2)
//     {
//       std::cerr << "Usage: blocking_udp_echo_server <port>\n";
//       return 1;
//     }
// 
//     boost::asio::io_service io_service;
// 
//     using namespace std; // For atoi.
//     server(io_service, atoi(argv[1]));
//   }
//   catch (std::exception& e)
//   {
//     std::cerr << "Exception: " << e.what() << "\n";
//   }
// 
//   return 0;
// }

namespace Mescaline { namespace Audio
{
    using namespace Memory;

    class OSCPacket
    {
    public:
        OSCPacket(char data[], size_t size)
            : m_packet(data, size)
            , m_data(data)
        { }
        ~OSCPacket()
        {
            delete [] m_data;
        }

        OSC::ServerPacket   m_packet;
        char*               m_data;
    };

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

    class Group;
    class PluginInterface;

//    class Message
//    {
//    public:
//        static const size_t kMaxPayloadBytes = 128;
//
//        typedef bool (*PerformFunc)(void*);
//
//        Message()
//            : m_perform(0)
//        {
//#ifndef NDEBUG
//            memset(m_payload, 0, kMaxPayloadBytes);
//#endif
//        }
//        Message(const Message& other)
//        {
//            m_perform = other.m_perform;
//            memcpy(m_payload, other.m_payload, kMaxPayloadBytes);
//        }
//
//        char* payload() { return m_payload.bytes; }
//
//    private:
//        bool (*m_perform)(void*);
//        char m_payload[kMaxPayloadBytes];
//    };

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

        void initModule(MescalineInitFunc moduleInitFunc);

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

        typedef boost::lockfree::ringbuffer<OSCPacket*, 1024> CommandFifo;
//        typedef boost::lockfree::fifo<Message> MessageFifo;

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
//        CommandFifo                 m_commandFifo;
//        MessageFifo                 m_nrtToRtFifo;
//        MessageFifo                 m_rtToNrtFifo;
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
    
    protected:
        Environment* environment() { return m_env; }

    private:
        Plugin::Loader*     m_pluginLoader;
        Plugin::Manager     m_pluginManager;
        Environment*        m_env;
    };
}; };

#endif // MESCALINE_AUDIO_ENGINE_H_INCLUDED
