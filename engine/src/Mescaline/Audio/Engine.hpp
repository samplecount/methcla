#ifndef MESCALINE_AUDIO_ENGINE_H_INCLUDED
#define MESCALINE_AUDIO_ENGINE_H_INCLUDED

#include <Mescaline/Audio.hpp>
#include <Mescaline/Audio/AudioBus.hpp>
// #include <Mescaline/Audio/API.hpp>
#include <Mescaline/Audio/IO/Client.hpp>
#include <Mescaline/Audio/Node.hpp>
#include <Mescaline/Audio/SynthDef.hpp>
#include <Mescaline/Exception.hpp>
#include <Mescaline/Memory/Manager.hpp>
#include <Mescaline/Utility/MessageQueue.hpp>

#include <boost/unordered_map.hpp>
#include <boost/utility.hpp>

#include <cstddef>
#include <vector>

#include "lv2/lv2plug.in/ns/ext/atom/atom.h"
#include "lv2/lv2plug.in/ns/ext/atom/forge.h"

namespace Mescaline { namespace Audio
{
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

    using std::size_t;

    struct EngineException : virtual Mescaline::Exception { };
    struct InvalidNodeId : virtual EngineException { };
    struct DuplicateNodeId : virtual EngineException { };
    struct ErrorInfoNodeIdTag { };
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

        const Plugin::Manager& plugins() const { return m_plugins; }
        Plugin::Manager& plugins() { return m_plugins; }

        Group* rootNode() { return m_rootNode; }

        size_t sampleRate() const { return m_sampleRate; }
        size_t blockSize() const { return m_blockSize; }

        //* Return audio bus with id.
        AudioBus* audioBus(const AudioBusId& id);
        //* Return external audio output bus at index.
        AudioBus& externalAudioOutput(size_t index);
        //* Return external audio input bus at index.
        AudioBus& externalAudioInput(size_t index);

        Memory::RTMemoryManager& rtMem() { return m_rtMem; }

        const Epoch& epoch() const { return m_epoch; }

        void process(size_t numFrames, sample_t** inputs, sample_t** outputs);

        // URIs and messages
        const LV2::URIDMap& uriMap() const { return plugins().uriMap(); }
        LV2::URIDMap& uriMap() { return plugins().uriMap(); }

        LV2_URID mapUri(const char* uri) { return uriMap().map(uri); }
        const char* unmapUri(LV2_URID urid) const { return uriMap().unmap(urid); }

        struct Uris
        {
            // atom
            LV2_URID atom_Blank;
            LV2_URID atom_Resource;
            LV2_URID atom_Sequence;
            // patch
            LV2_URID patch_Insert;
            LV2_URID patch_subject;
            LV2_URID patch_body;
        };

        const Uris& uris() const { return m_uris; }

        // Request queue
        typedef Utility::MessageQueue<8192,8192> MessageQueue;
        void request(const LV2_Atom* msg, const MessageQueue::Respond& respond, void* data);

        // Worker thread
        typedef Utility::WorkerThread<8192,8192> Worker;

        LV2_Atom_Forge* prepare(const Worker::Perform& perform, void* data)
        {
            return m_worker.toWorker().prepare(perform, data);
        }
        void commit()
        {
            m_worker.toWorker().commit();
        }

    protected:
        void processRequests();
        void handleRequest(MessageQueue::Message& request);
        void handleMessageRequest(MessageQueue::Message& request, const LV2_Atom_Object* msg);
        void handleSequenceRequest(MessageQueue::Message& request, const LV2_Atom_Sequence* bdl);

    protected:
        friend class Node;
        ResourceMap<NodeId,Node>& nodes() { return m_nodes; }

    private:
        const size_t                        m_sampleRate;
        const size_t                        m_blockSize;
        Memory::RTMemoryManager             m_rtMem;
        Plugin::Manager&                    m_plugins;
        ResourceMap<AudioBusId,AudioBus>    m_audioBuses;
        ResourceMap<AudioBusId,AudioBus>    m_freeAudioBuses;
        ResourceMap<NodeId,Node>            m_nodes;
        Group*                              m_rootNode;
        std::vector<ExternalAudioBus*>      m_audioInputChannels;
        std::vector<ExternalAudioBus*>      m_audioOutputChannels;
        Epoch                               m_epoch;
        MessageQueue                        m_requests;
        Worker                              m_worker;
        Uris                                m_uris;
    };

    class Engine : public IO::Client
    {
    public:
        Engine(Plugin::Loader* pluginLoader);
        virtual ~Engine();

        virtual void configure(const IO::Driver& driver);
        virtual void process(size_t numFrames, sample_t** inputs, sample_t** outputs);
    
        Environment& env()
        {
            BOOST_ASSERT( m_env != 0 );
            return *m_env;
        }
        const Environment& env() const
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
