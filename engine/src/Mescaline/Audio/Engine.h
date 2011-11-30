#ifndef MESCALINE_AUDIO_ENGINE_H_INCLUDED
#define MESCALINE_AUDIO_ENGINE_H_INCLUDED

#include <boost/container/vector.hpp>
#include <boost/intrusive/list.hpp>
#include <boost/cstdint.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/thread/thread.hpp>
#include <boost/utility.hpp>
#include <Mescaline/Exception.hpp>
#include <Mescaline/Audio/IO/Client.h>
#include <list> // use boost invasive containers
#include <vector>

using namespace boost::container;

namespace Mescaline { namespace Audio
{
    typedef float sample_t;
    typedef int32_t NodeId;
    typedef int32_t BusId;
    typedef int32_t BufferId;
    
    class Engine : public IO::Client
    {
    public:
        Engine(double sampleRate);
        virtual void process(size_t numFrames, sample_t* buffers);
    };
    
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

    private:
        Nodes m_nodes;
    };

    class MemoryManager
    {
    public:
        class Alignment
        {
        public:
            Alignment(size_t alignment)
                : m_alignment(alignment)
            { }
            size_t alignment() const { return m_alignment; }
        private:
            size_t m_alignment;
        };

        virtual void* malloc(size_t numBytes) throw(MemoryAllocationFailure) = 0;
        virtual void* memalign(const Alignment& alignment, size_t numBytes) throw(MemoryAllocationFailure) = 0;
        virtual void free(void* ptr) throw(MemoryAllocationFailure) = 0;
        
        template <typename T> T* alloc(size_t numElems=1) throw(MemoryAllocationFailure)
            { return static_cast<T*>(malloc(numElems * sizeof(T))); }
        template <typename T> T* allocAligned(const Alignment& alignment, size_t numElems=1) throw(MemoryAllocationFailure)
            { return static_cast<T*>(memalign(alignment, numElems * sizeof(T))); }
    };

    class NRTMemoryManager : public MemoryManager
    {
    public:
        virtual void* malloc(size_t numBytes) throw(MemoryAllocationFailure);
        virtual void* memalign(const Alignment& alignment, size_t numBytes) throw(MemoryAllocationFailure);
        virtual void free(void* ptr) throw(MemoryAllocationFailure);
    };

    class AudioBus : boost::noncopyable
    {
    public:
        AudioBus(MemoryManager& mem, size_t numFrames, uint32_t writeCount);
        sample_t* data() { return m_data; }

        void lockForWriting() { }
        void unlockForWriting() { }
        void lockForReading() { }
        void unlockForReading() { }

    private:
        uint32_t    m_writeCount;
        sample_t*   m_data;
    };
    
    class Environment
    {
    public:
        struct Options
        {
            Options()
                : maxNumNodes(512)
                , maxNumAudioBuses(128)
                , maxNumControlBuses(128)
                , blockSize(64)
            { }

            size_t maxNumNodes;
            size_t maxNumAudioBuses;
            size_t maxNumControlBuses;
            size_t blockSize;
        };

        Environment(const Options& options);

        Node* rootNode() { return m_rootNode; }
        const NodeMap& nodes() const { return m_nodes; }

        size_t blockSize() const { return m_blockSize; }
        AudioBus& audioBus(size_t index) { return *m_audioBuses.at(index); }

        MemoryManager& rtMem() { return m_rtMem; }
        MemoryManager& nrtMem() { return m_nrtMem; }

    private:
        NRTMemoryManager    m_rtMem;
        NRTMemoryManager    m_nrtMem;
        Node*               m_rootNode;
        NodeMap             m_nodes;
        size_t              m_blockSize;
        vector<AudioBus*>   m_audioBuses;
    };
    
    class Node : public boost::intrusive::list_base_hook<>
    {
    public:
        Node(Environment& env, const NodeId& id)
            : m_env(env)
            , m_id(id)
        { }

        /// Return environment.
        const Environment& environment() const { return m_env; }
        Environment& environment() { return m_env; }
        
        /// Return the node's id.
        const NodeId& id() const { return m_id; }

        // Process a number of frames.
        virtual void process(size_t numFrames) = 0;

    protected:
        void* operator new(size_t numBytes, Environment& env);
        void operator delete(void* ptr, Environment& env);
        template <class T> static void free(T* node);
        friend class NodeMap;
        virtual void free() = 0;

    private:
        Environment&    m_env;
        NodeId          m_id;
    };
    
    typedef boost::intrusive::list<Node> NodeList;
    
    class Group : public Node
    {
    public:
        Group(Environment& env, const NodeId& id)
            : Node(env, id)
        { }

        static Group* construct(Environment& env, const NodeId& id);

        const NodeList& children() const { return m_children; }

        void addToHead(Node& node) { m_children.push_front(node); }
        void addToTail(Node& node) { m_children.push_back(node); }

        virtual void process(size_t numFrames);

    protected:
        virtual void free();

    private:
        NodeList m_children;
    };

    // TODO: Use intrusive_ptr!
    // typedef boost::shared_ptr<Bus> BusRef;
    
    template <class T> class Connection
    {
    public:
        enum Type
        {
            kIn
          , kInFeedback
          , kOut
          , kOffsetOut
          , kReplaceOut
        };
        
        Type type() const { return m_type; }
        T& bus() { return m_bus; }
        
    private:
        Type m_type;
        T&   m_bus;
    };

    class Synth : public Node
    {
    public:
        Synth(Environment& env, const NodeId& id, size_t numInputs, size_t numOutputs)
            : Node(env, id)
            , m_numInputs(numInputs)
            , m_numOutputs(numOutputs)
        { }

        /// Return number of inputs.
        size_t numInputs() const { return m_numInputs; }
        /// Get *external* input bus.
        AudioBus& getInput(size_t input);
        /// Map input to bus.
        void setInput(size_t input, AudioBus& bus, InputConnection<AudioBus>::Type type);

        // Return number of outputs.
        size_t numOutputs() const { return m_numOutputs; }
        /// Get *external* output bus.
        AudioBus& getOutput(size_t output);
        // Map output to bus.
        void setOutput(size_t output, AudioBus& bus, OutputConnection<AudioBus>::Type type);

        typedef boost::container::vector<Connection<AudioBus> > AudioBusConnections;
        
        virtual void process(size_t numFrames);
        virtual void compute(size_t numFrames, sample_t** inputs, sample_t** outputs);

    private:
        size_t              m_numInputs;
        size_t              m_numOutputs;
        InputConnections    m_inputConnections;
        OutputConnections   m_outputConnections;
        sample_t**          m_buffers;
    };
    
    class FaustSynth : public Synth
    {
    };
}; };

#endif // MESCALINE_AUDIO_ENGINE_H_INCLUDED
