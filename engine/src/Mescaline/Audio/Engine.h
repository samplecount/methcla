#ifndef MESCALINE_AUDIO_ENGINE_H_INCLUDED
#define MESCALINE_AUDIO_ENGINE_H_INCLUDED

#include <boost/container/vector.hpp>
#include <boost/intrusive/list.hpp>
#include <boost/cstdint.hpp>
#include <boost/shared_ptr.hpp>
#include <list> // use boost invasive containers
#include <Mescaline/Audio/IO/Client.h>

namespace Mescaline { namespace Audio
{
    typedef float sample_t;
    
    class Engine : public IO::Client
    {
    public:
        Engine(double sampleRate);
        virtual void process(size_t numFrames, sample_t* buffers);
    };
    
    class Environment
    {
        // Memory allocator(s)
        // Node map
        // Root node
    };
    
    typedef int32_t NodeId;
    typedef int32_t BusId;
    typedef int32_t BufferId;

    class Bus
    {
    public:
        void lock() {}
        void unlock() {}
    };
    
    // TODO: Use intrusive_ptr!
    typedef boost::shared_ptr<Bus> BusRef;
    
    class InputConnection
    {
    public:
        enum Type
        {
            kIn
          , kInFeedback
        };
        
    private:
        BusRef  m_bus;
    };

    class OutputConnection
    {
    public:
        enum Type
        {
            kOut
          , kOffsetOut
          , kReplaceOut
        };
        
    private:
        BusRef  m_bus;
    };

    class Node : public boost::intrusive::list_base_hook<>
    {
    public:
        Node(Environment& env, const NodeId& id, size_t numInputs, size_t numOutputs, size_t bufferSize)
            : m_env(env)
            , m_id(id)
            , m_numInputs(numInputs)
            , m_numOutputs(numOutputs)
            , m_bufferSize(bufferSize)
        { }

        const NodeId& id() const { return m_id; }

        /// Return the biggest acceptable buffer size for this node.
        size_t bufferSize() const { return m_bufferSize; }
        
        /// Return number of inputs.
        size_t numInputs() const { return m_numInputs; }
        /// Get *external* input bus.
        BusRef getInput(size_t input);
        /// Map input to bus.
        void setInput(size_t input, BusRef bus, InputConnection::Type type);

        // Return number of outputs.
        size_t numOutputs() const { return m_numOutputs; }
        /// Get *external* output bus.
        BusRef& getOutput(size_t output);
        // Map output to bus.
        void setOutput(size_t output, BusRef bus, OutputConnection::Type type);

        // Process a number of frames.
        virtual void process(size_t numFrames);
        virtual void compute(size_t numFrames);

        typedef boost::container::vector<InputConnection> InputConnections;
        typedef boost::container::vector<OutputConnection> OutputConnections;
        
    private:
        Environment&        m_env;
        NodeId              m_id;
        size_t              m_numInputs;
        size_t              m_numOutputs;
        size_t              m_bufferSize;
        InputConnections    m_inputConnections;
        OutputConnections   m_outputConnections;
    };
    
    typedef boost::intrusive::list<Node> NodeList;
    
    class Group : public Node
    {
    public:
        Group(Environment& env, const NodeId& id, size_t numInputs, size_t numOutputs, size_t bufferSize);

        const NodeList& children() const { return m_children; }

    private:
        NodeList m_children;
    };

    class Synth : public Node
    {
    };
    
    class FaustSynth : public Synth
    {
    };
}; };

#endif // MESCALINE_AUDIO_ENGINE_H_INCLUDED
