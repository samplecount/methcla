#ifndef MESCALINE_AUDIO_ENGINE_H_INCLUDED
#define MESCALINE_AUDIO_ENGINE_H_INCLUDED

#include <Mescaline/API.h>
#include <Mescaline/Audio.hpp>
#include <Mescaline/Audio/AudioBus.hpp>
#include <Mescaline/Audio/Node.hpp>
#include <Mescaline/Audio/IO/Client.hpp>
#include <Mescaline/Audio/SynthDef.hpp>
#include <Mescaline/Exception.hpp>
#include <Mescaline/Memory/Manager.hpp>

#include <boost/container/vector.hpp>
#include <boost/cstdint.hpp>
#include <boost/ptr_container/ptr_map.hpp>
#include <boost/ptr_container/ptr_vector.hpp>
#include <boost/thread/thread.hpp>
#include <boost/utility.hpp>

#include <bitset>
#include <string>
#include <vector>

using namespace boost::container;

namespace Mescaline { namespace Audio
{
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

    class Node;
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

    class PluginInterface;

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

        Environment(const Options& options);
        ~Environment();

        typedef boost::ptr_map<string,SynthDef> SynthDefMap;
        const SynthDefMap& synthDefs() const { return m_synthDefs; }
        void registerSynthDef(SynthDef* synthDef)
            { string name(synthDef->name());
              m_synthDefs.insert(name, synthDef); }
        
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

    	MescalineHost* pluginInterface();
	
   private:
        const size_t                m_sampleRate;
        const size_t                m_blockSize;
        PluginInterface*            m_pluginInterface;
        RTMemoryManager             m_rtMem;
        SynthDefMap                 m_synthDefs;
        Group*                      m_rootNode;
        NodeMap                     m_nodes;
        boost::ptr_vector<ExternalAudioBus> m_audioInputChannels;
        boost::ptr_vector<ExternalAudioBus> m_audioOutputChannels;
        boost::ptr_vector<InternalAudioBus> m_audioBuses;
        Epoch                       m_epoch;
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
        static PluginInterface* cast(MescalineHost* self)
            { return reinterpret_cast<PluginInterface*>(self); }
        static const PluginInterface* cast_const(const MescalineHost* self)
            { return reinterpret_cast<const PluginInterface*>(self); }
        static unsigned int GetSampleRate(const MescalineHost* self)
            { return cast_const(self)->m_env.sampleRate(); }
        static void RegisterSynthDef(MescalineHost* self, MescalineSynthDef* synthDef)
            { cast(self)->m_env.registerSynthDef(new SynthDef(self, synthDef)); }

    private:
        Environment& m_env;
    };
    
    class Engine : public IO::Client
    {
    public:
        Engine();
        virtual void configure(const IO::Driver& driver);
        virtual void process(size_t numFrames, sample_t** inputs, sample_t** outputs);
    
    protected:
        Environment* environment() { return m_env; }

    private:
        Environment* m_env;
    };
}; };

#endif // MESCALINE_AUDIO_ENGINE_H_INCLUDED
