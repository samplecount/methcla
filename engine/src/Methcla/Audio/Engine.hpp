// Copyright 2012-2013 Samplecount S.L.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

#ifndef METHCLA_AUDIO_ENGINE_HPP_INCLUDED
#define METHCLA_AUDIO_ENGINE_HPP_INCLUDED

#include <methcla/engine.h>
#include <methcla/lv2/atom.hpp>

#include "Methcla/Audio.hpp"
#include "Methcla/Audio/AudioBus.hpp"
#include "Methcla/Audio/IO/Driver.hpp"
#include "Methcla/Audio/Node.hpp"
#include "Methcla/Audio/SynthDef.hpp"
#include "Methcla/Exception.hpp"
#include "Methcla/Memory/Manager.hpp"
#include "Methcla/Utility/MessageQueue.hpp"

#include <boost/utility.hpp>

#include <cstddef>
#include <functional>
#include <string>
#include <vector>

#include <oscpp/client.hpp>
#include <oscpp/server.hpp>

#include "lv2/lv2plug.in/ns/ext/atom/atom.h"
#include "lv2/lv2plug.in/ns/ext/patch/patch.h"

// This is missing from patch.h
#ifndef LV2_PATCH__Insert
# define LV2_PATCH__Insert LV2_PATCH_PREFIX "Insert"
#endif

namespace Methcla { namespace Audio
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

    struct EngineException : virtual Methcla::Exception { };
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

    typedef std::function<void (Methcla_RequestId, const void*, size_t)> PacketHandler;

    class Environment : public boost::noncopyable
    {
    public:
        struct Options
        {
            Options()
                : realtimeMemorySize(1024*1024)
                , maxNumNodes(1024)
                , maxNumAudioBuses(128)
                , maxNumControlBuses(4096)
                , sampleRate(44100)
                , blockSize(64)
                , numHardwareInputChannels(2)
                , numHardwareOutputChannels(2)
            { }

            size_t realtimeMemorySize;
            size_t maxNumNodes;
            size_t maxNumAudioBuses;
            size_t maxNumControlBuses;
            size_t sampleRate;
            size_t blockSize;
            size_t numHardwareInputChannels;
            size_t numHardwareOutputChannels;
        };

        Environment(PluginManager& pluginManager, const PacketHandler& handler, const Options& options);
        ~Environment();

        const PluginManager& plugins() const { return m_plugins; }
        PluginManager& plugins() { return m_plugins; }

        Group* rootNode() { return m_rootNode; }

        size_t sampleRate() const { return m_sampleRate; }
        size_t blockSize() const { return m_blockSize; }

        //* Return audio bus with id.
        AudioBus* audioBus(const AudioBusId& id);
        //* Return number of external audio outputs.
        size_t numExternalAudioOutputs() const
        {
            return m_audioOutputChannels.size();
        }
        //* Return number of external audio inputs.
        size_t numExternalAudioInputs() const
        {
            return m_audioInputChannels.size();
        }

        //* Return external audio output bus at index.
        AudioBus& externalAudioOutput(size_t index);
        //* Return external audio input bus at index.
        AudioBus& externalAudioInput(size_t index);

        Memory::RTMemoryManager& rtMem() { return m_rtMem; }

        const Epoch& epoch() const { return m_epoch; }

        void process(size_t numFrames, sample_t** inputs, sample_t** outputs);

        //* Send an OSC request to the engine.
        void send(const void* packet, size_t size);

    protected:
        static const size_t kQueueSize = 8192;

        struct Request
        {
            void* packet;
            size_t size;
            void (*free)(void*);
        };

        typedef Utility::MessageQueue<Request,kQueueSize> MessageQueue;

        union CommandData
        {
            struct
            {
                void (*func)(void*);
                void* ptr;
            } free;
            struct
            {
                Methcla_RequestId requestId;
                union {
                    uint32_t nodeId;
                    char error[512];
                } data;
            } response;
        };

        struct Command
        {
            typedef Utility::Channel<Command,kQueueSize> Channel;
            typedef void (*PerformFunc)(Command& cmd, Channel& channel);

            Command()
                : env(nullptr)
                , performFunc(nullptr)
            {
                memset(&data, 0, sizeof(data));
            }
            Command(Environment* e, PerformFunc f)
                : env(e)
                , performFunc(f)
            {
                memset(&data, 0, sizeof(data));
            }
            Command(const Command& other)
                : env(other.env)
                , performFunc(other.performFunc)
            {
                memcpy(&data, &other.data, sizeof(data));
            }
            Command(Environment* e, PerformFunc f, Methcla_RequestId requestId)
                : Command(e, f)
            {
                data.response.requestId = requestId;
            }

            void perform(Channel& channel)
            {
                if (performFunc != nullptr) performFunc(*this, channel);
            }
            
            Environment* env;
            PerformFunc  performFunc;
            CommandData  data;
        };

        static void perform_free(Command&, Command::Channel&);
        static void perform_response_ack(Command&, Command::Channel&);
        static void perform_response_nodeId(Command&, Command::Channel&);
        static void perform_response_error(Command&, Command::Channel&);
        static void perform_response_query_external_inputs(Command&, Command::Channel&);
        static void perform_response_query_external_outputs(Command&, Command::Channel&);

    protected:
        // Worker thread
        typedef Utility::WorkerThread<Command,kQueueSize> Worker;

        void processRequests();
        void processMessage(const OSC::Server::Message& message);
        void processBundle(const OSC::Server::Bundle& bundle);

        //* Context: NRT
        void reply(Methcla_RequestId requestId, const void* packet, size_t size)
        {
            m_listener(requestId, packet, size);
        }

        void reply(Methcla_RequestId requestId, const OSC::Client::Packet& packet)
        {
            reply(requestId, packet.data(), packet.size());
        }

        void replyError(Methcla_RequestId requestId, const char* what);

        //* Context: NRT
        void notify(const void* packet, size_t size)
        {
            m_listener(kMethcla_Notification, packet, size);
        }

        void notify(const OSC::Client::Packet& packet)
        {
            notify(packet.data(), packet.size());
        }

        //* Send a command to the worker thread.
        //
        // Context: RT
        void send(const Command& cmd)
        {
            m_worker.toWorker().send(cmd);
        }

    protected:
        friend class Node;
        ResourceMap<NodeId,Node>& nodes() { return m_nodes; }

    private:
        const size_t                        m_sampleRate;
        const size_t                        m_blockSize;
        Memory::RTMemoryManager             m_rtMem;
        PluginManager&                      m_plugins;
        PacketHandler                       m_listener;
        ResourceMap<AudioBusId,AudioBus>    m_audioBuses;
        ResourceMap<AudioBusId,AudioBus>    m_freeAudioBuses;
        ResourceMap<NodeId,Node>            m_nodes;
        Group*                              m_rootNode;
        std::vector<ExternalAudioBus*>      m_audioInputChannels;
        std::vector<ExternalAudioBus*>      m_audioOutputChannels;
        Epoch                               m_epoch;
        MessageQueue                        m_requests;
        Worker                              m_worker;
    };

    class Engine
    {
    public:
        Engine(PluginManager& pluginManager, const PacketHandler& handler, const std::string& lv2Directory);
        virtual ~Engine();

        Environment& env()
        {
            return *m_env;
        }
        const Environment& env() const
        {
            return *m_env;
        }

        void start();
        void stop();

    private:
        static void processCallback(void* data, size_t numFrames, sample_t** inputs, sample_t** outputs);

    private:
        IO::Driver*     m_driver;
        Environment*    m_env;
    };
}; };

#endif // METHCLA_AUDIO_ENGINE_HPP_INCLUDED
