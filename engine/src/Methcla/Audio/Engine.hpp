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
#include <methcla/plugin.h>

#include "Methcla/Audio.hpp"
#include "Methcla/Audio/AudioBus.hpp"
#include "Methcla/Audio/IO/Driver.hpp"
#include "Methcla/Audio/Node.hpp"
#include "Methcla/Audio/SynthDef.hpp"
#include "Methcla/Memory/Manager.hpp"

#include <boost/utility.hpp>

#include <cstddef>
#include <functional>
#include <string>
#include <vector>

#include <oscpp/client.hpp>
#include <oscpp/server.hpp>

namespace Methcla { namespace Audio
{
    class Environment;

    typedef void CommandData;

    typedef void (*PerformFunc)(Environment* env, void* data);

    class Group;

    typedef std::function<void (Methcla_RequestId, const void*, size_t)> PacketHandler;

    class EnvironmentImpl;

    class Environment : public boost::noncopyable
    {
    public:
        struct Options
        {
            Options()
                : realtimeMemorySize(1024*1024)
                , maxNumNodes(1024)
                , maxNumAudioBuses(1024)
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

        Environment(PacketHandler handler, const Options& options);
        ~Environment();

        Group* rootNode() { return m_rootNode; }

        size_t sampleRate() const { return m_sampleRate; }
        size_t blockSize() const { return m_blockSize; }

        //* Return number of external audio outputs.
        size_t numExternalAudioOutputs() const;
        //* Return number of external audio inputs.
        size_t numExternalAudioInputs() const;

        //* Return external audio output bus at index.
        AudioBus* externalAudioOutput(AudioBusId id);
        //* Return external audio input bus at index.
        AudioBus* externalAudioInput(AudioBusId id);

        //* Return audio bus with id (needed by Synth).
        AudioBus* audioBus(AudioBusId id);

        Memory::RTMemoryManager& rtMem();

        Epoch epoch() const;

        //* Send an OSC request to the engine.
        void send(const void* packet, size_t size);

        //* Register SynthDef.
        void registerSynthDef(const Methcla_SynthDef* synthDef);

        //* Lookup SynthDef
        const std::shared_ptr<SynthDef>& synthDef(const char* uri) const;

        //* Sound file API registration
        void registerSoundFileAPI(const char* mimeType, const Methcla_SoundFileAPI* api);
        const Methcla_SoundFileAPI* soundFileAPI(const char* mimeType) const;

        //* Convert environment to Methcla_Host.
        const Methcla_Host* asHost() const { return &m_host; }

        //* Convert environment to Methcla_World.
        const Methcla_World* asWorld() const { return &m_world; }

        //* Send a command from the realtime thread to the worker thread.
        //
        // Context: RT
        void sendToWorker(PerformFunc f, void* data);

        //* Send a command from the worker thread to the realtime thread.
        //
        // Context: NRT
        void sendFromWorker(PerformFunc f, void* data);

    protected:
        friend class EnvironmentImpl;
        friend class Engine;

        ResourceMap<NodeId,Node>& nodes()
        {
            return m_nodes;
        }

        void process(size_t numFrames, const sample_t* const* inputs, sample_t* const* outputs);

    private:
        static void perform_response_ack(Environment*, CommandData*);
        static void perform_response_nodeId(Environment*, CommandData*);
        static void perform_response_error(Environment*, CommandData*);
        static void perform_response_query_external_inputs(Environment*, CommandData*);
        static void perform_response_query_external_outputs(Environment*, CommandData*);

        void processRequests();
        void processMessage(const OSCPP::Server::Message& message);
        void processBundle(const OSCPP::Server::Bundle& bundle);

        //* Context: NRT
        void reply(Methcla_RequestId requestId, const void* packet, size_t size)
        {
            m_listener(requestId, packet, size);
        }

        void reply(Methcla_RequestId requestId, const OSCPP::Client::Packet& packet)
        {
            reply(requestId, packet.data(), packet.size());
        }

        void replyError(Methcla_RequestId requestId, const char* what);

        //* Context: NRT
        void notify(const void* packet, size_t size)
        {
            m_listener(kMethcla_Notification, packet, size);
        }

        void notify(const OSCPP::Client::Packet& packet)
        {
            notify(packet.data(), packet.size());
        }

    private:
        EnvironmentImpl*                        m_impl;
        const size_t                            m_sampleRate;
        const size_t                            m_blockSize;
        SynthDefMap                             m_synthDefs;
        PacketHandler                           m_listener;
        ResourceMap<NodeId,Node>                m_nodes;
        Group*                                  m_rootNode;
        Methcla_Host                            m_host;
        Methcla_World                           m_world;
        std::list<const Methcla_SoundFileAPI*>  m_soundFileAPIs;
    };

    class Engine
    {
    public:
        Engine(PacketHandler handler, IO::Driver::Options driverOptions);
        virtual ~Engine();

        Environment& env()
        {
            return *m_env;
        }
        const Environment& env() const
        {
            return *m_env;
        }

        void loadPlugins(const std::list<Methcla_LibraryFunction>& funcs);

        void start();
        void stop();

    private:
        static void processCallback(void* data, size_t numFrames, const sample_t* const* inputs, sample_t* const* outputs);

    private:
        PluginManager   m_plugins;
        IO::Driver*     m_driver;
        Environment*    m_env;
    };
} }

#endif // METHCLA_AUDIO_ENGINE_HPP_INCLUDED
