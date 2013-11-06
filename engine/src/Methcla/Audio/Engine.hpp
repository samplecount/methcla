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
#include "Methcla/Utility/MessageQueueInterface.hpp"
#include "Methcla/Utility/WorkerInterface.hpp"

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

    class Request;

    class EnvironmentImpl;

    class Environment
    {
    public:
        enum Mode
        {
            kRealtimeMode,
            kNonRealtimeMode
        };

        struct Options
        {
            Mode mode = kRealtimeMode;
            size_t realtimeMemorySize = 1024*1024;
            size_t maxNumNodes = 1024;
            size_t maxNumAudioBuses = 1024;
            size_t maxNumControlBuses = 4096;
            size_t sampleRate = 44100;
            size_t blockSize = 64;
            size_t numHardwareInputChannels = 2;
            size_t numHardwareOutputChannels = 2;
            std::list<Methcla_LibraryFunction> pluginLibraries;
        };

        struct Command
        {
            void perform()
            {
                if (m_perform != nullptr)
                    m_perform(m_env, m_data);
            }

            Environment* m_env;
            PerformFunc  m_perform;
            void*        m_data;
        };

        typedef Utility::MessageQueueInterface<Request*> MessageQueue;
        typedef Utility::WorkerInterface<Command> Worker;

        Environment(PacketHandler handler, const Options& options, MessageQueue* queue=nullptr, Worker* worker=nullptr);
        ~Environment();

        Environment(const Environment&) = delete;
        Environment& operator=(const Environment&) = delete;

        //* Convert environment to Methcla_Host.
        operator const Methcla_Host* () const;

        //* Convert environment to Methcla_World.
        operator const Methcla_World* () const;

        //* Return the root group node.
        ResourceRef<Group> rootNode();

        double sampleRate() const { return m_sampleRate; }
        size_t blockSize() const { return m_blockSize; }

        //* Return number of external audio outputs.
        size_t numExternalAudioOutputs() const;
        //* Return number of external audio inputs.
        size_t numExternalAudioInputs() const;

        //* Return external audio output bus at index.
        AudioBus* externalAudioOutput(AudioBusId id);
        //* Return external audio input bus at index.
        AudioBus* externalAudioInput(AudioBusId id);

        //* Return number of audio buses.
        size_t numAudioBuses() const;

        //* Return audio bus with id (needed by Synth).
        AudioBus* audioBus(AudioBusId id);

        Memory::RTMemoryManager& rtMem();

        Epoch epoch() const;

        //* Send an OSC request to the engine.
        void send(const void* packet, size_t size);

        //* Return true if there are any pending scheduled commands.
        bool hasPendingCommands() const;

        //* Register SynthDef.
        void registerSynthDef(const Methcla_SynthDef* synthDef);

        //* Lookup SynthDef
        const std::shared_ptr<SynthDef>& synthDef(const char* uri) const;

        //* Sound file API registration
        void registerSoundFileAPI(const Methcla_SoundFileAPI* api);

        //* Get list of registered soundfile APIs (most recent ones first).
        const std::list<const Methcla_SoundFileAPI*>& soundFileAPIs() const;

        //* Send a command from the realtime thread to the worker thread.
        //
        // Context: RT
        void sendToWorker(PerformFunc f, void* data);

        //* Send a command from the worker thread to the realtime thread.
        //
        // Context: NRT
        void sendFromWorker(PerformFunc f, void* data);

        void process(
            Methcla_Time currentTime,
            size_t numFrames,
            const sample_t* const* inputs,
            sample_t* const* outputs
            );

        void setLogFlags(Methcla_EngineLogFlags flags);

    private:
        friend class EnvironmentImpl;

        static void perform_response_ack(Environment*, CommandData*);
        static void perform_response_nodeId(Environment*, CommandData*);
        static void perform_response_error(Environment*, CommandData*);
        static void perform_response_query_external_inputs(Environment*, CommandData*);
        static void perform_response_query_external_outputs(Environment*, CommandData*);

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

        friend class Group;
        void releaseNode(NodeId nodeId);

    private:
        EnvironmentImpl*                        m_impl;
        const double                            m_sampleRate;
        const size_t                            m_blockSize;
        PacketHandler                           m_listener;
        Methcla_Host                            m_host;
        Methcla_World                           m_world;
    };

    class Engine
    {
    public:
        Engine(PacketHandler handler, const Environment::Options& engineOptions, const IO::Driver::Options& driverOptions);
        virtual ~Engine();

        IO::Driver* driver()
        {
            return m_driver;
        }
        const IO::Driver* driver() const
        {
            return m_driver;
        }

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
        static void processCallback(
            void* data,
            Methcla_Time currentTime,
            size_t numFrames,
            const sample_t* const* inputs,
            sample_t* const* outputs
            );

    private:
        IO::Driver*     m_driver;
        Environment*    m_env;
    };
} }

#endif // METHCLA_AUDIO_ENGINE_HPP_INCLUDED
