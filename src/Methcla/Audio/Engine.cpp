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

#include "Methcla/Audio/Engine.hpp"
#include "Methcla/Audio/EngineImpl.hpp"
#include "Methcla/Audio/Group.hpp"
#include "Methcla/Audio/Synth.hpp"
#include "Methcla/Memory.hpp"
#include "Methcla/Platform.hpp"

#include <cassert>

using namespace Methcla;
using namespace Methcla::Audio;

extern "C" {

static void methcla_api_host_notify(const Methcla_Host* host, const void* packet, size_t size)
{
    assert(host);
    assert(host->handle);
    static_cast<Environment*>(host->handle)->notify(packet, size);
}

static void methcla_api_host_log_line(const Methcla_Host* host, Methcla_LogLevel level, const char* message)
{
    assert(host);
    assert(host->handle);
    assert(message);
    static_cast<Environment*>(host->handle)->logLineNRT(level, message);
}

static void methcla_api_host_register_synthdef(const Methcla_Host* host, const Methcla_SynthDef* synthDef)
{
    assert(host && host->handle);
    assert(synthDef);
    static_cast<Environment*>(host->handle)->registerSynthDef(synthDef);
}

static void methcla_api_host_register_soundfile_api(const Methcla_Host* host, const Methcla_SoundFileAPI* api)
{
    assert(host && host->handle && api);
    static_cast<Environment*>(host->handle)->registerSoundFileAPI(api);
}

static void* methcla_api_host_alloc(const Methcla_Host*, size_t size)
{
    try {
        return Memory::alloc(size);
    } catch (std::invalid_argument) {
    } catch (std::bad_alloc) {
    }
    return nullptr;
}

static void methcla_api_host_free(const Methcla_Host*, void* ptr)
{
    Memory::free(ptr);
}

static void* methcla_api_host_alloc_aligned(const Methcla_Host*, size_t alignment, size_t size)
{
    try {
        return Memory::allocAligned(Memory::Alignment(alignment), size);
    } catch (std::invalid_argument) {
    } catch (std::bad_alloc) {
    }
    return nullptr;
}

static void methcla_api_host_free_aligned(const Methcla_Host*, void* ptr)
{
    Memory::freeAligned(ptr);
}

static Methcla_Error methcla_api_host_soundfile_open(const Methcla_Host* host, const char* path, Methcla_FileMode mode, Methcla_SoundFile** file, Methcla_SoundFileInfo* info)
{
    assert(host && host->handle);
    assert(path);
    assert(file);
    assert(info);

    auto& apis = static_cast<Environment*>(host->handle)->soundFileAPIs();

    if (apis.empty())
    {
        return methcla_error_new_with_message(
            kMethcla_UnsupportedFileTypeError,
            "No registered sound file APIs"
        );
    }

    // Open sound file with first API that doesn't return an error.
    for (auto it=apis.begin(); it != apis.end(); it++)
    {
        Methcla_Error result = (*it)->open(*it, path, mode, file, info);
        if (methcla_is_ok(result))
        {
            assert(file != nullptr);
            return result;
        }
        else if (!(   methcla_error_has_code(result, kMethcla_UnsupportedFileTypeError)
                   || methcla_error_has_code(result, kMethcla_UnsupportedDataFormatError)))
        {
            return result;
        }
        else
        {
            methcla_error_free(result);
        }
    }

    return methcla_error_new_with_message(
        kMethcla_UnsupportedFileTypeError,
        "File type not supported by any of the registered APIs"
    );
}

static double methcla_api_world_samplerate(const Methcla_World* world)
{
    assert(world && world->handle);
    return static_cast<Environment*>(world->handle)->sampleRate();
}

static size_t methcla_api_world_block_size(const Methcla_World* world)
{
    assert(world && world->handle);
    return static_cast<Environment*>(world->handle)->blockSize();
}

static Methcla_Time methcla_api_world_current_time(const Methcla_World* world)
{
    assert(world != nullptr);
    assert(world->handle != nullptr);
    return static_cast<Environment*>(world->handle)->currentTime();
}

static void* methcla_api_world_alloc(const Methcla_World* world, size_t size)
{
    assert(world && world->handle);
    try {
        return static_cast<Environment*>(world->handle)->rtMem().alloc(size);
    } catch (std::invalid_argument) {
    } catch (std::bad_alloc) {
    }
    return nullptr;
}

static void methcla_api_world_free(const Methcla_World* world, void* ptr)
{
    assert(world && world->handle);
    return static_cast<Environment*>(world->handle)->rtMem().free(ptr);
}

static void* methcla_api_world_alloc_aligned(const Methcla_World* world, size_t alignment, size_t size)
{
    assert(world && world->handle);
    try {
        return static_cast<Environment*>(world->handle)->rtMem().allocAligned(alignment, size);
    } catch (std::invalid_argument) {
    } catch (std::bad_alloc) {
    }
    return nullptr;
}

static void methcla_api_world_free_aligned(const Methcla_World* world, void* ptr)
{
    assert(world && world->handle);
    return static_cast<Environment*>(world->handle)->rtMem().freeAligned(ptr);
}

static void methcla_api_world_log_line(const Methcla_World* world, Methcla_LogLevel level, const char* message)
{
    assert(world);
    assert(world->handle);
    static_cast<Environment*>(world->handle)->logLineRT(level, message);
}

static void methcla_api_world_synth_done(const Methcla_World*, Methcla_Synth* synth)
{
    assert(synth != nullptr);
    Synth::fromSynth(synth)->setDone();
}

static void methcla_api_host_perform_command(const Methcla_Host*, Methcla_WorldPerformFunction, void*);
static void methcla_api_world_perform_command(const Methcla_World*, Methcla_HostPerformFunction, void*);

} // extern "C"

Environment::Environment(
    LogHandler logHandler,
    PacketHandler packetHandler,
    const Options& options,
    MessageQueue* messageQueue,
    Worker* worker
    )
    : m_sampleRate(options.sampleRate)
    , m_blockSize(options.blockSize)
{
    // Initialize Methcla_Host interface
    m_host = {
        this,
        methcla_api_host_register_synthdef,
        methcla_api_host_register_soundfile_api,
        methcla_api_host_alloc,
        methcla_api_host_free,
        methcla_api_host_alloc_aligned,
        methcla_api_host_free_aligned,
        methcla_api_host_soundfile_open,
        methcla_api_host_perform_command,
        methcla_api_host_notify,
        methcla_api_host_log_line
    };

    // Initialize Methcla_World interface
    m_world = {
        this,
        methcla_api_world_samplerate,
        methcla_api_world_block_size,
        methcla_api_world_current_time,
        methcla_api_world_alloc,
        methcla_api_world_free,
        methcla_api_world_alloc_aligned,
        methcla_api_world_free_aligned,
        methcla_api_world_perform_command,
        methcla_api_world_log_line,
        methcla_api_world_synth_done
    };

    m_impl = new EnvironmentImpl(this, logHandler, packetHandler, options, messageQueue, worker);
    m_impl->init(options);

    using namespace std::placeholders;

    m_impl->nrt_log(kMethcla_LogDebug)
        << "Methcla engine (version " << methcla_version() << ")";
}

Environment::~Environment()
{
    delete m_impl;
}

//* Convert environment to Methcla_Host.
Environment::operator const Methcla_Host* () const
{
    return &m_host;
}

Group* Environment::rootNode()
{
    return m_impl->rootNode();
}

//* Convert environment to Methcla_World.
Environment::operator const Methcla_World* () const
{
    return &m_world;
}

size_t Environment::numAudioBuses() const
{
    return m_impl->m_internalAudioBuses.size();
}

AudioBus* Environment::audioBus(AudioBusId id)
{
    return m_impl->m_internalAudioBuses.at(id).get();
}

size_t Environment::numExternalAudioOutputs() const
{
    return m_impl->m_externalAudioOutputs.size();
}

size_t Environment::numExternalAudioInputs() const
{
    return m_impl->m_externalAudioInputs.size();
}

AudioBus* Environment::externalAudioOutput(AudioBusId id)
{
    return m_impl->m_externalAudioOutputs.at(id).get();
}

AudioBus* Environment::externalAudioInput(AudioBusId id)
{
    return m_impl->m_externalAudioInputs.at(id).get();
}

Memory::RTMemoryManager& Environment::rtMem()
{
    return m_impl->rtMem();
}

Epoch Environment::epoch() const
{
    return m_impl->m_epoch;
}

Methcla_Time Environment::currentTime() const
{
    return m_impl->currentTime();
}

void Environment::send(const void* packet, size_t size)
{
    m_impl->m_requests->send(new Request(this, packet, size));
}

bool Environment::hasPendingCommands() const
{
    return !m_impl->m_scheduler.isEmpty();
}

void Environment::sendToWorker(PerformFunc f, void* data)
{
    m_impl->sendToWorker(f, data);
}

void Environment::sendFromWorker(PerformFunc f, void* data)
{
    m_impl->sendFromWorker(f, data);
}

void Environment::process(Methcla_Time currentTime, size_t numFrames, const sample_t* const* inputs, sample_t* const* outputs)
{
    assert( numFrames <= blockSize() );
    m_impl->process(currentTime, numFrames, inputs, outputs);
}

void Environment::setLogFlags(Methcla_EngineLogFlags flags)
{
    m_impl->m_logFlags.store(flags);
}

void Environment::logLineRT(Methcla_LogLevel level, const char* message)
{
    m_impl->logLineRT(level, message);
}

void Environment::logLineNRT(Methcla_LogLevel level, const char* message)
{
    m_impl->logLineNRT(level, message);
}

void Environment::nodeEnded(NodeId nodeId)
{
    m_impl->nodeEnded(nodeId);
}

void Environment::reply(Methcla_RequestId requestId, const void* packet, size_t size)
{
    m_impl->reply(requestId, packet, size);
}

void Environment::reply(Methcla_RequestId requestId, const OSCPP::Client::Packet& packet)
{
    m_impl->reply(requestId, packet);
}

void Environment::replyError(Methcla_RequestId requestId, const char* msg)
{
    m_impl->replyError(requestId, msg);
}

void Environment::notify(const void* packet, size_t size)
{
    m_impl->notify(packet, size);
}

void Environment::notify(const OSCPP::Client::Packet& packet)
{
    m_impl->notify(packet);
}

void Environment::registerSynthDef(const Methcla_SynthDef* def)
{
    m_impl->registerSynthDef(def);
}

const Memory::shared_ptr<SynthDef>& Environment::synthDef(const char* uri) const
{
    return m_impl->synthDef(uri);
}

void Environment::registerSoundFileAPI(const Methcla_SoundFileAPI* api)
{
    m_impl->m_soundFileAPIs.push_front(api);
}

const std::list<const Methcla_SoundFileAPI*>& Environment::soundFileAPIs() const
{
    return m_impl->m_soundFileAPIs;
}

template <typename T> struct CallbackData
{
    T     func;
    void* arg;
};

static void perform_worldCommand(Environment* env, void* data)
{
    CallbackData<Methcla_WorldPerformFunction>* self = (CallbackData<Methcla_WorldPerformFunction>*)data;
    self->func(*env, self->arg);
    env->sendToWorker(perform_nrt_free, self);
}

static void methcla_api_host_perform_command(const Methcla_Host* host, Methcla_WorldPerformFunction perform, void* data)
{
    Environment* env = static_cast<Environment*>(host->handle);
    CallbackData<Methcla_WorldPerformFunction>* callbackData = Methcla::Memory::allocOf<CallbackData<Methcla_WorldPerformFunction>>();
    callbackData->func = perform;
    callbackData->arg = data;
    env->sendFromWorker(perform_worldCommand, callbackData);
}

static void perform_hostCommand(Environment* env, void* data)
{
    CallbackData<Methcla_HostPerformFunction>* self = (CallbackData<Methcla_HostPerformFunction>*)data;
    self->func(*env, self->arg);
    env->sendFromWorker(perform_rt_free, self);
}

static void methcla_api_world_perform_command(const Methcla_World* world, Methcla_HostPerformFunction perform, void* data)
{
    Environment* env = static_cast<Environment*>(world->handle);
    CallbackData<Methcla_HostPerformFunction>* callbackData = env->rtMem().allocOf<CallbackData<Methcla_HostPerformFunction>>();
    callbackData->func = perform;
    callbackData->arg = data;
    env->sendToWorker(perform_hostCommand, callbackData);
}

#include <iostream>

Methcla_LogHandler Methcla::Platform::defaultLogHandler()
{
    Methcla_LogHandler handler;
    handler.handle = nullptr;
    handler.log_line = [](void*, Methcla_LogLevel, const char* message){
        std::cout << message << std::endl;
    };
    return handler;
}
