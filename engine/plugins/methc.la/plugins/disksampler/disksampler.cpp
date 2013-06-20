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

#include "methc.la/plugins/disksampler/disksampler.h"

#include <atomic>
#include <iostream>
#include <oscpp/server.hpp>
#include <unistd.h>

static const size_t kCacheLineSize = 64;
static const size_t kDiskBlockSize = 8192;
static const size_t kDiskTransferSize = kDiskBlockSize * 4;
static const size_t kNumTransfersPerBuffer = 4;

static inline size_t bytesToFrames(size_t channels, size_t bytes)
{
    return bytes / (channels * sizeof(float));
}

static inline size_t framesToBytes(size_t channels, size_t frames)
{
    return frames * channels * sizeof(float);
}

typedef enum {
    kSampler_amp,
    kSampler_output_0,
    kSampler_output_1,
    kSamplerPorts
} PortIndex;

enum StateVar
{
    kInitializing,
    kIdle,
    kFilling,
    kFinishing,
    kMemoryPlayback,
    kFinished
};

struct State
{
   State(size_t inFrames, size_t inBufferFrames)
       : state(kInitializing)
       , file(nullptr)
       , channels(0)
       , frames(inFrames)
       , transferFrames(0)
       , bufferFrames(inBufferFrames)
       , buffer(nullptr)
       , readPos(0)
       , writePos(0)
   {
       assert( state.is_lock_free() );
       assert( writePos.is_lock_free() );
       assert( readPos.is_lock_free() );
   }

    std::atomic<int> state;

    Methcla_SoundFile* file;

    size_t channels;
    size_t frames;

    size_t transferFrames;
    size_t bufferFrames;
    float* buffer;

    std::atomic<size_t> readPos;
    // Force read and write pointers to different cache lines.
    char padding[kCacheLineSize-sizeof(readPos)];
    std::atomic<size_t> writePos;

    inline static size_t readable(size_t w, size_t r, size_t n)
    {
        return w >= r ? w - r : w + n - r;
    }

    inline static size_t writable(size_t w, size_t r, size_t n)
    {
        return w >= r ? r - w + n - 1 : r - w - 1;
    }
};

struct DiskSampler
{
    Methcla_Resource* handle;
    float* ports[kSamplerPorts];
    char* path;
    bool loop;
    State state;
};

extern "C" {
    static bool port_descriptor(const Methcla_SynthOptions*, Methcla_PortCount, Methcla_PortDescriptor*);
    static void configure(const void*, size_t, const void*, size_t, Methcla_SynthOptions*);
    static void command_fill_buffer(const Methcla_Host*, void*);
    static void command_init_buffer(const Methcla_Host*, void*);
    static void construct( const Methcla_World*, const Methcla_SynthDef*, const Methcla_SynthOptions*, Methcla_Resource*, Methcla_Synth*);
    static void release_synth_cb(const Methcla_World*, void*);
    static void free_cb(const Methcla_Host*, void*);
    static void close_cb(const Methcla_Host*, void*);
    static void destroy(const Methcla_World*, Methcla_Synth*);
    static void connect(Methcla_Synth*, Methcla_PortCount, void* data);
    static void process(const Methcla_World*, Methcla_Synth*, size_t);
}

static bool
port_descriptor( const Methcla_SynthOptions* /* options */
               , Methcla_PortCount index
               , Methcla_PortDescriptor* port )
{
    switch ((PortIndex)index) {
        case kSampler_amp:
            port->type = kMethcla_ControlPort;
            port->direction = kMethcla_Input;
            port->flags = kMethcla_PortFlags;
            return true;
        case kSampler_output_0:
        case kSampler_output_1:
            port->type = kMethcla_AudioPort;
            port->direction = kMethcla_Output;
            port->flags = kMethcla_PortFlags;
            return true;
        default:
            return false;
    }
}

struct Options
{
    const char* path;
    bool loop;
    int32_t frames;
};

static void
configure(const void* tags, size_t tags_size, const void* args, size_t args_size, Methcla_SynthOptions* outOptions)
{
    OSCPP::Server::ArgStream argStream(OSCPP::ReadStream(tags, tags_size), OSCPP::ReadStream(args, args_size));
    Options* options = (Options*)outOptions;
    options->path = argStream.string();
    options->loop = argStream.atEnd() ? false : argStream.int32();
    options->frames = argStream.atEnd() ? -1 : argStream.int32();
    // std::cout << "DiskSampler: "
    //           << options->path << " "
    //           << options->loop << " "
    //           << options->frames << "\n";
}

static inline Methcla_Error read_all(Methcla_SoundFile* file, float* buffer, size_t channels, size_t inNumFrames, size_t* outNumFrames, bool loop)
{
    size_t numFramesToRead = inNumFrames;
    size_t numFramesRead = 0;

    for (;;) {
        size_t numFrames;
        Methcla_Error err =
            methcla_soundfile_read_float(file, buffer + channels * numFramesRead, numFramesToRead, &numFrames);
        if (err != kMethcla_NoError) return err;
        numFramesToRead -= numFrames;
        numFramesRead += numFrames;
        if (!loop || numFramesToRead == 0)
            break;
        // If looping, seek back to the beginning
        err = methcla_soundfile_seek(file, 0);
        if (err != kMethcla_NoError) return err;
    }

    *outNumFrames = numFramesRead;

    return kMethcla_NoError;
}

static inline void finish(State* state)
{
    state->state.store(kFinished, std::memory_order_relaxed);
}

static inline void finish(DiskSampler* self)
{
    finish(&self->state);
}

static void release_synth_cb(const Methcla_World* world, void* data)
{
    methcla_world_resource_release(world, (Methcla_Resource*)data);
}

static inline void fill_buffer(State* self, bool loop)
{
    // std::cout << "fill_buffer\n";

    const size_t transferFrames = self->transferFrames;
    const size_t bufferFrames = self->bufferFrames;
    const size_t writePos = self->writePos.load(std::memory_order_relaxed);

    assert( (bufferFrames % transferFrames) == 0 );
    assert( State::writable(
                writePos,
                self->readPos.load(std::memory_order_acquire),
                bufferFrames)
            >= transferFrames );

    size_t numFrames;
    Methcla_Error err = read_all(
        self->file,
        self->buffer + self->channels * writePos,
        self->channels,
        transferFrames,
        &numFrames,
        loop);
    if (err != kMethcla_NoError) {
        finish(self);
        return;
    }

    assert( !loop || (numFrames == transferFrames) );

    const size_t nextWritePos = writePos + numFrames;
    self->writePos.store(nextWritePos == bufferFrames ? 0 : nextWritePos, std::memory_order_release);

    if (!loop && (numFrames < transferFrames)) {
        self->state.store(kFinishing, std::memory_order_release);
    } else {
        self->state.store(kIdle, std::memory_order_release);
    }
}

static void command_fill_buffer(const Methcla_Host* host, void* data)
{
    // std::cout << "fill_buffer\n";
    DiskSampler* self = static_cast<DiskSampler*>(data);
    fill_buffer(&self->state, self->loop);
    methcla_host_perform_command(host, release_synth_cb, self->handle);
}

// Only safe to be called during initialization
static inline void init_buffer_cleanup(DiskSampler* self)
{
    finish(self);
    if (self->state.file) {
        methcla_soundfile_close(self->state.file);
        self->state.file = nullptr;
    }
    if (self->state.buffer) {
        free(self->state.buffer);
        self->state.buffer = nullptr;
    }
}

static void command_init_buffer(const Methcla_Host* host, void* data)
{
    DiskSampler* self = static_cast<DiskSampler*>(data);

    Methcla_SoundFileInfo info;

    Methcla_Error err = methcla_host_soundfile_open(
        host,
        self->path,
        kMethcla_Read,
        &self->state.file,
        &info);

    if (err == kMethcla_NoError) {
        self->state.channels = info.channels;
        self->state.frames = self->state.frames <= 0 ? info.frames : std::min<int64_t>(self->state.frames, info.frames);
        // If kDiskTransferSize is smaller than audio block size, use audio block size.
        const size_t transferFrames = std::max(
            bytesToFrames(self->state.channels, kDiskTransferSize),
            // bufferFrames is audio block size initially
            self->state.bufferFrames);
        if (self->state.frames <= transferFrames) {
            // If the file's number of frames is less than transferFrames,
            // read it once and play back directly from memory.
            self->state.transferFrames = 0;
            self->state.bufferFrames = self->state.frames;
            self->state.buffer = (float*)malloc(framesToBytes(self->state.channels, self->state.bufferFrames));
            if (self->state.buffer == nullptr) {
                init_buffer_cleanup(self);
            } else {
                size_t numFrames = 0;
                Methcla_Error err = methcla_soundfile_read_float(
                    self->state.file, self->state.buffer, self->state.bufferFrames, &numFrames);
                if (err == kMethcla_NoError && numFrames == self->state.bufferFrames) {
                    // After having read the whole file close it right away.
                    methcla_soundfile_close(self->state.file);
                    self->state.file = nullptr;
                    self->state.state.store(kMemoryPlayback, std::memory_order_release);
                } else {
                    init_buffer_cleanup(self);
                }
            }
        } else {
            // Load the first transferFrames into memory for streaming.
            self->state.transferFrames = transferFrames;
            self->state.bufferFrames = self->state.transferFrames * kNumTransfersPerBuffer;
            self->state.buffer = (float*)malloc(framesToBytes(self->state.channels, self->state.bufferFrames));
            if (self->state.buffer == nullptr) {
                init_buffer_cleanup(self);
            } else {
                size_t numFrames = 0;
                Methcla_Error err = methcla_soundfile_read_float(
                    self->state.file, self->state.buffer, self->state.transferFrames, &numFrames);
                if (err == kMethcla_NoError) {
                    assert( numFrames == self->state.transferFrames );
                    self->state.writePos.store(numFrames == self->state.bufferFrames ? 0 : numFrames, std::memory_order_release);
                    self->state.state.store(kIdle, std::memory_order_release);
                } else {
                    init_buffer_cleanup(self);
                }
            }
        }
   } else {
        init_buffer_cleanup(self);
    }

    methcla_host_perform_command(host, release_synth_cb, self->handle);
}

static void
construct( const Methcla_World* world
         , const Methcla_SynthDef* /* synthDef */
         , const Methcla_SynthOptions* inOptions
         , Methcla_Resource* handle
         , Methcla_Synth* synth )
{
    const Options* options = (const Options*)inOptions;

    DiskSampler* self = (DiskSampler*)synth;

    self->path = (char*)methcla_world_alloc(world, strlen(options->path)+1);
    if (self->path == nullptr) return;
    strcpy(self->path, options->path);

    self->loop = options->loop;

    new (&self->state) State(options->frames, methcla_world_block_size(world));

    // Need to retain a reference to the outer Synth, otherwise the self pointer
    // might dangle if the synth is freed while the async command is still being
    // executed.
    self->handle = handle;
    methcla_world_resource_retain(world, self->handle);
    methcla_world_perform_command(world, command_init_buffer, self);
}

static void free_cb(const Methcla_Host*, void* data)
{
    free(data);
}

static void close_cb(const Methcla_Host*, void* data)
{
    methcla_soundfile_close((Methcla_SoundFile*)data);
}

static void
destroy(const Methcla_World* world, Methcla_Synth* synth)
{
    DiskSampler* self = (DiskSampler*)synth;
    if (self->path) {
        methcla_world_free(world, self->path);
        self->path = nullptr;
    }
    if (self->state.buffer) {
        methcla_world_perform_command(world, free_cb, self->state.buffer);
        self->state.buffer = nullptr;
    }
    if (self->state.file) {
        methcla_world_perform_command(world, close_cb, self->state.file);
        self->state.file = nullptr;
    }
}

static void
connect( Methcla_Synth* synth
       , Methcla_PortCount index
       , void* data )
{
    ((DiskSampler*)synth)->ports[index] = (float*)data;
}

static inline void
process_disk(const Methcla_World* world, DiskSampler* self, size_t numFrames, float amp, const float* buffer, float* out0, float* out1, StateVar state)
{
    assert( self->state.file != nullptr );

    const size_t bufferFrames = self->state.bufferFrames;
    const size_t writePos = self->state.writePos.load(std::memory_order_acquire);
    const size_t readPos = self->state.readPos.load(std::memory_order_relaxed);

    if (state == kIdle) {
        if (State::writable(writePos, readPos, bufferFrames) >= self->state.transferFrames) {
            // Trigger refill
            self->state.state.store(kFilling, std::memory_order_relaxed);
            methcla_world_resource_retain(world, self->handle);
            methcla_world_perform_command(world, command_fill_buffer, self);
        }
    }

    const size_t readable = std::min(numFrames, State::readable(writePos, readPos, bufferFrames));
    const size_t readable1 = std::min(readable, self->state.bufferFrames - readPos);
    const size_t readable2 = readable - readable1;
    const size_t channels = self->state.channels;

    if (channels == 1) {
        for (size_t k = 0; k < readable1; k++) {
            out0[k] = out1[k] = amp * buffer[(readPos+k)*channels];
        }
        for (size_t k = 0; k < readable2; k++) {
            out0[readable1+k] = out1[readable1+k] = amp * buffer[k*channels];
        }
        for (size_t k = readable; k < numFrames; k++) {
            out0[k] = out1[k] = 0.f;
        }
    } else {
        for (size_t k = 0; k < readable1; k++) {
            const size_t j = (readPos+k)*channels;
            out0[k] = amp * buffer[j];
            out1[k] = amp * buffer[j+1];
        }
        for (size_t k = 0; k < readable2; k++) {
            const size_t m = readable1+k;
            const size_t j = k*channels;
            out0[m] = amp * buffer[j];
            out1[m] = amp * buffer[j+1];
        }
        for (size_t k = readable; k < numFrames; k++) {
            out0[k] = out1[k] = 0.f;
        }
    }

    if (readable < numFrames) {
        if (state == kFinishing) {
            self->state.state.store(kFinished, std::memory_order_relaxed);
        }
#if DEBUG
        else {
            std::cerr << "process_disk: underrun " << numFrames << " " << readable << " " << numFrames - readable << "\n";
        }
#endif
    }

    const size_t nextReadPos = readable2 > 0 ? readable2 : readPos + readable1;
    self->state.readPos.store(nextReadPos == bufferFrames ? 0 : nextReadPos, std::memory_order_release);
}

static inline void
process_memory(DiskSampler* self, size_t numFrames, float amp, const float* buffer, float* out0, float* out1)
{
    size_t pos = self->state.readPos;
    const size_t left = self->state.frames - pos;
    const size_t channels = self->state.channels;

    if (left >= numFrames) {
        if (channels == 1) {
            for (size_t k = 0; k < numFrames; k++) {
                out0[k] = out1[k] = amp * buffer[(pos+k)*channels];
            }
        } else {
            for (size_t k = 0; k < numFrames; k++) {
                const size_t j = (pos+k)*channels;
                out0[k] = amp * buffer[j];
                out1[k] = amp * buffer[j+1];
            }
        }
        if (left == numFrames) {
            if (self->loop) self->state.readPos = 0;
            else finish(self);
        } else {
            self->state.readPos = pos + numFrames;
        }
    } else if (self->loop) {
        size_t played = 0;
        size_t toPlay = left;
        while (played < numFrames) {
            if (channels == 1) {
                for (size_t k = 0; k < toPlay; k++) {
                    const size_t m = played+k;
                    out0[m] = out1[m] = amp * buffer[(pos+k)*channels];
                }
            } else {
                for (size_t k = 0; k < toPlay; k++) {
                    const size_t m = played+k;
                    const size_t j = (pos+k)*channels;
                    out0[m] = amp * buffer[j];
                    out1[m] = amp * buffer[j+1];
                }
            }
            played += toPlay;
            pos += toPlay;
            if (pos >= self->state.frames)
                pos = 0;
            toPlay = std::min<size_t>(numFrames - played, self->state.frames);
        }
        self->state.readPos = pos;
    } else {
        if (channels == 1) {
            for (size_t k = 0; k < left; k++) {
                out0[k] = out1[k] = amp * buffer[(pos+k)*channels];
            }
        } else {
            for (size_t k = 0; k < left; k++) {
                const size_t j = (pos+k)*channels;
                out0[k] = amp * buffer[j];
                out1[k] = amp * buffer[j+1];
            }
        }
        for (size_t k = left; k < numFrames; k++) {
            out0[k] = out1[k] = 0.f;
        }
        finish(self);
    }
}

static void
process(const Methcla_World* world, Methcla_Synth* synth, size_t numFrames)
{
    DiskSampler* self = static_cast<DiskSampler*>(synth);

    const float amp = *self->ports[kSampler_amp];
    float* out0 = self->ports[kSampler_output_0];
    float* out1 = self->ports[kSampler_output_1];
    const float* buffer = self->state.buffer;

    StateVar state = (StateVar)self->state.state.load(std::memory_order_consume);
    switch (state) {
        case kIdle:
        case kFilling:
        case kFinishing:
        process_disk(world, self, numFrames, amp, buffer, out0, out1, state);
        break;
        case kMemoryPlayback:
        process_memory(self, numFrames, amp, buffer, out0, out1);
        break;
        case kInitializing:
        case kFinished:
        for (size_t k = 0; k < numFrames; k++) {
            out0[k] = out1[k] = 0.f;
        }
        break;
    }
}

static const Methcla_SynthDef descriptor =
{
    METHCLA_PLUGINS_DISKSAMPLER_URI,
    sizeof(DiskSampler),
    sizeof(Options),
    configure,
    port_descriptor,
    construct,
    connect,
    nullptr,
    process,
    destroy
};

static const Methcla_Library library = { nullptr, nullptr };

METHCLA_EXPORT const Methcla_Library* methcla_plugins_disksampler(const Methcla_Host* host, const char* /* bundlePath */)
{
    methcla_host_register_synthdef(host, &descriptor);
    return &library;
}
