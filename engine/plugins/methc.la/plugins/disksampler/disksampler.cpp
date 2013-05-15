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

static const size_t kDiskBlockSize = 8192;
static const size_t kDiskTransferSize = kDiskBlockSize * 4;

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

enum
{
    kInitializing,
    kIdle,
    kFilling,
    kMemoryPlayback,
    kFinished
};

struct State
{
    // State()
    //     : state(kInitializing)
    //     , file(nullptr)
    //     , buffer(nullptr)
    // { }
    // ~State()
    // {
    //     if (file != nullptr) {
    //         methcla_soundfile_close(file);
    //         file = nullptr;
    //     }
    //     if (buffer != nullptr) {
    //         std::free(buffer);
    //         buffer = nullptr;
    //     }
    // }

    std::atomic<int> state;

    Methcla_SoundFile* file;

    size_t channels;
    size_t frames;

    size_t bufferFrames;
    size_t transferFrames;
    float* buffer;

    std::atomic<size_t> readPos;
    std::atomic<size_t> writePos;

    inline static size_t readable(size_t w, size_t r, size_t n)
    {
        return w >= r ? w - r : w + n - r;
    }

    inline static size_t writable(size_t w, size_t r, size_t n)
    {
        return w >= r ? r - w + n - 1 : r - w - 1;
    }

    inline size_t readable() const
    {
        size_t w = writePos.load(std::memory_order_relaxed);
        size_t r = readPos.load(std::memory_order_relaxed);
        return readable(w, r, bufferFrames);
    }

    inline size_t writable() const
    {
        size_t w = writePos.load(std::memory_order_relaxed);
        size_t r = readPos.load(std::memory_order_relaxed);
        return writable(w, r, bufferFrames);
    }
};

struct Synth {
    float* ports[kSamplerPorts];
    char* path;
    bool loop;
    State state;
};

static bool
port_descriptor( const Methcla_SynthOptions* options
               , size_t index
               , Methcla_PortDescriptor* port )
{
    switch ((PortIndex)index) {
        case kSampler_amp:
            *port = { .type = kMethcla_ControlPort,
                      .direction = kMethcla_Input,
                      .flags = kMethcla_PortFlags };
            return true;
        case kSampler_output_0:
        case kSampler_output_1:
            *port = { .type = kMethcla_AudioPort,
                      .direction = kMethcla_Output,
                      .flags = kMethcla_PortFlags };
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
    OSC::Server::ArgStream argStream(OSC::ReadStream(tags, tags_size), OSC::ReadStream(args, args_size));
    Options* options = (Options*)outOptions;
    options->path = argStream.string();
    options->loop = argStream.atEnd() ? false : argStream.int32();
    options->frames = argStream.atEnd() ? -1 : argStream.int32();
    std::cout << "DiskSampler: "
              << options->path << " "
              << options->loop << " "
              << options->frames << "\n";
}

static Methcla_FileError read_all(Methcla_SoundFile* file, float* buffer, size_t inNumFrames, size_t* outNumFrames, bool loop)
{
    size_t numFramesToRead = inNumFrames;
    size_t numFramesRead = 0;
    for (;;) {
        size_t numFrames;
        Methcla_FileError err =
            methcla_soundfile_read_float(file, buffer, numFramesToRead, &numFrames);
        if (err != kMethcla_FileNoError) return err;
        numFramesToRead -= numFrames;
        numFramesRead += numFrames;
        if (!loop || numFramesToRead == 0)
            break;
        err = methcla_soundfile_seek(file, 0);
        if (err != kMethcla_FileNoError) return err;
    }
    *outNumFrames = numFramesRead;
    return kMethcla_FileNoError;
}

static inline void finish(Synth* self)
{
    self->state.state.store(kFinished, std::memory_order_relaxed);
}

static void fill_buffer(const Methcla_Host*, void* data)
{
//    std::cout << "fill_buffer\n";
    Synth* self = (Synth*)data;

    assert( self->state.writable() >= self->state.transferFrames );

    const size_t bufferFrames = self->state.bufferFrames;
    const size_t transferFrames = self->state.transferFrames;
    const size_t writePos = self->state.writePos.load(std::memory_order_relaxed);

    const size_t numFrames1 = std::min(transferFrames, bufferFrames-writePos);
    const size_t numFrames2 = transferFrames - numFrames1;

    size_t numFrames;
    Methcla_FileError err = read_all(
        self->state.file,
        self->state.buffer + self->state.channels * writePos,
        numFrames1,
        &numFrames,
        self->loop);
    if (err != kMethcla_FileNoError) {
        finish(self);
        return;
    }

    if ((numFrames == numFrames1) && (numFrames2 > 0)) {
        err = read_all(
            self->state.file,
            self->state.buffer,
            numFrames2,
            &numFrames,
            self->loop);
        if (err != kMethcla_FileNoError) {
            finish(self);
            return;
        }
    }

    const size_t nextWritePos = numFrames2 > 0 ? numFrames2 : writePos + numFrames1;
    self->state.writePos.store(nextWritePos == bufferFrames ? 0 : nextWritePos, std::memory_order_relaxed);
    self->state.state.store(kIdle, std::memory_order_release);
}

// Only safe to be called during initialization
inline static void init_buffer_cleanup(Synth* self)
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

static void init_buffer(const Methcla_Host* host, void* data)
{
    std::cout << "init_buffer\n";
    Synth* self = (Synth*)data;

    Methcla_SoundFileInfo info;

    Methcla_FileError err = methcla_host_soundfile_open(
        host,
        self->path,
        kMethcla_Read,
        &self->state.file,
        &info);

    if (err == kMethcla_FileNoError) {
        self->state.channels = info.channels;
        self->state.frames = self->state.frames <= 0 ? info.frames : std::min<int64_t>(self->state.frames, info.frames);
        const size_t transferFrames = bytesToFrames(self->state.channels, kDiskTransferSize);
        if (self->state.frames <= transferFrames) {
            self->state.bufferFrames = self->state.frames;
            self->state.transferFrames = 0;
            self->state.buffer = (float*)malloc(framesToBytes(self->state.channels, self->state.bufferFrames));
            if (self->state.buffer == nullptr) {
                init_buffer_cleanup(self);
            } else {
                size_t numFrames = 0;
                Methcla_FileError err = methcla_soundfile_read_float(
                    self->state.file, self->state.buffer, self->state.bufferFrames, &numFrames);
                if (err == kMethcla_FileNoError && numFrames == self->state.bufferFrames) {
                    self->state.state.store(kMemoryPlayback, std::memory_order_release);
                } else {
                    init_buffer_cleanup(self);
                }
            }
        } else {
            self->state.bufferFrames = transferFrames * 4;
            self->state.transferFrames = transferFrames;
            self->state.buffer = (float*)malloc(framesToBytes(self->state.channels, self->state.bufferFrames));
            if (self->state.buffer == nullptr) {
                init_buffer_cleanup(self);
            } else {
                size_t numFrames = 0;
                Methcla_FileError err = methcla_soundfile_read_float(
                    self->state.file, self->state.buffer, self->state.transferFrames, &numFrames);
                if (err == kMethcla_FileNoError && numFrames == self->state.transferFrames) {
                    self->state.writePos.store(numFrames, std::memory_order_relaxed);
                    self->state.state.store(kIdle, std::memory_order_release);
                } else {
                    init_buffer_cleanup(self);
                }
            }
        }
    } else {
        init_buffer_cleanup(self);
    }
}

static void
construct( const Methcla_World* world
         , const Methcla_SynthDef* synthDef
         , const Methcla_SynthOptions* inOptions
         , Methcla_Synth* synth )
{
    const Options* options = (const Options*)inOptions;

    Synth* self = (Synth*)synth;

    self->path = (char*)methcla_world_alloc(world, strlen(options->path)+1);
    if (self->path == nullptr) return;
    strcpy(self->path, options->path);

    self->loop = options->loop;

    new (&self->state) State();
    self->state.frames = options->frames;

    // TODO: Need to retain a reference to the outer Synth, otherwise the self pointer might dangle if the synth is freed while the async command is still being executed. Same goes for fill_buffer
    methcla_world_perform_command(world, init_buffer, self);
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
    Synth* self = (Synth*)synth;
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
       , size_t index
       , void* data )
{
    ((Synth*)synth)->ports[index] = (float*)data;
}

inline static void
process_disk(Synth* self, size_t numFrames, float amp, const float* buffer, float* out0, float* out1)
{
    assert( self->state.file != nullptr );

    // Check readable
    // if state.readable < numFrames
    // then output what's there, zero the rest and drop the corresponding amount the next time around?
    const size_t readable = std::min(numFrames, self->state.readable());
    const size_t readPos = self->state.readPos;
    const size_t readable1 = std::min(readable, self->state.bufferFrames - readPos);
    const size_t readable2 = readable - readable1;
    const size_t channels = self->state.channels;

    if (readable < numFrames) {
        std::cerr << "process_disk: underrun " << numFrames - readable << "\n";
    }

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
            const size_t j = k*channels;
            const size_t m = readable1+k;
            out0[m] = amp * buffer[j];
            out1[m] = amp * buffer[j+1];
        }
        for (size_t k = readable; k < numFrames; k++) {
            out0[k] = out1[k] = 0.f;
        }
    }

    const size_t nextReadPos = readable2 > 0 ? readable2 : readPos + readable1;
    self->state.readPos.store(nextReadPos == self->state.bufferFrames ? 0 : nextReadPos, std::memory_order_relaxed);
}

inline static void
process_memory(Synth* self, size_t numFrames, float amp, const float* buffer, float* out0, float* out1)
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
            toPlay = std::min(numFrames - played, self->state.frames);
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
    Synth* self = (Synth*)synth;

    const float amp = *self->ports[kSampler_amp];
    float* out0 = self->ports[kSampler_output_0];
    float* out1 = self->ports[kSampler_output_1];
    const float* buffer = self->state.buffer;

    int state = self->state.state.load(std::memory_order_consume);
    switch (state) {
        case kIdle:
        if (self->state.writable() >= self->state.transferFrames) {
            // Trigger refill
            self->state.state.store(kFilling, std::memory_order_relaxed);
            methcla_world_perform_command(world, fill_buffer, self);
        }
        case kFilling:
        process_disk(self, numFrames, amp, buffer, out0, out1);
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
    sizeof(Synth),
    sizeof(Options),
    configure,
    port_descriptor,
    construct,
    connect,
    nullptr,
    process,
    destroy
};

static const Methcla_Library library = { NULL, NULL };

METHCLA_EXPORT const Methcla_Library* methcla_plugins_disksampler(const Methcla_Host* host, const char* bundlePath)
{
    methcla_host_register_synthdef(host, &descriptor);
    return &library;
}
