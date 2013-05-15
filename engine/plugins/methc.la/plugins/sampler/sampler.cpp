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

#include "methc.la/plugins/sampler/sampler.h"

#include <iostream>
#include <oscpp/server.hpp>
#include <unistd.h>

typedef enum {
    kSampler_amp,
    kSampler_output_0,
    kSampler_output_1,
    kSamplerPorts
} PortIndex;

typedef struct {
    float* ports[kSamplerPorts];
    float* buffer;
    size_t channels;
    size_t frames;
    bool loop;
    size_t pos;
} Synth;

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
    int32_t numFrames;
};

static void
configure(const void* tags, size_t tags_size, const void* args, size_t args_size, Methcla_SynthOptions* outOptions)
{
    OSC::Server::ArgStream argStream(OSC::ReadStream(tags, tags_size), OSC::ReadStream(args, args_size));
    Options* options = (Options*)outOptions;
    options->path = argStream.string();
    options->loop = argStream.atEnd() ? false : argStream.int32();
    options->numFrames = argStream.atEnd() ? -1 : argStream.int32();
    std::cout << "Sampler: "
              << options->path << " "
              << options->loop << " "
              << options->numFrames << "\n";
}

struct LoadMessage
{
    Synth* synth;
    size_t numChannels;
    int64_t numFrames;
    float* buffer;
    char path[];
};

static void set_buffer(const Methcla_World* world, const Methcla_CommandChannel* channel, void* data)
{
    std::cout << "set_buffer\n";
    LoadMessage* msg = (LoadMessage*)data;
    msg->synth->buffer = msg->buffer;
    msg->synth->channels = msg->numChannels;
    msg->synth->frames = msg->numFrames;
    methcla_world_free(world, msg);
}

static void load_sound_file(const Methcla_World* world, const Methcla_CommandChannel* channel, void* data)
{
    std::cout << "load_sound_file\n";
    LoadMessage* msg = (LoadMessage*)data;

    Methcla_SoundFile* file;
    Methcla_SoundFileInfo info;
    Methcla_FileError err = methcla_host_soundfile_open(methcla_world_host(world), msg->path, kMethcla_Read, &file, &info);

    if (err == kMethcla_FileNoError) {
        msg->numFrames = msg->numFrames < 0 ? info.frames : std::min<int64_t>(msg->numFrames, info.frames);
        msg->numChannels = info.channels;
        msg->buffer = (float*)malloc(msg->numChannels * msg->numFrames * sizeof(float));
        std::cout << "load_sound_file: " << msg->path << " " << info.channels << " " << info.frames << "\n";
        // TODO: error handling
        if (msg->buffer != nullptr) {
            size_t numFrames;
            err = methcla_soundfile_read_float(file, msg->buffer, msg->numFrames, &numFrames);
        }
        methcla_soundfile_close(file);
    }

    methcla_command_channel_send(channel, set_buffer, msg);
}

static void free_buffer_cb(const Methcla_World* world, const Methcla_CommandChannel* channel, void* data)
{
    std::cout << "free_buffer\n";
    free(data);
}

static void freeBuffer(const Methcla_World* world, Synth* self)
{
    if (self->buffer) {
        methcla_world_perform_command(world, free_buffer_cb, self->buffer);
        self->buffer = nullptr;
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
    self->buffer = nullptr;
    self->channels = 0;
    self->frames = 0;
    self->loop = options->loop;
    self->pos = 0;

    LoadMessage* msg = (LoadMessage*)methcla_world_alloc(world, sizeof(LoadMessage) + strlen(options->path)+1);
    msg->synth = self;
    msg->numFrames = options->numFrames;
    strcpy(msg->path, options->path);

    methcla_world_perform_command(world, load_sound_file, msg);
}

static void
destroy(const Methcla_World* world, Methcla_Synth* synth)
{
    Synth* self = (Synth*)synth;
    freeBuffer(world, self);
}

static void
connect( Methcla_Synth* synth
       , size_t index
       , void* data )
{
    ((Synth*)synth)->ports[index] = (float*)data;
}

static void
process(const Methcla_World* world, Methcla_Synth* synth, size_t numFrames)
{
    Synth* self = (Synth*)synth;

    const float amp = *self->ports[kSampler_amp];
    float* out0 = self->ports[kSampler_output_0];
    float* out1 = self->ports[kSampler_output_1];

    float* buffer = self->buffer;
    if (buffer) {
        size_t pos = self->pos;
        const size_t left = self->frames - pos;
        const size_t channels = self->channels;

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
                if (self->loop) self->pos = 0;
                else freeBuffer(world, self);
            } else {
                self->pos = pos + numFrames;
            };
        } else if (self->loop) {
            size_t played = 0;
            size_t toPlay = left;
            while (played < numFrames) {
                if (channels == 1) {
                    for (size_t k = 0; k < toPlay; k++) {
                        const size_t m = played+k;
                        const size_t j = (pos+k)*channels;
                        out0[m] = out1[m] = amp * buffer[j];
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
                if (pos >= self->frames)
                    pos = 0;
                toPlay = std::min(numFrames - played, self->frames);
            }
            self->pos = pos;
        } else {
            if (channels == 1) {
                for (size_t k = 0; k < left; k++) {
                    const size_t j = (pos+k)*channels;
                    out0[k] = out1[k] = amp * buffer[j];
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
            freeBuffer(world, self);
        }
    } else {
        for (size_t k = 0; k < numFrames; k++) {
            out0[k] = out1[k] = 0.f;
        }
    }
}

static const Methcla_SynthDef descriptor =
{
    nullptr,
    METHCLA_PLUGINS_SAMPLER_URI,
    sizeof(Synth),
    sizeof(Options),
    configure,
    port_descriptor,
    construct,
    connect,
    nullptr,
    process,
    destroy,
    nullptr
};

static const Methcla_Library library = { NULL, NULL };

METHCLA_EXPORT const Methcla_Library* methcla_plugins_sampler(const Methcla_Host* host, const char* bundlePath)
{
    methcla_host_register_synthdef(host, &descriptor);
    return &library;
}
