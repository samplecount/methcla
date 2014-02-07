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

#include <methcla/plugins/sampler.h>

#include <cmath>
#include <oscpp/server.hpp>

namespace
{

typedef enum {
    kSampler_amp,
    kSampler_rate,
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
    double phase;
} Synth;

struct Options
{
    const char* path;
    bool loop;
    size_t startFrame;
    size_t numFrames;
};

struct LoadMessage
{
    Synth* synth;
    size_t numChannels;
    int64_t startFrame;
    int64_t numFrames;
    float* buffer;
    char* path;
};

// Declare callback with C linkage
extern "C"
{
    static bool
    port_descriptor( const Methcla_SynthOptions*,
                     Methcla_PortCount,
                     Methcla_PortDescriptor* );
    static void
    configure( const void*, size_t,
               const void*, size_t,
               Methcla_SynthOptions* );

    static void
    construct( const Methcla_World*,
               const Methcla_SynthDef*,
               const Methcla_SynthOptions*,
               Methcla_Synth* );

    static void
    destroy( const Methcla_World*,
             Methcla_Synth* );

    static void
    connect( Methcla_Synth*,
             Methcla_PortCount,
             void* );

    static void
    process( const Methcla_World*,
             Methcla_Synth*,
             size_t );
}

bool
port_descriptor( const Methcla_SynthOptions* /* options */
               , Methcla_PortCount index
               , Methcla_PortDescriptor* port )
{
    switch ((PortIndex)index) {
        case kSampler_amp:
        case kSampler_rate:
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

static void
configure(const void* tags, size_t tags_size, const void* args, size_t args_size, Methcla_SynthOptions* outOptions)
{
    OSCPP::Server::ArgStream argStream(OSCPP::ReadStream(tags, tags_size), OSCPP::ReadStream(args, args_size));
    Options* options = (Options*)outOptions;
    options->path = argStream.string();
    options->loop = argStream.atEnd() ? false : argStream.int32();
    options->startFrame = argStream.atEnd() ? 0 : std::max(0, argStream.int32());
    options->numFrames = argStream.atEnd() ? -1 : std::max(0, argStream.int32());
}

static void set_buffer(const Methcla_World* world, void* data)
{
    LoadMessage* msg = (LoadMessage*)data;
    msg->synth->buffer = msg->buffer;
    msg->synth->channels = msg->numChannels;
    msg->synth->frames = msg->numFrames;
    methcla_world_free(world, msg);
}

static void load_sound_file(const Methcla_Host* context, void* data)
{
    LoadMessage* msg = (LoadMessage*)data;
    assert( msg != nullptr );

    Methcla_SoundFile* file = nullptr;
    Methcla_SoundFileInfo info;
    memset(&info, 0, sizeof(info));

    Methcla_Error err = methcla_host_soundfile_open(context, msg->path, kMethcla_FileModeRead, &file, &info);

    if (err == kMethcla_NoError)
    {
        msg->startFrame = std::min(std::max(int64_t(0), msg->startFrame), info.frames);
        msg->numFrames = msg->numFrames < 0
                            ? info.frames - msg->startFrame
                            : std::min(msg->numFrames, info.frames - msg->startFrame);
        msg->numChannels = info.channels;

        if (msg->numFrames > 0)
        {
            msg->buffer = (float*)methcla_host_alloc(context, msg->numChannels * msg->numFrames * sizeof(float));

            // TODO: error handling
            if (msg->buffer != nullptr)
            {
                methcla_soundfile_seek(file, msg->startFrame);
                size_t numFrames;
                err = methcla_soundfile_read_float(file, msg->buffer, msg->numFrames, &numFrames);
            }
        }
        else
        {
            msg->buffer = nullptr;
        }

        methcla_soundfile_close(file);
    }

    methcla_host_perform_command(context, set_buffer, msg);
}

static void free_buffer_cb(const Methcla_Host* context, void* data)
{
    methcla_host_free(context, data);
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
         , const Methcla_SynthDef* /* synthDef */
         , const Methcla_SynthOptions* inOptions
         , Methcla_Synth* synth )
{
    const Options* options = (const Options*)inOptions;

    Synth* self = (Synth*)synth;
    self->buffer = nullptr;
    self->channels = 0;
    self->frames = 0;
    self->loop = options->loop;
    self->phase = 0.;

    LoadMessage* msg = (LoadMessage*)methcla_world_alloc(world, sizeof(LoadMessage) + strlen(options->path)+1);
    msg->synth = self;
    msg->numChannels = 0;
    msg->startFrame = options->startFrame;
    msg->numFrames = options->numFrames;
    msg->path = (char*)msg + sizeof(LoadMessage);
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
       , Methcla_PortCount index
       , void* data )
{
    ((Synth*)synth)->ports[index] = (float*)data;
}

static inline void
process_no_interp(
    const Methcla_World* world,
    Synth* self,
    size_t numFrames,
    float amp,
    float /* rate */,
    const float* buffer,
    float* out0,
    float* out1 )
{
    size_t pos = (size_t)self->phase;
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
            if (self->loop) self->phase = 0;
            else freeBuffer(world, self);
        } else {
            self->phase = pos + numFrames;
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
        self->phase = pos;
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
}

static inline float hermite1(float x, float y0, float y1, float y2, float y3)
{
    // 4-point, 3rd-order Hermite (x-form)
    const float c0 = y1;
    const float c1 = 0.5f * (y2 - y0);
    const float c2 = y0 - 2.5f * y1 + 2.f * y2 - 0.5f * y3;
    const float c3 = 1.5f * (y1 - y2) + 0.5f * (y3 - y0);

    return ((c3 * x + c2) * x + c1) * x + c0;
}

template <bool wrapInterp, bool wrapPhase> inline size_t resample(float* out0, float* out1, size_t numFrames, const float* buffer, size_t bufferChannels, size_t bufferFrames, size_t bufferEnd, float amp, float rate, double& phase)
{
    const size_t bufferChannel1 = 0;
    const size_t bufferChannel2 = bufferChannels > 1 ? 1 : 0;
    const double maxPhase = (double)bufferFrames;

    size_t k;

    for (k=0; k < numFrames; k++)
    {
        const double findex = std::floor(phase);
        const size_t index = (size_t)findex;

        const float* xm;
        const float* x0;
        const float* x1;
        const float* x2;

        if (index == 0)
        {
            if (wrapInterp)
                xm = buffer + (bufferFrames - 1) * bufferChannels;
            else
                xm = buffer;
        }
        else
        {
            xm = buffer + (index - 1) * bufferChannels;
        }

        if (index < bufferEnd - 2)
        {
            x0 = buffer + index * bufferChannels;
            x1 = x0 + bufferChannels;
            x2 = x1 + bufferChannels;
        }
        else if (index < bufferEnd - 1)
        {
            x0 = buffer + index * bufferChannels;
            x1 = x0 + bufferChannels;
            if (wrapInterp)
                x2 = buffer;
            else
                x2 = x1;
        }
        else if (index < bufferEnd)
        {
            x0 = buffer + index * bufferChannels;
            if (wrapInterp)
            {
                x1 = buffer;
                x2 = buffer + bufferChannels;
            }
            else
            {
                x1 = x0;
                x2 = x0;
            }
        }
        else
        {
            break;
        }

        const double x = phase - findex;

        out0[k] = amp * hermite1(x, xm[bufferChannel1], x0[bufferChannel1], x1[bufferChannel1], x2[bufferChannel1]);
        out1[k] = amp * hermite1(x, xm[bufferChannel2], x0[bufferChannel2], x1[bufferChannel2], x2[bufferChannel2]);

        phase += rate;

        if (wrapPhase && phase >= maxPhase)
            phase -= maxPhase;
    }

    return k;
}

static inline void
process_interp(
    const Methcla_World* world,
    Synth* self,
    size_t numFrames,
    float amp,
    float rate,
    const float* buffer,
    float* out0,
    float* out1 )
{
    const size_t bufferFrames = self->frames;
    const size_t bufferChannels = self->channels;
    double phase = self->phase;

    if (self->loop)
    {
        size_t numFramesProduced = 0;

        while (numFramesProduced < numFrames)
        {
            numFramesProduced += resample<true,true>(
                out0 + numFramesProduced,
                out1 + numFramesProduced,
                numFrames - numFramesProduced,
                buffer,
                bufferChannels,
                bufferFrames,
                bufferFrames,
                amp,
                rate,
                phase
            );
        }

        assert(numFramesProduced == numFrames);
    }
    else
    {
        const size_t numFramesProduced = resample<false,false>(
            out0,
            out1,
            numFrames,
            buffer,
            bufferChannels,
            bufferFrames,
            bufferFrames,
            amp,
            rate,
            phase
        );

        if (numFramesProduced < numFrames)
        {
            for (size_t k = numFramesProduced; k < numFrames; k++)
            {
                out0[k] = out1[k] = 0.f;
            }
            freeBuffer(world, self);
        }
    }

    self->phase = phase;
}

static void
process(const Methcla_World* world, Methcla_Synth* synth, size_t numFrames)
{
    Synth* self = (Synth*)synth;
    float* out0 = self->ports[kSampler_output_0];
    float* out1 = self->ports[kSampler_output_1];
    const float* buffer = self->buffer;

    if (buffer)
    {
        const float amp = *self->ports[kSampler_amp];
        const float rate = *self->ports[kSampler_rate];
        process_interp(world, self, numFrames, amp, rate, buffer, out0, out1);
    }
    else
    {
        for (size_t k = 0; k < numFrames; k++) {
            out0[k] = out1[k] = 0.f;
        }
    }
}

static const Methcla_SynthDef descriptor =
{
    METHCLA_PLUGINS_SAMPLER_URI,
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

} // namespace

METHCLA_EXPORT const Methcla_Library* methcla_plugins_sampler(const Methcla_Host* host, const char* /* bundlePath */)
{
    methcla_host_register_synthdef(host, &descriptor);
    return &library;
}
