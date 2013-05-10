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

typedef enum {
    kSampler_amp,
    kSampler_output_0,
    kSampler_output_1
} PortIndex;

typedef struct {
    float* amp;
    float* output[2];
} Synth;

static bool
port_descriptor( const Methcla_SynthOptions* options
               , size_t index
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

static void command_callback(const void* data, const Methcla_World* world, const Methcla_CommandChannel* channel)
{
}

static void
construct( const Methcla_World* world
         , const Methcla_SynthDef* synthDef
         , const Methcla_SynthOptions* options
         , Methcla_Synth* synth )
{
    Synth* self = (Synth*)synth;
    // methcla_world_perform_command(world, sine_print_freq, sine);
}

static void
connect( Methcla_Synth* synth
       , size_t index
       , void* data )
{
    Synth* self = (Synth*)synth;

    switch ((PortIndex)index) {
        case kSampler_amp:
            self->amp = (float*)data;
            break;
        case kSampler_output_0:
            self->output[0] = (float*)data;
            break;
        case kSampler_output_1:
            self->output[1] = (float*)data;
            break;
        default:
            break;
    }
}

static void
process(const Methcla_World* world, Methcla_Synth* synth, size_t numFrames)
{
    Synth* self = (Synth*)synth;

    const float amp = *self->amp;
    float** output = self->output;
    
    for (size_t k = 0; k < numFrames; k++) {
        output[0][k] = 0.f;
    }
    for (size_t k = 0; k < numFrames; k++) {
        output[1][k] = 0.f;
    }
    
    // sine->phase = phase;
}

static const Methcla_SynthDef descriptor =
{
    nullptr,
    METHCLA_PLUGINS_SAMPLER_URI,
    sizeof(Synth),
    0, nullptr,
    port_descriptor,
    construct,
    connect,
    nullptr,
    process,
    nullptr,
    nullptr
};

static const Methcla_Library library = { NULL, NULL };

METHCLA_EXPORT const Methcla_Library* methcla_plugins_sampler(const Methcla_Host* host, const char* bundlePath)
{
    methcla_host_register_synthdef(host, &descriptor);
    return &library;
}
