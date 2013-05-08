/*
    Copyright 2012-2013 Samplecount S.L.

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

        http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.
*/

#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "methc.la/plugins/sine/sine.h"

typedef enum {
    SINE_FREQ,
    SINE_OUTPUT,
    kSineNumPorts
} PortIndex;

typedef struct {
    float* freq;
    float* output;
    double phase;
    double freqToPhaseInc;
} Sine;

static bool
sine_port( const Methcla_SynthDef* synthDef
         , size_t index
         , Methcla_Port* port )
{
    switch ((PortIndex)index) {
        case SINE_FREQ:
            port->type = kMethcla_ControlPort;
            port->direction = kMethcla_Input;
            port->flags = kMethcla_PortFlags;
            return true;
        case SINE_OUTPUT:
            port->type = kMethcla_AudioPort;
            port->direction = kMethcla_Output;
            port->flags = kMethcla_PortFlags;
            return true;
        default:
            return false;
    }
}
static void
sine_construct( const Methcla_SynthDef* synthDef
         , const Methcla_World* world
         , Methcla_Synth* synth )
{
    Sine* sine = (Sine*)synth;
    sine->phase = 0.;
    sine->freqToPhaseInc = 2.*M_PI/methcla_world_samplerate(world);
}

static void
sine_connect( Methcla_Synth* synth
       , size_t port
       , void* data)
{
    Sine* sine = (Sine*)synth;

    switch ((PortIndex)port) {
        case SINE_FREQ:
            sine->freq = (float*)data;
            // *sine->freq = (float)rand() / (float)RAND_MAX * 400 + 200;
            // fprintf(stderr, "SINE_FREQ: %f\n", *sine->freq);
            break;
        case SINE_OUTPUT:
            sine->output = (float*)data;
            break;
        default:
            break;
    }
}

static void

sine_process(Methcla_Synth* synth, size_t numFrames)
{
    Sine* sine = (Sine*)synth;

    const float  freq     = *sine->freq;
    double phase          = sine->phase;
    const double phaseInc = freq * sine->freqToPhaseInc;
    float* const output   = sine->output;

    for (uint32_t k = 0; k < numFrames; k++) {
        output[k] = sin(phase) * 0.05f;
        phase += phaseInc;
    }

    sine->phase = phase;
}

static const Methcla_SynthDef descriptor = {
    METHCLA_PLUGINS_SINE_URI,
    sizeof(Sine),
    sine_port,
    sine_construct,
    sine_connect,
    sine_process,
    NULL
};

static const Methcla_Library library = { NULL, NULL };

METHCLA_EXPORT const Methcla_Library* methcla_plugins_sine(const Methcla_Host* host, const char* bundlePath)
{
    methcla_host_register_synthdef(host, &descriptor);
    return &library;
}
