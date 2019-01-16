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

#include <methcla/plugins/sine.h>

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* M_PI is not in C11 */
static const double kPi = 3.14159265358979323846264338327950288;

typedef enum
{
    kSine_freq,
    kSine_amp,
    kSine_out,
    kSinePorts
} PortIndex;

typedef struct
{
    float* ports[kSinePorts];
    double phase;
    double freqToPhaseInc;
} Sine;

static const Methcla_PortDescriptor kPortDescriptors[] = {
    {.type = kMethcla_ControlPort,
     .direction = kMethcla_Input,
     .flags = kMethcla_PortFlags},
    {.type = kMethcla_ControlPort,
     .direction = kMethcla_Input,
     .flags = kMethcla_PortFlags},
    {.type = kMethcla_AudioPort,
     .direction = kMethcla_Output,
     .flags = kMethcla_PortFlags}};

static bool port_descriptor(const Methcla_SynthOptions* options,
                            Methcla_PortCount           index,
                            Methcla_PortDescriptor*     port)
{
    if (index < kSinePorts)
    {
        *port = kPortDescriptors[index];
        return true;
    }
    return false;
}

// static void print_freq(const Methcla_Host* host, void* data)
// {
//     Sine* sine = (Sine*)data;
//     fprintf(stderr, "SINE_FREQ [NRT]: %f\n", *sine->ports[kSine_freq]);
// }

static void construct(Methcla_World* world, const Methcla_SynthDef* synthDef,
                      const Methcla_SynthOptions* options, Methcla_Synth* synth)
{
    Sine* sine = (Sine*)synth;
    sine->phase = 0.;
    sine->freqToPhaseInc = 2. * kPi / methcla_world_samplerate(world);
    // methcla_world_perform_command(world, print_freq, sine);
}

static void connect(Methcla_Synth* synth, Methcla_PortCount port, void* data)
{
    ((Sine*)synth)->ports[port] = (float*)data;
}

static void process(Methcla_World* world, Methcla_Synth* synth,
                    size_t numFrames)
{
    Sine* sine = (Sine*)synth;

    const float  freq = *sine->ports[kSine_freq];
    const float  amp = *sine->ports[kSine_amp];
    double       phase = sine->phase;
    const double phaseInc = freq * sine->freqToPhaseInc;
    float* const output = sine->ports[kSine_out];

    for (size_t k = 0; k < numFrames; k++)
    {
        output[k] = amp * sin(phase);
        phase += phaseInc;
    }

    sine->phase = phase;
}

static const Methcla_SynthDef descriptor = {METHCLA_PLUGINS_SINE_URI,
                                            sizeof(Sine),
                                            0,
                                            NULL,
                                            port_descriptor,
                                            construct,
                                            connect,
                                            NULL,
                                            process,
                                            NULL};

static Methcla_Library library = {NULL, NULL};

METHCLA_EXPORT Methcla_Library* methcla_plugins_sine(Methcla_Host* host,
                                                     const char*   bundlePath)
{
    methcla_host_register_synthdef(host, &descriptor);
    return &library;
}
