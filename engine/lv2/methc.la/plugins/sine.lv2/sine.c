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

#include "lv2/lv2plug.in/ns/lv2core/lv2.h"
#include "lv2/methc.la/ext/rt-instantiate/rt-instantiate.h"

#define SINE_URI "http://methc.la/lv2/plugins/sine"

typedef enum {
    SINE_FREQ   = 0,
    SINE_OUTPUT = 1
} PortIndex;

typedef struct {
    float* freq;
    float* output;
    double phase;
    double freqToPhaseInc;
} Sine;

static uint32_t instance_size(const LV2_Descriptor* descriptor)
{
    return sizeof(Sine);
}

static LV2_Handle
instantiate( const LV2_Descriptor*     descriptor
           , double                    sampleRate
           , const char*               bundlePath
           , const LV2_Feature* const* features
           , void*                     location )
{
    Sine* sine = (Sine*)location;
    sine->freqToPhaseInc = 2.*M_PI/sampleRate;
    return (LV2_Handle)sine;
}

static void
activate(LV2_Handle instance)
{
    Sine* sine = (Sine*)instance;
    sine->phase = 0;
}

static void
connect_port(LV2_Handle instance,
             uint32_t   port,
             void*      data)
{
    Sine* sine = (Sine*)instance;

    switch ((PortIndex)port) {
    case SINE_FREQ:
        sine->freq = (float*)data;
        break;
    case SINE_OUTPUT:
        sine->output = (float*)data;
        break;
    }
}

static void
run(LV2_Handle instance, uint32_t numFrames)
{
    Sine* sine = (Sine*)instance;

    const float  freq     = *sine->freq;
    double phase          = sine->phase;
    const double phaseInc = freq * sine->freqToPhaseInc;
    float* const output   = sine->output;

    for (uint32_t k = 0; k < numFrames; k++) {
        output[k] = sin(phase);
        phase += phaseInc;
    }
    
    sine->phase = phase;
}

static void
deactivate(LV2_Handle instance)
{
}

static void
cleanup(LV2_Handle instance)
{
    free(instance);
}

static const LV2_RT_Instantiate_Interface rtiInterface = {
    instance_size
  , instantiate
};

const void*
extension_data(const char* uri)
{
    if (strcmp(uri, LV2_RT_INSTANTIATE__INTERFACE) == 0) {
        return &rtiInterface;
    }
    return NULL;
}

static const LV2_Descriptor descriptor = {
    SINE_URI,
    lv2_rt_instantiate_default_instantiate,
    connect_port,
    activate,
    run,
    deactivate,
    cleanup,
    extension_data
};

// LV2_SYMBOL_EXPORT
METHCLA_EXPORT const LV2_Descriptor* methcla_lv2_plugins_sine_lv2_descriptor(uint32_t index) __attribute__((used));

const LV2_Descriptor* methcla_lv2_plugins_sine_lv2_descriptor(uint32_t index)
{
    switch (index) {
    case 0:
        return &descriptor;
    default:
        return NULL;
    }
}

// volatile __attribute__((used)) LV2_Descriptor_Function dummy = methcla_lv2_plugins_sine_lv2_descriptor;
volatile void* fuckMe = (void*)methcla_lv2_plugins_sine_lv2_descriptor;
