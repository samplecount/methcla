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

#include <methcla/plugins/patch-cable.h>

#include <algorithm>
#include <cmath>
#include <iostream>

typedef enum {
    kPort_in,
    kPort_out,
    kNumPorts
} PortIndex;

typedef struct {
    float* ports[kNumPorts];
} PatchCable;

static const Methcla_PortDescriptor kPortDescriptors[] = {
    { kMethcla_Input, kMethcla_AudioPort, kMethcla_PortFlags },
    { kMethcla_Output, kMethcla_AudioPort, kMethcla_PortFlags }
};

static bool
port_descriptor( const Methcla_SynthOptions*
               , Methcla_PortCount index
               , Methcla_PortDescriptor* port )
{
    if (index < kNumPorts) {
        *port = kPortDescriptors[index];
        return true;
    }
    return false;
}

static void
construct( const Methcla_World*
         , const Methcla_SynthDef*
         , const Methcla_SynthOptions*
         , Methcla_Synth* synth )
{
    PatchCable* self = static_cast<PatchCable*>(synth);
    std::fill(self->ports, self->ports + kNumPorts, nullptr);
}

static void
connect( Methcla_Synth* synth
       , Methcla_PortCount port
       , void* data)
{
    PatchCable* self = static_cast<PatchCable*>(synth);
    self->ports[port] = static_cast<float*>(data);
}

static void
process(const Methcla_World*, Methcla_Synth* synth, size_t numFrames)
{
    PatchCable* self = static_cast<PatchCable*>(synth);

    const float* src = self->ports[kPort_in];
    float* dst = self->ports[kPort_out];

    // float rms = 0;
    // for (size_t i=0; i < numFrames; i++) {
    //     rms += src[i] * src[i];
    // }
    // rms = sqrt(rms);
    // std::cout << "patch_cable " << rms << "\n";

    std::copy(src, src + numFrames, dst);
}

static const Methcla_SynthDef descriptor =
{
    METHCLA_PLUGINS_PATCH_CABLE_URI,
    sizeof(PatchCable),
    0, NULL,
    port_descriptor,
    construct,
    connect,
    NULL,
    process,
    NULL
};

static const Methcla_Library library = { NULL, NULL };

METHCLA_EXPORT const Methcla_Library* methcla_plugins_patch_cable(const Methcla_Host* host, const char* /* bundlePath */)
{
    methcla_host_register_synthdef(host, &descriptor);
    return &library;
}
