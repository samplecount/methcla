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

#include <methcla/plugin.hpp>
#include <methcla/plugins/patch_cable.h>

#include <algorithm>
#include <cmath>
#include <iostream>

using namespace Methcla::Plugin;

typedef enum
{
    kPort_in,
    kPort_out,
    kNumPorts
} PortIndex;

typedef struct
{
    float* ports[kNumPorts];
} PatchCable;

static const Methcla_PortDescriptor kPortDescriptors[] = {
    {kMethcla_Input, kMethcla_AudioPort, kMethcla_PortFlags},
    {kMethcla_Output, kMethcla_AudioPort, kMethcla_PortFlags}};

static bool port_descriptor(const Methcla_SynthOptions*,
                            Methcla_PortCount       index,
                            Methcla_PortDescriptor* port)
{
    if (index < kNumPorts)
    {
        *port = kPortDescriptors[index];
        return true;
    }
    return false;
}

static void construct(Methcla_World*, const Methcla_SynthDef*,
                      const Methcla_SynthOptions*, Methcla_Synth* synth)
{
    PatchCable* self = static_cast<PatchCable*>(synth);
    std::fill(self->ports, self->ports + kNumPorts, nullptr);
}

static void connect(Methcla_Synth* synth, Methcla_PortCount port, void* data)
{
    PatchCable* self = static_cast<PatchCable*>(synth);
    self->ports[port] = static_cast<float*>(data);
}

static void process(Methcla_World*, Methcla_Synth* synth, size_t numFrames)
{
    PatchCable* self = static_cast<PatchCable*>(synth);

    const float* src = self->ports[kPort_in];
    float*       dst = self->ports[kPort_out];

    // float rms = 0;
    // for (size_t i=0; i < numFrames; i++) {
    //     rms += src[i] * src[i];
    // }
    // rms = sqrt(rms);
    // std::cout << "patch_cable " << rms << "\n";

    std::copy(src, src + numFrames, dst);
}

static const Methcla_SynthDef descriptor = {METHCLA_PLUGINS_PATCH_CABLE_URI,
                                            sizeof(PatchCable),
                                            0,
                                            NULL,
                                            port_descriptor,
                                            construct,
                                            connect,
                                            NULL,
                                            process,
                                            NULL};

// Amplifier

namespace {

    typedef NoOptions AmplifierOptions;

    class AmplifierPorts
    {
    public:
        enum Port
        {
            kInput,
            kOutput,
            kGain
        };

        static constexpr size_t numPorts()
        {
            return 3;
        }

        static Methcla_PortDescriptor descriptor(Port port)
        {
            switch (port)
            {
                case kInput:
                    return Methcla::Plugin::PortDescriptor::audioInput();
                case kOutput:
                    return Methcla::Plugin::PortDescriptor::audioOutput();
                case kGain:
                    return Methcla::Plugin::PortDescriptor::controlInput();
                default:
                    throw std::runtime_error(METHCLA_PLUGINS_AMPLIFIER_URI
                                             ": Invalid port index");
            }
        }
    };

    class Amplifier
    {
        float* m_ports[AmplifierPorts::numPorts()];

    public:
        Amplifier(const World<Amplifier>&, const Methcla_SynthDef*,
                  const AmplifierOptions&)
        {}

        void connect(AmplifierPorts::Port port, void* data)
        {
            m_ports[port] = static_cast<float*>(data);
        }

        void process(const World<Amplifier>&, size_t numFrames)
        {
            const float* const input = m_ports[AmplifierPorts::kInput];
            float*             output = m_ports[AmplifierPorts::kOutput];
            const float        gain = *m_ports[AmplifierPorts::kGain];

            if (gain == 0.0f)
            {
                for (size_t i = 0; i < numFrames; i++)
                {
                    output[i] = 0.0f;
                }
            }
            else if (gain == 1.f)
            {
                for (size_t i = 0; i < numFrames; i++)
                {
                    output[i] = input[i];
                }
            }
            else
            {
                for (size_t i = 0; i < numFrames; i++)
                {
                    output[i] = gain * input[i];
                }
            }
        }
    };

    StaticSynthDef<Amplifier, AmplifierOptions, AmplifierPorts> kAmplifierDef;

}; // namespace

// Library initialization

static Methcla_Library library = {NULL, NULL};

METHCLA_EXPORT Methcla_Library* METHCLA_PLUGIN_LOAD(
    methcla_plugins_patch_cable)(Methcla_Host* host,
                                 const char* /* bundlePath */)
{
    methcla_host_register_synthdef(host, &descriptor);
    kAmplifierDef(host, METHCLA_PLUGINS_AMPLIFIER_URI);
    return &library;
}
