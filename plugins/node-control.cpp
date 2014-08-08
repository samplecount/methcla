// Copyright 2013 Samplecount S.L.
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

#include <methcla/plugins/node-control.h>
#include <methcla/plugin.hpp>

using namespace Methcla::Plugin;

// DoneAfter

class DoneAfterOptions
{
public:
    DoneAfterOptions(OSCPP::Server::ArgStream args)
    {
        m_seconds = args.float32();
    }

    float m_seconds;
};

typedef NoPorts DoneAfterPorts;

class DoneAfter
{
    double m_numFramesLeft;
    bool   m_done;

public:
    DoneAfter(const World<DoneAfter>& world, const Methcla_SynthDef*, const DoneAfterOptions& options)
        : m_numFramesLeft((double)options.m_seconds * world.sampleRate())
        , m_done(false)
    {
    }

    void connect(DoneAfterPorts::Port, void*)
    {
    }

    void process(const World<DoneAfter>& world, size_t numFrames)
    {
        if (!m_done)
        {
            m_numFramesLeft -= numFrames;
            if (m_numFramesLeft <= 0)
            {
                m_done = true;
                world.synthDone(this);
            }
        }
    }
};

StaticSynthDef<DoneAfter,DoneAfterOptions,DoneAfterPorts> kDoneAfterDef;

// LinearEnvelope

class ASREnvelopeOptions
{
public:
    ASREnvelopeOptions(OSCPP::Server::ArgStream args)
    {
        attackTime = args.float32();
        sustainTime = args.float32();
        sustainLevel = args.float32();
        releaseTime = args.float32();
    }

    float attackTime;
    float sustainTime;
    float sustainLevel;
    float releaseTime;
};

class ASREnvelopePorts
{
public:
    enum Port
    {
        kInput
      , kOutput
    };

    static constexpr size_t numPorts() { return 2; }

    static Methcla_PortDescriptor descriptor(Port port)
    {
        switch (port)
        {
            case kInput:  return Methcla::Plugin::PortDescriptor::audioInput();
            case kOutput: return Methcla::Plugin::PortDescriptor::audioOutput();
            default: throw std::runtime_error("Invalid port index");
        }
    }
};

class ASREnvelope
{
    enum State
    {
        kAttackPhase,
        kSustainPhase,
        kReleasePhase,
        kDone
    };

    ASREnvelopeOptions m_options;

    State  m_state;
    size_t m_numFramesLeft;
    float  m_slope;
    float  m_level;
    float* m_ports[ASREnvelopePorts::numPorts()];

public:
    ASREnvelope(const World<ASREnvelope>& world, const Methcla_SynthDef*, const ASREnvelopeOptions& options)
        : m_options(options)
        , m_state(kAttackPhase)
        , m_numFramesLeft(options.attackTime * world.sampleRate())
        , m_slope(m_options.sustainLevel / m_numFramesLeft)
        , m_level(0.f)
    {
    }

    void connect(ASREnvelopePorts::Port port, void* data)
    {
        m_ports[port] = static_cast<float*>(data);
    }

    void process(const World<ASREnvelope>& world, size_t numFrames)
    {
        const float* input = m_ports[ASREnvelopePorts::kInput];
        float* output = m_ports[ASREnvelopePorts::kOutput];

        if (m_state == kDone)
        {
            for (size_t k=0; k < numFrames; k++)
                output[k] = 0.f;
        }
        else
        {
            size_t outFrame = 0;

            while (outFrame < numFrames)
            {
                const size_t n = std::min(numFrames - outFrame, m_numFramesLeft);

                for (size_t k=0; k < n; k++)
                {
                    output[outFrame] = input[outFrame] * m_level;
                    m_level += m_slope;
                    outFrame++;
                }

                m_numFramesLeft -= n;

                if (m_numFramesLeft == 0)
                {
                    if (m_state == kAttackPhase)
                    {
                        m_state = kSustainPhase;
                        m_numFramesLeft = m_options.sustainTime * world.sampleRate();
                        m_slope = 0.f;
                    }
                    else if (m_state == kSustainPhase)
                    {
                        m_state = kReleasePhase;
                        m_numFramesLeft = m_options.releaseTime * world.sampleRate();
                        m_slope = -(m_level / m_numFramesLeft);
                    }
                    else
                    {
                        m_state = kDone;
                        world.synthDone(this);
                        break;
                    }
                }
            }

            for (size_t k=outFrame; k < numFrames; k++)
                output[k] = 0.f;
        }
    }
};

StaticSynthDef<ASREnvelope,ASREnvelopeOptions,ASREnvelopePorts> kASREnvelopeDef;

// Library

static const Methcla_Library library = { NULL, NULL };

METHCLA_EXPORT const Methcla_Library* methcla_plugins_node_control(const Methcla_Host* host, const char* /* bundlePath */)
{
    kDoneAfterDef(host, METHCLA_PLUGINS_DONE_AFTER_URI);
    kASREnvelopeDef(host, METHCLA_PLUGINS_ASR_ENVELOPE_URI);
    return &library;
}
