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

static const char kDoneAfterUri[] = METHCLA_PLUGINS_DONE_AFTER_URI;

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
    DoneAfter(const World<DoneAfter>& world, const Methcla_SynthDef* synthDef, const DoneAfterOptions& options)
        : m_numFramesLeft((double)options.m_seconds * world.sampleRate())
        , m_done(false)
    {
    }

    void connect(DoneAfterPorts::Port port, void* data)
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

SynthClass<DoneAfter,StaticSynthOptions<DoneAfterOptions,DoneAfterPorts>,DoneAfterPorts,kDoneAfterUri>
    kDoneAfterClass;

static const Methcla_Library library = { NULL, NULL };

METHCLA_EXPORT const Methcla_Library* methcla_plugins_node_control(const Methcla_Host* host, const char* /* bundlePath */)
{
    methcla_host_register_synthdef(
        host,
        kDoneAfterClass.descriptor()
    );
    return &library;
}
