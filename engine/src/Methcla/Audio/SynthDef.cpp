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

#include "Methcla/Exception.hpp"
#include "Methcla/Audio/SynthDef.hpp"

#include <iostream>
#include <memory>
#include <utility>

using namespace boost;
using namespace Methcla::Audio;
using namespace std;

SynthDef::SynthDef(const Methcla_SynthDef* synthDef)
    : m_descriptor(synthDef)
    // , m_numAudioInputs(0)
    // , m_numAudioOutputs(0)
    // , m_numControlInputs(0)
    // , m_numControlOutputs(0)
{
    // Methcla_Port port;
    // std::memset(&port, 0, sizeof(port));
    // for (size_t i=0; m_descriptor->port(m_descriptor, nullptr, i, &port); i++) {
    //     switch (port.type) {
    //         case kMethcla_AudioPort:
    //             switch (port.direction) {
    //                 case kMethcla_Input:
    //                     m_ports.push_back(Port(port, m_numAudioInputs));
    //                     m_numAudioInputs++;
    //                     break;
    //                 case kMethcla_Output:
    //                     m_ports.push_back(Port(port, m_numAudioOutputs));
    //                     m_numAudioOutputs++;
    //                     break;
    //             }
    //             break;
    //         case kMethcla_ControlPort:
    //             switch (port.direction) {
    //                 case kMethcla_Input:
    //                     m_ports.push_back(Port(port, m_numControlInputs));
    //                     m_numControlInputs++;
    //                     break;
    //                 case kMethcla_Output:
    //                     m_ports.push_back(Port(port, m_numControlOutputs));
    //                     m_numControlOutputs++;
    //                     break;
    //             }
    //     }
    // }

    // Validate descriptor fields (some are optional)
    if (m_descriptor->uri == nullptr || m_descriptor->uri[0] == '\0')
        throw std::invalid_argument("SynthDef: Missing URI");
    if (m_descriptor->construct == nullptr)
        throw std::invalid_argument("SynthDef: Missing `construct' function");
    if (m_descriptor->port_descriptor == nullptr)
        throw std::invalid_argument("SynthDef: Missing `port_descriptor' function");
    if (m_descriptor->connect == nullptr)
        throw std::invalid_argument("SynthDef: Missing `connect' function");
    if (m_descriptor->process == nullptr)
        throw std::invalid_argument("SynthDef: Missing `process' function");

    m_options = m_descriptor->options_size > 0 ? new char[m_descriptor->options_size] : nullptr;

    std::cerr << "SynthDef " << uri() << " loaded (" << m_descriptor << "):" << std::endl
              << "    instance size: " << instanceSize() << std::endl;
              // << "    control inputs: " << numControlInputs() << std::endl
              // << "    control outputs: " << numControlOutputs() << std::endl
              // << "    audio inputs: " << numAudioInputs() << std::endl
              // << "    audio outputs: " << numAudioOutputs() << std::endl;
}

SynthDef::~SynthDef()
{
    delete [] static_cast<char*>(m_options);
}

const Methcla_SynthOptions* SynthDef::configure(OSCPP::Server::ArgStream options) const
{
    if (m_descriptor->configure) {
        auto state = options.state();
        m_descriptor->configure(
            std::get<0>(state).pos(), std::get<0>(state).consumable(),
            std::get<1>(state).pos(), std::get<1>(state).consumable(),
            m_options
        );
        return m_options;
    }
    return nullptr;
}

bool SynthDef::portDescriptor(const Methcla_SynthOptions* options, size_t index, Methcla_PortDescriptor* port) const
{
    return m_descriptor->port_descriptor(options, index, port);
}

void SynthDef::construct(const Methcla_World* world, const Methcla_SynthOptions* options, Synth* owner, Methcla_Synth* synth) const
{
    m_descriptor->construct(world, m_descriptor, options, owner, synth);
}

void SynthDef::destroy(const Methcla_World* world, Methcla_Synth* synth) const
{
    if (m_descriptor->destroy) m_descriptor->destroy(world, synth);
}

PluginLibrary::PluginLibrary(const Methcla_Library* lib, std::shared_ptr<Methcla::Plugin::Library> plugin)
    : m_lib(lib)
    , m_plugin(plugin)
{
}

PluginLibrary::~PluginLibrary()
{
    if ((m_lib != nullptr) && (m_lib->destroy != nullptr)) {
        m_lib->destroy(m_lib);
    }
}

void PluginManager::loadPlugins(const Methcla_Host* host, const std::list<Methcla_LibraryFunction>& funcs)
{
    for (auto f : funcs) {
        const Methcla_Library* lib = f(host, ".");
        if (lib != nullptr) {
            m_libs.push_back(std::make_shared<PluginLibrary>(lib));
        }
    }
}

void PluginManager::loadPlugins(const Methcla_Host* host, const std::string& directory)
{
    std::cout << "PluginManager::loadPlugins not yet implemented" << std::endl;
}
