// Copyright 2012-2013 Samplecount S.L.
//
// SPDX-License-Identifier: Apache-2.0

#include "Methcla/Audio/Synth.hpp"
#include "Methcla/Audio/SynthDef.hpp"

#include <iostream>
#include <memory>
#include <stdexcept>
#include <utility>

using namespace Methcla::Audio;

SynthDef::SynthDef(const Methcla_SynthDef* synthDef)
: m_descriptor(synthDef)
{
    // Validate descriptor fields (some are optional)
    if (m_descriptor->uri == nullptr || m_descriptor->uri[0] == '\0')
        throw std::invalid_argument("SynthDef: Missing URI");
    if (m_descriptor->construct == nullptr)
        throw std::invalid_argument("SynthDef: Missing `construct' function");
    if (m_descriptor->port_descriptor == nullptr)
        throw std::invalid_argument(
            "SynthDef: Missing `port_descriptor' function");
    if (m_descriptor->connect == nullptr)
        throw std::invalid_argument("SynthDef: Missing `connect' function");
    if (m_descriptor->process == nullptr)
        throw std::invalid_argument("SynthDef: Missing `process' function");

    m_options = m_descriptor->options_size > 0
                    ? new char[m_descriptor->options_size]
                    : nullptr;
}

SynthDef::~SynthDef()
{
    delete[] static_cast<char*>(m_options);
}

const Methcla_SynthOptions*
SynthDef::configure(OSCPP::Server::ArgStream options) const
{
    if (m_descriptor->configure)
    {
        auto state = options.state();
        m_descriptor->configure(std::get<0>(state).pos(),
                                std::get<0>(state).consumable(),
                                std::get<1>(state).pos(),
                                std::get<1>(state).consumable(), m_options);
        return m_options;
    }
    return nullptr;
}

bool SynthDef::portDescriptor(const Methcla_SynthOptions* options,
                              Methcla_PortCount           index,
                              Methcla_PortDescriptor*     port) const
{
    return m_descriptor->port_descriptor(options, index, port);
}

void SynthDef::construct(Methcla_World*              world,
                         const Methcla_SynthOptions* options,
                         Methcla_Synth*              synth) const
{
    m_descriptor->construct(world, m_descriptor, options, synth);
}

void SynthDef::destroy(Methcla_World* world, Methcla_Synth* synth) const
{
    if (m_descriptor->destroy)
        m_descriptor->destroy(world, synth);
}
