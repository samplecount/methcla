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

#ifndef METHCLA_AUDIO_SYNTHDEF_HPP_INCLUDED
#define METHCLA_AUDIO_SYNTHDEF_HPP_INCLUDED

#include <methcla/engine.h>
#include <methcla/plugin.h>

#include "Methcla/Plugin/Loader.hpp"
#include "Methcla/Utility/Hash.hpp"

#include <cstring>
#include <list>
#include <memory>
#include <oscpp/server.hpp>
#include <string>
#include <unordered_map>

namespace Methcla { namespace Audio {

class Synth;

class SynthDef
{
public:
    SynthDef(const Methcla_SynthDef* def);
    ~SynthDef();

    SynthDef(const SynthDef&) = delete;
    SynthDef& operator=(const SynthDef&) = delete;

    inline const char* uri() const { return m_descriptor->uri; }

    inline size_t instanceSize () const { return m_descriptor->instance_size; }

    // NOTE: Uses static data and should only be called from a single thread (normally the audio thread) at a time.
    const Methcla_SynthOptions* configure(OSCPP::Server::ArgStream options) const;

    //* Return port descriptor at index.
    bool portDescriptor(const Methcla_SynthOptions* options, size_t index, Methcla_PortDescriptor* port) const;

    void construct(const Methcla_World* world, const Methcla_SynthOptions* options, Methcla_Synth* synth) const;
    void destroy(const Methcla_World* world, Methcla_Synth* synth) const;

    inline void connect(Methcla_Synth* synth, Methcla_PortCount port, void* data) const
    {
        m_descriptor->connect(synth, port, data);
    }

    inline void activate(const Methcla_World* world, Methcla_Synth* synth) const
    {
        if (m_descriptor->activate) m_descriptor->activate(world, synth);
    }

    inline void process(const Methcla_World* world, Methcla_Synth* synth, size_t numFrames) const
    {
        m_descriptor->process(world, synth, numFrames);
    }

private:
    const Methcla_SynthDef* m_descriptor;
    Methcla_SynthOptions*   m_options; // Only access from one thread
};

typedef std::unordered_map<const char*,
                           std::shared_ptr<SynthDef>,
                           Utility::Hash::cstr_hash,
                           Utility::Hash::cstr_equal>
        SynthDefMap;

//* Plugin library.
class PluginLibrary
{
public:
    PluginLibrary(const Methcla_Library* lib, std::shared_ptr<Methcla::Plugin::Library> plugin=nullptr);
    ~PluginLibrary();

    PluginLibrary(const PluginLibrary&) = delete;
    PluginLibrary& operator=(const PluginLibrary&) = delete;

private:
    const Methcla_Library*                      m_lib;
    std::shared_ptr<Methcla::Plugin::Library>   m_plugin;
};

class PluginManager
{
public:
    PluginManager();

    PluginManager(const PluginManager&) = delete;
    PluginManager& operator=(const PluginManager&) = delete;

    //* Load plugins from static functions.
    void loadPlugins(const Methcla_Host* host, const std::list<Methcla_LibraryFunction>& funcs);

    //* Load plugins from directory.
    void loadPlugins(const Methcla_Host* host, const std::string& directory);

private:
    typedef std::list<std::shared_ptr<PluginLibrary>>
            Libraries;
    Libraries m_libs;
};

} }

#endif // METHCLA_AUDIO_SYNTHDEF_HPP_INCLUDED
