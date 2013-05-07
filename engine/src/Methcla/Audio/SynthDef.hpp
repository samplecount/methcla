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

#include <boost/utility.hpp>
#include <cstring>
#include <list>
#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

namespace Methcla { namespace Audio {

class Port
{
public:
    enum Type
    {
        kInput      = 1
      , kOutput     = 2
      , kAudio      = 4
      , kControl    = 8
    };

    Port(Type type, uint32_t index, const char* symbol)
        : m_type(type)
        , m_index(index)
        , m_symbol(symbol)
    { }

    Type type() const { return m_type; }
    bool isa(Type t) const { return (m_type & t) == t; }
    bool isa(Type t1, Type t2) const { return isa(t1) && isa(t2); }

    uint32_t index() const { return m_index; }
    const char* symbol() const { return m_symbol.c_str(); }

private:
    Type        m_type;
    uint32_t    m_index;
    std::string m_symbol;
};

class FloatPort : public Port
{
public:
    FloatPort( Type type, uint32_t index, const char* symbol
             , float minValue, float maxValue, float defaultValue );

    float minValue() const { return m_minValue; }
    float maxValue() const { return m_maxValue; }
    float defaultValue() const { return m_defaultValue; }

private:
    float   m_minValue;
    float   m_maxValue;
    float   m_defaultValue;
};

//* Plugin library.
class PluginLibrary
{
public:
    PluginLibrary(const Methcla_Library* lib, std::shared_ptr<Methcla::Plugin::Library> plugin=nullptr);
    ~PluginLibrary();

private:
    const Methcla_Library*                      m_lib;
    std::shared_ptr<Methcla::Plugin::Library>   m_plugin;
};

class PluginManager;

class SynthDef : boost::noncopyable
{
public:
    SynthDef(PluginManager& manager, const Methcla_SynthDef* def);

    const char* uri() const { return m_descriptor->uri; }

    size_t instanceSize () const { return m_descriptor->size; }

    size_t numPorts() const { return m_ports.size(); }
    const Port& port(size_t i) const { return m_ports.at(i); }

    size_t numAudioInputs    () const { return m_numAudioInputs;    }
    size_t numAudioOutputs   () const { return m_numAudioOutputs;   }
    size_t numControlInputs  () const { return m_numControlInputs;  }
    size_t numControlOutputs () const { return m_numControlOutputs; }

    Methcla_Synth* construct(const Methcla_World* world, Methcla_Synth* synth) const
    {
        m_descriptor->construct(m_descriptor, world, synth);
        return synth;
    }

    void destroy(const Methcla_World* world, Methcla_Synth* synth) const
    {
        if (m_descriptor->destroy) m_descriptor->destroy(m_descriptor, world, synth);
    }

    void connectPort(Methcla_Synth* synth, uint32_t port, void* data) const
    {
        m_descriptor->connect(synth, port, data);
    }

    void process(Methcla_Synth* synth, size_t numFrames) const
    {
        m_descriptor->process(synth, numFrames);
    }

private:
    const Methcla_SynthDef*         m_descriptor;
    std::vector<Port>               m_ports;
    size_t                          m_numAudioInputs;
    size_t                          m_numAudioOutputs;
    size_t                          m_numControlInputs;
    size_t                          m_numControlOutputs;
};

class PluginManager : boost::noncopyable
{
public:
    typedef std::list<Methcla_LibraryFunction> LibraryFunctions;

    PluginManager(const LibraryFunctions& libs);

    // Plugin discovery and loading
    void loadPlugins(const std::string& directory);

    // SynthDef lookup
    const std::shared_ptr<SynthDef>& lookup(const char* uri) const;

private:
    static void registerSynthDef(Methcla_HostHandle handle, const Methcla_SynthDef* synthDef);

private:
    typedef std::list<std::shared_ptr<PluginLibrary>>
            Libraries;
    typedef std::unordered_map<const char*,
                               std::shared_ptr<SynthDef>,
                               Utility::Hash::cstr_hash,
                               Utility::Hash::cstr_equal>
            PluginMap;

    Libraries m_libs;
    PluginMap m_plugins;
};

}; };

#endif // METHCLA_AUDIO_SYNTHDEF_HPP_INCLUDED
