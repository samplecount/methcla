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

#include "Methcla/Lilv.hpp"
#include "Methcla/Plugin/Loader.hpp"
#include "Methcla/Utility/Hash.hpp"

#include <boost/filesystem.hpp>
#include <boost/utility.hpp>
#include <cstring>
#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

#include "lv2/methc.la/ext/rt-instantiate/rt-instantiate.h"

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

//* LV2 plugin library.
class PluginLibrary
{
public:
    PluginLibrary(const LV2_Descriptor_Function descFunc, std::shared_ptr<Methcla::Plugin::Library> library=nullptr)
        : m_lib(library)
        , m_descFunc(descFunc)
        , m_libDesc(nullptr)
    { }
    PluginLibrary(const LV2_Lib_Descriptor* libDesc, std::shared_ptr<Methcla::Plugin::Library> library=nullptr)
        : m_lib(library)
        , m_descFunc(nullptr)
        , m_libDesc(libDesc)
    { }
    ~PluginLibrary();

    static std::shared_ptr<PluginLibrary> create(const char* bundlePath, const LV2_Feature* const* features, std::shared_ptr<Methcla::Plugin::Library> library);
    static std::shared_ptr<PluginLibrary> create(const char* bundlePath, const LV2_Feature* const* features, LV2_Lib_Descriptor_Function library);
    static std::shared_ptr<PluginLibrary> create(LV2_Descriptor_Function library);

    const LV2_Descriptor* get(uint32_t index);

private:
    std::shared_ptr<Methcla::Plugin::Library> m_lib;
    LV2_Descriptor_Function m_descFunc;
    const LV2_Lib_Descriptor* m_libDesc;
};

class PluginManager;

class Plugin : boost::noncopyable
{
public:
    Plugin(PluginManager& manager, std::shared_ptr<PluginLibrary> library, const LilvPlugin* plugin);

    const char* uri() const;
    const char* name() const;

    size_t instanceSize      () const;

    size_t numPorts() const { return m_ports.size(); }
    const FloatPort& port(size_t i) const { return m_ports.at(i); }

    size_t numAudioInputs    () const { return m_numAudioInputs;    }
    size_t numAudioOutputs   () const { return m_numAudioOutputs;   }
    size_t numControlInputs  () const { return m_numControlInputs;  }
    size_t numControlOutputs () const { return m_numControlOutputs; }

    LV2_Handle construct(void* location, double sampleRate, const LV2_Feature* const* features=nullptr) const;

    void destroy(LV2_Handle instance) const
    {
        if (m_descriptor->cleanup) m_descriptor->cleanup(instance);
    }

    void activate(LV2_Handle instance) const
    {
        if (m_descriptor->activate) m_descriptor->activate(instance);
    }

    void deactivate(LV2_Handle instance) const
    {
        if (m_descriptor->deactivate) m_descriptor->deactivate(instance);
    }

    void connectPort(LV2_Handle instance, uint32_t port, void* data) const
    {
        m_descriptor->connect_port(instance, port, data);
    }

    void run(LV2_Handle instance, uint32_t numSamples) const
    {
        m_descriptor->run(instance, numSamples);
    }

private:
    std::shared_ptr<PluginLibrary>              m_library;
    const LilvPlugin*                           m_plugin;
    const LV2_Descriptor*                       m_descriptor;
    const char*                                 m_bundlePath;
    const LV2_Feature* const*                   m_features;
    const LV2_RT_Instantiate_Interface*         m_constructor;
    std::vector<FloatPort>                      m_ports;
    uint32_t                                    m_numAudioInputs;
    uint32_t                                    m_numAudioOutputs;
    uint32_t                                    m_numControlInputs;
    uint32_t                                    m_numControlOutputs;
};

class PluginManager : boost::noncopyable
{
public:
    typedef std::unordered_map<std::string,LV2_Lib_Descriptor_Function> StaticLibraryMap;

    PluginManager(const StaticLibraryMap& libs=StaticLibraryMap());
    ~PluginManager();

    // Features
    const LV2_Feature* const* features();

    // Node creation
    Lilv::NodePtr newUri(const char* uri);

    // Plugin discovery and loading
    void loadPlugins(const boost::filesystem::path& directory);

    // Plugin access
    const std::shared_ptr<Plugin>& lookup(const char* name) const;

private:
    void addFeature(const char* uri, void* data=0);

    typedef std::vector<const LV2_Feature*> Features;
    typedef std::unordered_map<std::string,
                               std::shared_ptr<PluginLibrary>>
            LibraryMap;
    typedef std::unordered_map<const char*,
                               std::shared_ptr<Plugin>,
                               Utility::Hash::cstr_hash,
                               Utility::Hash::cstr_equal>
            PluginMap;

    LilvWorld*                              m_world;
    Features                                m_features;
    Methcla::Plugin::StaticLoader           m_loader;
    StaticLibraryMap                        m_staticLibs;
    LibraryMap                              m_libs;
    PluginMap                               m_plugins;
};

}; };

#endif // METHCLA_AUDIO_SYNTHDEF_HPP_INCLUDED
