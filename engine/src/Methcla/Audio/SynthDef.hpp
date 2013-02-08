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

#include <Methcla/LV2/URIDMap.hpp>
#include <Methcla/Utility/Hash.hpp>

#include <boost/filesystem.hpp>
#include <boost/utility.hpp>
#include <cstring>
#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

#include "lilv/lilv.h"
#include "lv2/lv2plug.in/ns/ext/urid/urid.h"
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

namespace Plugin{

class Binary
{
public:
    virtual ~Binary() { }
    virtual LV2_Descriptor_Function descriptorFunction() = 0;
};

class Loader
{
public:
    virtual ~Loader() { }
    virtual std::shared_ptr<Binary> load(const LilvPlugin* plugin) = 0;
};

class StaticBinary : public Binary
{
public:
    StaticBinary(LV2_Descriptor_Function function);
    virtual LV2_Descriptor_Function descriptorFunction();

private:
    LV2_Descriptor_Function m_descriptorFunction;
};

class StaticLoader : public Loader
{
public:
    typedef std::unordered_map<std::string, LV2_Descriptor_Function>
            DescriptorFunctionMap;

    StaticLoader(const DescriptorFunctionMap& functions);
    virtual std::shared_ptr<Binary> load(const LilvPlugin* plugin);

private:
    DescriptorFunctionMap m_functions;
};

//class Descriptor
//{
//public:
//    static Descriptor* load(std::shared_ptr<Binary> binary, const LilvPlugin* plugin);
//
//    const char* uri() const { return m_descriptor->URI; }
//
//    template <class T> const T* extensionData(const char* uri) const
//    {
//        return static_cast<const T*>(m_descriptor->extension_data(uri));
//    }
//
//protected:
//    Descriptor(std::shared_ptr<Binary> binary, const LV2_Descriptor* descriptor);
//
//private:
//    std::shared_ptr<Binary> m_binary;
//    const LV2_Descriptor* m_descriptor;
//};

class Node : boost::noncopyable
{
public:
    Node(LilvNode* node);
    ~Node();

    const LilvNode* impl() const { return m_impl; }

private:
    LilvNode* m_impl;
};

typedef std::shared_ptr<Node> NodePtr;

class Nodes : boost::noncopyable
{
public:
    Nodes(LilvNodes* nodes);
    ~Nodes();
    
    const LilvNodes* impl() const { return m_impl; }

private:
    LilvNodes* m_impl;
};

typedef std::shared_ptr<Nodes> NodesPtr;

// class Constructor
// {
// public:
//     Constructor( const LV2_Descriptor* descriptor
//                , double sampleRate
//                , const char* bundlePath
//                , const LV2_Feature** features )
//         : m_descriptor(descriptor)
//         , m_sampleRate(sampleRate)
//         , m_bundlePath(bundlePath)
//         , m_features
//     { }
// 
//     const LV2_Descriptor* descriptor() const { return m_descriptor; }
// 
//     virtual size_t instanceSize() const = 0;
//     virtual size_t instanceAlignment() const = 0;
//     virtual void construct( double sample_rate
//                           , const char* bundle_path
//                           , const LV2_Feature *const *features
//                           ) const = 0;
// 
// private:
//     const LV2_Descriptor* m_descriptor;
//     double m_sampleRate;
//     const char* m_bundlePath;
//     const LV2_Feature** m_features;
// };
// 
// class PlacementConstructor : public Constructor
// {
// public:
//     PlacementConstructor(const LV2_Descriptor* descriptor, const LV2_Placement_Instantiate_Interface* interface);
// 
//     virtual size_t instanceSize() const;
//     virtual size_t instanceAlignment() const;
//     virtual void construct( double sample_rate
//                           , const char* bundle_path
//                           , const LV2_Feature *const *features
//                           ) const;
// 
// private:
//     const LV2_Placement_Instantiate_Interface* m_interface;
// };
// 
// class HostFeatures
// {
// public:
//     
//     hardRTCapable
//     hardRTInstantiable
// 
//     placement-instantiate:location
// };

class Manager;

class Plugin : boost::noncopyable
{
public:
    Plugin(Manager& manager, const LilvPlugin* plugin);

    const char* uri() const;
    const char* name() const;

    size_t instanceSize      () const;
    size_t instanceAlignment () const;

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
    const LilvPlugin*                   m_plugin;
    const LV2_Descriptor*               m_descriptor;
    std::shared_ptr<Binary>             m_binary;
    const char*                         m_bundlePath;
    const LV2_Feature* const*           m_features;
    const LV2_RT_Instantiate_Interface* m_constructor;
    std::vector<FloatPort>              m_ports;
    uint32_t                            m_numAudioInputs;
    uint32_t                            m_numAudioOutputs;
    uint32_t                            m_numControlInputs;
    uint32_t                            m_numControlOutputs;
};

class Manager : boost::noncopyable
{
public:
    Manager(Loader& loader);
    ~Manager();

    // Features
    const LV2_Feature* const* features();

    // Node creation
    NodePtr newUri(const char* uri);

    // Plugin discovery and loading
    Loader& loader() { return m_loader; }
    void loadPlugins(const boost::filesystem::path& directory);

    // Plugin access
    typedef std::shared_ptr<Plugin> PluginHandle;

    const PluginHandle& lookup(LV2_URID urid) const;

    // Uri mapping
    const LV2::URIDMap& uriMap() const { return m_uriMap; }
	LV2::URIDMap& uriMap() { return m_uriMap; }

private:
    void addFeature(const char* uri, void* data=0);

    // typedef std::unordered_map<
    //             const char*
    //           , PluginHandle
    //           , Methcla::Utility::Hash::string_hash
    //           , Methcla::Utility::Hash::string_equal_to >
    //         Map;
    typedef std::unordered_map<LV2_URID, PluginHandle> Map;

private:
    typedef std::vector<const LV2_Feature*> Features;
    Loader&         m_loader;
    LilvWorld*      m_world;
    Features        m_features;
    Map             m_plugins;
    LV2::URIDMap    m_uriMap;
};

};

}; };

#endif // METHCLA_AUDIO_SYNTHDEF_HPP_INCLUDED