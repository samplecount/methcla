#ifndef MESCALINE_AUDIO_SYNTHDEF_HPP_INCLUDED
#define MESCALINE_AUDIO_SYNTHDEF_HPP_INCLUDED

#include <Mescaline/Audio/Plugin/API.h>
#include <Mescaline/Utility/Hash.hpp>

#include <boost/filesystem.hpp>
#include <boost/ptr_container/ptr_vector.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/unordered_map.hpp>
#include <boost/utility.hpp>

#include <cstring>
#include <string>
#include <vector>

#include <lilv/lilv.h>
#include "lv2/lv2plug.in/ns/ext/urid/urid.h"

#include "lv2/puesnada.es/ext/rt-instantiate/rt-instantiate.h"

namespace Mescaline { namespace Audio {

using namespace boost::filesystem;
using namespace std;

class SynthDef : boost::noncopyable
{
public:
    SynthDef(MescalineHost* host, MescalineSynthDef* def)
        : m_host(host)
        , m_def(def)
    { }
    
    const char* name() const { return m_def->name; }

    size_t instanceSize      () const { return m_def->instanceSize;       }
    size_t instanceAlignment () const { return m_def->instanceAlignment;  }
    
    size_t numAudioInputs    () const { return m_def->numAudioInputs;    }
    size_t numAudioOutputs   () const { return m_def->numAudioOutputs;   }
    size_t numControlInputs  () const { return m_def->numControlInputs;  }
    size_t numControlOutputs () const { return m_def->numControlOutputs; }

    void initialize() { (*m_def->fInitialize)(m_host, m_def); }
    void cleanup() { (*m_def->fCleanup)(m_host, m_def); }

    const MescalineControlSpec& controlInputSpec(size_t index) const
    {
        const MescalineControlSpec* spec = MescalineSynthDefGetControlInputSpec(m_def, index);
        BOOST_ASSERT( spec != 0 );
        return *spec;
    }

    const MescalineControlSpec& controlOutputSpec(size_t index) const
    {
        const MescalineControlSpec* spec = MescalineSynthDefGetControlOutputSpec(m_def, index);
        BOOST_ASSERT( spec != 0 );
        return *spec;
    }

    void construct(MescalineSynth* instance) const
    {
        memset(instance, 0, sizeof(MescalineSynth));
        MescalineSynthDefConstruct(m_host, m_def, instance);
    }

    void destroy(MescalineSynth* instance) const
    {
        MescalineSynthDefDestroy(m_host, m_def, instance);
    }

private:
    MescalineHost*     m_host;
    MescalineSynthDef* m_def;
};

class SynthDefMap
{
public:
    void insert(const SynthDef* synthDef)
    {
        m_map[synthDef->name()] = synthDef;
    }

    const SynthDef& lookup(const char* name) const
    {
        // TODO: error handling
        return *m_map.at(name);
    }

    // TODO: Use reference count for SynthDefs and implement delete/overwrite
    // TODO: Use 4-byte aligned symbols for faster lookups

private:
    typedef boost::unordered_map<
                const char*
              , const SynthDef*
              , Mescaline::Utility::Hash::string_hash
              , Mescaline::Utility::Hash::string_equal_to >
            Map;
    Map m_map;
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
    virtual boost::shared_ptr<Binary> load(const LilvPlugin* plugin) = 0;
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
    typedef boost::unordered_map<std::string, LV2_Descriptor_Function>
            DescriptorFunctionMap;

    StaticLoader(const DescriptorFunctionMap& functions);
    virtual boost::shared_ptr<Binary> load(const LilvPlugin* plugin);

private:
    DescriptorFunctionMap m_functions;
};

//class Descriptor
//{
//public:
//    static Descriptor* load(boost::shared_ptr<Binary> binary, const LilvPlugin* plugin);
//
//    const char* uri() const { return m_descriptor->URI; }
//
//    template <class T> const T* extensionData(const char* uri) const
//    {
//        return static_cast<const T*>(m_descriptor->extension_data(uri));
//    }
//
//protected:
//    Descriptor(boost::shared_ptr<Binary> binary, const LV2_Descriptor* descriptor);
//
//private:
//    boost::shared_ptr<Binary> m_binary;
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

typedef boost::shared_ptr<Node> NodePtr;

class Nodes : boost::noncopyable
{
public:
    Nodes(LilvNodes* nodes);
    ~Nodes();
    
    const LilvNodes* impl() const { return m_impl; }

private:
    LilvNodes* m_impl;
};

typedef boost::shared_ptr<Nodes> NodesPtr;

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
    uint32_t index() const { return m_index; }
    const char* symbol() const { return m_symbol.c_str(); }

private:
    Type        m_type;
    uint32_t    m_index;
    string      m_symbol;
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

class Plugin : boost::noncopyable
{
public:
    Plugin(Manager& manager, const LilvPlugin* plugin);
    ~Plugin();

    const char* uri() const;
    const char* name() const;

    size_t instanceSize      () const;
    size_t instanceAlignment () const;

    size_t numAudioInputs    () const { return m_audioInputs.size();    }
    size_t numAudioOutputs   () const { return m_audioOutputs.size();   }
    size_t numControlInputs  () const { return m_controlInputs.size();  }
    size_t numControlOutputs () const { return m_controlOutputs.size(); }

    const FloatPort& audioInputPort(size_t i) const { return m_audioInputs.at(i); }
    const FloatPort& audioOutputPort(size_t i) const { return m_audioOutputs.at(i); }
    const FloatPort& controlInputPort(size_t i) const { return m_controlInputs.at(i); }
    const FloatPort& controlOutputPort(size_t i) const { return m_controlOutputs.at(i); }

    LV2_Handle construct(void* location, double sampleRate) const;

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

    // void initialize() { (*m_def->fInitialize)(m_host, m_def); }
    // void cleanup() { (*m_def->fCleanup)(m_host, m_def); }
    // 
    // const MescalineControlSpec& controlInputSpec(size_t index) const
    // {
    //     const MescalineControlSpec* spec = MescalineSynthDefGetControlInputSpec(m_def, index);
    //     BOOST_ASSERT( spec != 0 );
    //     return *spec;
    // }
    // 
    // const MescalineControlSpec& controlOutputSpec(size_t index) const
    // {
    //     const MescalineControlSpec* spec = MescalineSynthDefGetControlOutputSpec(m_def, index);
    //     BOOST_ASSERT( spec != 0 );
    //     return *spec;
    // }
    // 
    // void construct(MescalineSynth* instance) const
    // {
    //     memset(instance, 0, sizeof(MescalineSynth));
    //     MescalineSynthDefConstruct(m_host, m_def, instance);
    // }
    // 
    // void destroy(MescalineSynth* instance) const
    // {
    //     MescalineSynthDefDestroy(m_host, m_def, instance);
    // }

private:
    const LilvPlugin*                   m_plugin;
    const LV2_Descriptor*               m_descriptor;
    boost::shared_ptr<Binary>           m_binary;
    const char*                         m_bundlePath;
    const LV2_Feature* const*           m_features;
    const LV2_RT_Instantiate_Interface* m_constructor;
    vector<FloatPort>                   m_audioInputs;
    vector<FloatPort>                   m_audioOutputs;
    vector<FloatPort>                   m_controlInputs;
    vector<FloatPort>                   m_controlOutputs;
};

class UriMap
{
public:
    LV2_URID map(const char* uri) const;
    const char* unmap(LV2_URID urid) const;

private:
    typedef boost::unordered_map<
                const char*
              , LV2_URID
              , Mescaline::Utility::Hash::string_hash
              , Mescaline::Utility::Hash::string_equal_to >
            UriToId;
    typedef boost::unordered_map<
                LV2_URID
              , const char* >
            IdToUri;
    
    UriToId m_uriToId;
    IdToUri m_idToUri;
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
    void loadPlugins();

    // Plugin access
    typedef boost::shared_ptr<Plugin> PluginHandle;

    const PluginHandle& lookup(const char* uri) const;

private:
    void addFeature(const char* uri, void* data=0);

    typedef boost::unordered_map<
                const char*
              , PluginHandle
              , Mescaline::Utility::Hash::string_hash
              , Mescaline::Utility::Hash::string_equal_to >
            Map;

private:
    typedef std::vector<const LV2_Feature*> Features;
    Loader&     m_loader;
    LilvWorld*  m_world;
    Features    m_features;
    Map         m_plugins;
    UriMap      m_uris;
};

};

}; };

#endif // MESCALINE_AUDIO_SYNTHDEF_HPP_INCLUDED