#include "Mescaline/Exception.hpp"
#include "Mescaline/Audio/SynthDef.hpp"

#include <boost/scoped_ptr.hpp>
#include <boost/foreach.hpp>
#include <boost/optional.hpp>
#include <iostream>
#include <limits>
#include <utility>

#include "serd/serd.h"

using namespace boost;
// using namespace Mescaline::Audio;
using namespace Mescaline::Audio::Plugin;
using namespace std;

StaticBinary::StaticBinary(LV2_Descriptor_Function function)
    : m_descriptorFunction(function)
{ }

LV2_Descriptor_Function StaticBinary::descriptorFunction()
{
    return m_descriptorFunction;
}

StaticLoader::StaticLoader(const DescriptorFunctionMap& functions)
    : m_functions(functions)
{ }

shared_ptr<Binary> StaticLoader::load(const LilvPlugin* plugin)
{
    const char* uri = lilv_node_as_uri(lilv_plugin_get_uri(plugin));

    if (uri == 0)
        return boost::shared_ptr<Mescaline::Audio::Plugin::Binary>();

    DescriptorFunctionMap::iterator f = m_functions.find(uri);
    if (f == m_functions.end())
        return boost::shared_ptr<Mescaline::Audio::Plugin::Binary>();

    boost::shared_ptr<Binary> result(new StaticBinary(f->second));
    
    return result;
}

//Descriptor* Descriptor::load(shared_ptr<Binary> binary, const LilvPlugin* plugin)
//{
//    LV2_Descriptor_Function df = binary->descriptorFunction();
//    Descriptor* result = 0;
//
//    for (uint32_t i = 0; true; ++i) {
//        const LV2_Descriptor* ld = df(i);
//
//        if (ld == 0) {
//            break;
//        }
//
//        // Parse bundle URI to use as base URI
//        const LilvNode* bundle_uri     = lilv_plugin_get_bundle_uri(plugin);
//        const char*     bundle_uri_str = lilv_node_as_uri(bundle_uri);
//        SerdURI         base_uri;
//        if (serd_uri_parse((const uint8_t*)bundle_uri_str, &base_uri)) {
//            break;
//        }
//
//        // Resolve library plugin URI against base URI
//        SerdURI  abs_uri;
//        SerdNode abs_uri_node = serd_node_new_uri_from_string(
//            (const uint8_t*)ld->URI, &base_uri, &abs_uri);
//        if (!abs_uri_node.buf) {
//            break;
//        }
//
//        if (strcmp((const char*)abs_uri_node.buf,
//                   lilv_node_as_uri(lilv_plugin_get_uri(plugin))) == 0) {
//            result = new Descriptor(binary, ld);
//            serd_node_free(&abs_uri_node);
//            break;
//        } else {
//            serd_node_free(&abs_uri_node);
//        }
//    }
//
//    return result;
//}
//
//Descriptor::Descriptor(shared_ptr<Binary> binary, const LV2_Descriptor* descriptor)
//    : m_binary(binary)
//    , m_descriptor(descriptor)
//{ }

typedef pair<const LV2_Descriptor*, shared_ptr<Binary> > PluginDescriptor;

static optional<PluginDescriptor> loadPlugin(Loader& loader, const LilvPlugin* plugin)
{
    shared_ptr<Binary> binary(loader.load(plugin));

    if (binary == boost::shared_ptr<Binary>())
        return optional<PluginDescriptor>();

    LV2_Descriptor_Function df = binary->descriptorFunction();
    const LV2_Descriptor* result = 0;
    
    for (uint32_t i = 0; true; ++i) {
        const LV2_Descriptor* ld = df(i);
        
        if (ld == 0) {
            break;
        }
        
        // Parse bundle URI to use as base URI
        const LilvNode* bundle_uri     = lilv_plugin_get_bundle_uri(plugin);
        const char*     bundle_uri_str = lilv_node_as_uri(bundle_uri);
        SerdURI         base_uri;
        if (serd_uri_parse((const uint8_t*)bundle_uri_str, &base_uri)) {
            break;
        }
        
        // Resolve library plugin URI against base URI
        SerdURI  abs_uri;
        SerdNode abs_uri_node = serd_node_new_uri_from_string(
                                                              (const uint8_t*)ld->URI, &base_uri, &abs_uri);
        if (!abs_uri_node.buf) {
            break;
        }
        
        if (strcmp((const char*)abs_uri_node.buf,
                   lilv_node_as_uri(lilv_plugin_get_uri(plugin))) == 0) {
            result = ld;
            serd_node_free(&abs_uri_node);
            break;
        } else {
            serd_node_free(&abs_uri_node);
        }
    }
    
    return result == 0
        ? optional<PluginDescriptor>()
        : optional<PluginDescriptor>(PluginDescriptor(result, binary));
}


// PlacementConstructor::PlacementConstructor
//     ( const LV2_Descriptor* descriptor
//     , const LV2_Placement_Instantiate_Interface* interface )
//     : Constructor(descriptor)
//     , m_interface(interface)
// { }
// 
// size_t PlacementConstructor::instanceSize() const
// {
//     return m_interface->instance_size(descriptor());
// }
// 
// size_t PlacementConstructor::instanceAlignment() const
// {
//     return m_interface->instance_alignment(descriptor());
// }
// 
// void PlacementConstructor::construct(double sampleRate) const
// {
// }

Mescaline::Audio::FloatPort::FloatPort( Type type, uint32_t index, const char* symbol
                    , float minValue, float maxValue, float defaultValue )
    : Port(type, index, symbol)
    , m_minValue(isnan(minValue) ? -numeric_limits<float>::max() : minValue)
    , m_maxValue(isnan(maxValue) ? numeric_limits<float>::max() : maxValue)
    , m_defaultValue(isnan(defaultValue) ? 0 : defaultValue)
{ }

#define LV2_CORE_URI "http://lv2plug.in/ns/lv2core"
#define PUESNADA_URI "http://mescaline.puesnada.es/lv2"

Plugin::Plugin(Manager& manager, const LilvPlugin* plugin)
    : m_plugin(plugin)
    , m_descriptor(0)
    , m_features(manager.features())
    , m_numAudioInputs(0)
    , m_numAudioOutputs(0)
    , m_numControlInputs(0)
    , m_numControlOutputs(0)
{
    m_bundlePath = lilv_uri_to_path(lilv_node_as_uri(lilv_plugin_get_bundle_uri(m_plugin)));

    NodePtr audioClass(manager.newUri(LV2_CORE_URI "#AudioPort"));
    NodePtr controlClass(manager.newUri(LV2_CORE_URI "#ControlPort"));
    NodePtr inputClass(manager.newUri(LV2_CORE_URI "#InputPort"));
    NodePtr outputClass(manager.newUri(LV2_CORE_URI "#OutputPort"));

    const size_t numPorts = lilv_plugin_get_num_ports(m_plugin);
    float minValues[numPorts];
    float maxValues[numPorts];
    float defValues[numPorts];
    lilv_plugin_get_port_ranges_float(m_plugin, minValues, maxValues, defValues);

    for (size_t i=0; i < numPorts; i++) {
        const LilvPort* lilvPort = lilv_plugin_get_port_by_index(m_plugin, i);
        if (lilvPort != 0) {
            const char* symbol = lilv_node_as_string(lilv_port_get_symbol(m_plugin, lilvPort));
            if (lilv_port_is_a(m_plugin, lilvPort, audioClass->impl())) {
                if (lilv_port_is_a(m_plugin, lilvPort, inputClass->impl())) {
                    m_ports.push_back(FloatPort( Port::Type(Port::kAudio|Port::kInput)
                                               , m_numAudioInputs
                                               , symbol
                                               , minValues[i], maxValues[i], defValues[i]));
                    m_numAudioInputs++;
                } else if (lilv_port_is_a(m_plugin, lilvPort, outputClass->impl())) {
                    m_ports.push_back(FloatPort( Port::Type(Port::kAudio|Port::kOutput)
                                               , m_numAudioOutputs
                                               , symbol
                                               , minValues[i], maxValues[i], defValues[i]));
                    m_numAudioOutputs++;
                } else {
                    // TODO: Unknown port class
                    BOOST_ASSERT_MSG( false, "Unknown audio port class" );
                }
            } else if (lilv_port_is_a(m_plugin, lilvPort, controlClass->impl())) {
                if (lilv_port_is_a(m_plugin, lilvPort, inputClass->impl())) {
                    m_ports.push_back(FloatPort( Port::Type(Port::kControl|Port::kInput)
                                               , m_numControlInputs
                                               , symbol
                                               , minValues[i], maxValues[i], defValues[i]));
                    m_numControlInputs++;
                } else if (lilv_port_is_a(m_plugin, lilvPort, outputClass->impl())) {
                    m_ports.push_back(FloatPort( Port::Type(Port::kControl|Port::kOutput)
                                               , m_numControlOutputs
                                               , symbol
                                               , minValues[i], maxValues[i], defValues[i]));
                    m_numControlOutputs++;
                } else {
                    // TODO: Unknown port class
                    BOOST_ASSERT_MSG( false, "Unknown control port class" );
                }
            } else {
                // TODO: Unknown port class
                BOOST_ASSERT_MSG( false, "Unknown port class" );
            }
        }
    }

    optional<PluginDescriptor> pd = loadPlugin(manager.loader(), m_plugin);
    if (pd) {
        m_descriptor = pd->first;
        m_binary = pd->second;

        NodePtr extensionData(manager.newUri(LV2_CORE_URI "#extensionData"));
        NodesPtr extensions(new Nodes(lilv_plugin_get_value(plugin, extensionData->impl())));
        
        m_constructor = static_cast<const LV2_RT_Instantiate_Interface*>(
                            m_descriptor->extension_data(LV2_RT_INSTANTIATE__INTERFACE) );
        m_constructor->initialize(const_cast<LV2_Descriptor*>(m_descriptor), m_bundlePath, m_features);

        cout << "Plugin descriptor for " << uri() << " loaded (" << m_descriptor << "):"
             << "    instance size: " << instanceSize() << endl
             << "    instance alignment: " << instanceAlignment() << endl
             << "    control inputs: " << numControlInputs() << endl
             << "    control outputs: " << numControlOutputs() << endl
             << "    audio inputs: " << numAudioInputs() << endl
             << "    audio outputs: " << numAudioOutputs() << endl;
    } else {
        cout << "Couldn't find plugin descriptor for " << uri() << endl;
    }
}

const char* Plugin::uri() const
{
    return lilv_node_as_uri(lilv_plugin_get_uri(m_plugin));
}
 
const char* Plugin::name() const
{
    return lilv_node_as_string(lilv_plugin_get_name(m_plugin));
}

size_t Plugin::instanceSize() const
{
    return m_constructor->instance_size(m_descriptor);
}

size_t Plugin::instanceAlignment() const
{
    return m_constructor->instance_alignment(m_descriptor);    
}

LV2_Handle Plugin::construct(void* location, double sampleRate) const
{
    return m_constructor->instantiate(m_descriptor, location, sampleRate);
}

//const Descriptor& Plugin::descriptor() const
//{
//    return *m_descriptor;
//}

UriMap::UriMap()
{ }

LV2_URID UriMap::insert(const char* uri)
{
    LV2_URID urid = m_uriToId.size() + 1;
    if (urid == 0)
        BOOST_THROW_EXCEPTION(Exception() << ErrorInfoString("No more URIDs left"));
    m_uriToId[uri] = urid;
    m_idToUri[urid] = uri;
    return urid;
}

LV2_URID UriMap::map(const char* uri)
{
    UriToId::const_iterator it = m_uriToId.find(uri);
    return it == m_uriToId.end()
            ? insert(uri)
            : it->second;
}

const char* UriMap::unmap(LV2_URID urid) const
{
    IdToUri::const_iterator it = m_idToUri.find(urid);
    return it == m_idToUri.end() ? 0 : it->second;
}

static LV2_URID UriMap_map(LV2_URID_Map_Handle handle,
                           const char*         uri)
{
    return static_cast<UriMap*>(handle)->map(uri);
}

static const char* UriMap_unmap(LV2_URID_Unmap_Handle handle,
                                LV2_URID              urid)
{
    return static_cast<UriMap*>(handle)->unmap(urid);
}

void Manager::addFeature(const char* uri, void* data)
{
    LV2_Feature* f = new LV2_Feature;
    f->URI = uri;
    f->data = data;
    m_features.push_back(f);
}

Manager::Manager(Loader& loader)
    : m_loader(loader)
{
    m_world = lilv_world_new();
    if (m_world == 0)
        BOOST_THROW_EXCEPTION(Exception() << ErrorInfoString("`lilv_world_new' failed"));
    
    // Initialize features

    // http://lv2plug.in/ns/lv2core#hardRTCapable
    addFeature( LV2_CORE_URI "#hardRTCapable" );

    // http://mescaline.puesnada.es/lv2/ext/rt-instantiate#rtInstantiation
    addFeature( PUESNADA_URI "/ext/rt-instantiate#rtInstantiation" );

    // http://lv2plug.in/ns/ext/urid
    m_lv2UridMap.handle = &m_uriMap;
    m_lv2UridMap.map = UriMap_map;
    addFeature( LV2_URID_MAP_URI, &m_lv2UridMap );

    m_lv2UridUnmap.handle = &m_uriMap;
    m_lv2UridUnmap.unmap = UriMap_unmap;
    addFeature( LV2_URID_UNMAP_URI, &m_lv2UridUnmap );
}

Manager::~Manager()
{
    lilv_world_free(m_world);
    BOOST_FOREACH(const LV2_Feature* f, m_features) {
        delete f;
    }
}

const LV2_Feature* const* Manager::features()
{
    return &m_features[0];
}

const Manager::PluginHandle& Manager::lookup(const char* uri) const
{
    return m_plugins.find(uri)->second;
}

void Manager::loadPlugins()
{
    NodePtr extensionData(newUri(LV2_CORE_URI "#extensionData"));
    NodePtr hardRTCapable(newUri(LV2_CORE_URI "#hardRTCapable"));
    NodePtr rtInstantiation(newUri(PUESNADA_URI "/ext/rt-instantiate#rtInstantiation"));
    NodePtr rtInstantiateInterface(newUri(PUESNADA_URI "/ext/rt-instantiate#Interface"));

    lilv_world_load_all(m_world);
    const LilvPlugins* plugins = lilv_world_get_all_plugins(m_world);
    cout << "Mescaline::Audio::Plugin::Manager: " << lilv_plugins_size(plugins) << " plugins found" << endl;

    LILV_FOREACH(plugins, it, plugins) {
        const LilvPlugin* lilvPlugin = lilv_plugins_get(plugins, it);
        const char* uri = lilv_node_as_uri(lilv_plugin_get_uri(lilvPlugin));

        NodesPtr allFeatures(new Nodes(lilv_plugin_get_supported_features(lilvPlugin)));
        NodesPtr requiredFeatures(new Nodes(lilv_plugin_get_required_features(lilvPlugin)));
        NodesPtr optionalFeatures(new Nodes(lilv_plugin_get_optional_features(lilvPlugin)));
        NodesPtr extensions(new Nodes(lilv_plugin_get_value(lilvPlugin, extensionData->impl())));

        if (   lilv_nodes_contains(allFeatures->impl(), hardRTCapable->impl())
            && lilv_nodes_contains(allFeatures->impl(), rtInstantiation->impl())
            && lilv_nodes_contains(extensions->impl(), rtInstantiateInterface->impl()) )
        {
            cout << "Loading plugin " << uri << " ... " << endl;
            boost::shared_ptr<Plugin> plugin(new Plugin(*this, lilvPlugin));
            m_plugins[plugin->uri()] = plugin;
        }
    }
}

LV2_URID_Map* Manager::lv2UridMap()
{
    return &m_lv2UridMap;
}

LV2_URID_Unmap* Manager::lv2UridUnmap()
{
    return &m_lv2UridUnmap;
}

Node::Node(LilvNode* node)
    : m_impl(node)
{ }

Node::~Node()
{
    lilv_node_free(m_impl);
}

Nodes::Nodes(LilvNodes* impl)
    : m_impl(impl)
{ }

Nodes::~Nodes()
{
    lilv_nodes_free(m_impl);
}

NodePtr Manager::newUri(const char* uri)
{
    return NodePtr(new Node(lilv_new_uri(m_world, uri)));
}
