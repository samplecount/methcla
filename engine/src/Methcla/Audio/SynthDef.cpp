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

#include <boost/optional.hpp>
#include <iostream>
#include <limits>
#include <memory>
#include <utility>

#include "serd/serd.h"

using namespace boost;
using namespace Methcla::Lilv;
using namespace Methcla::Audio;
using namespace std;

// typedef pair<const LV2_Descriptor*, std::shared_ptr<Methcla::Plugin::Binary> > PluginDescriptor;

static const LV2_Descriptor* loadPlugin(const std::shared_ptr<PluginLibrary>& lib, const LilvPlugin* plugin)
{
    // const std::string pluginUri(lilv_node_as_uri(lilv_plugin_get_uri(plugin)));
    // const boost::filesystem::path libraryPath(lilv_uri_to_path(lilv_node_as_uri(lilv_plugin_get_library_uri(plugin))));
    // std::shared_ptr<Methcla::Plugin::Binary> binary(loader->load(pluginUri, libraryPath));
    // auto binary = nullptr;

    // if (binary == std::shared_ptr<Methcla::Plugin::Binary>())
        // return optional<PluginDescriptor>();

    // LV2_Descriptor_Function df = reinterpret_cast<LV2_Descriptor_Function>(symbolTable.lookup(pluginUri, "lv2_descriptor"));
    // if (df == nullptr) return optional<PluginDescriptor>();

    const LV2_Descriptor* result = nullptr;

    for (uint32_t i = 0; true; ++i) {
        const LV2_Descriptor* ld = lib->get(i);

        if (ld == nullptr) {
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

    return result;
}

Methcla::Audio::FloatPort::FloatPort( Type type, uint32_t index, const char* symbol
                    , float minValue, float maxValue, float defaultValue )
    : Port(type, index, symbol)
    , m_minValue(isnan(minValue) ? -numeric_limits<float>::max() : minValue)
    , m_maxValue(isnan(maxValue) ? numeric_limits<float>::max() : maxValue)
    , m_defaultValue(isnan(defaultValue) ? 0 : defaultValue)
{ }

std::shared_ptr<PluginLibrary> PluginLibrary::create(const char* bundlePath, const LV2_Feature* const* features, std::shared_ptr<Methcla::Plugin::Library> lib)
{
    auto df = reinterpret_cast<LV2_Descriptor_Function>(lib->symbol("lv2_descriptor"));

    if (df == nullptr) {
        auto ldf = reinterpret_cast<LV2_Lib_Descriptor_Function>(lib->symbol("lv2_lib_descriptor"));
        if (ldf == nullptr)
            return nullptr;
        auto desc = ldf(bundlePath, features);
        if (desc == nullptr)
            return nullptr;
        return std::make_shared<PluginLibrary>(desc, lib);
    } else {
        return std::make_shared<PluginLibrary>(df, lib);
    }
}

PluginLibrary::~PluginLibrary()
{
    if (m_libDesc != nullptr) {
        m_libDesc->cleanup(m_libDesc->handle);
    }
}

std::shared_ptr<PluginLibrary> PluginLibrary::create(const char* bundlePath, const LV2_Feature* const* features, LV2_Lib_Descriptor_Function ldf)
{
    auto desc = ldf(bundlePath, features);
    return std::make_shared<PluginLibrary>(desc, nullptr);
}

std::shared_ptr<PluginLibrary> PluginLibrary::create(LV2_Descriptor_Function df)
{
    return std::make_shared<PluginLibrary>(df, nullptr);
}

Plugin::Plugin(PluginManager& manager, std::shared_ptr<PluginLibrary> library, const LilvPlugin* plugin)
    : m_library(library)
    , m_plugin(plugin)
    , m_descriptor(0)
    , m_features(manager.features())
    , m_numAudioInputs(0)
    , m_numAudioOutputs(0)
    , m_numControlInputs(0)
    , m_numControlOutputs(0)
{
    m_bundlePath = lilv_uri_to_path(lilv_node_as_uri(lilv_plugin_get_bundle_uri(m_plugin)));

    Lilv::NodePtr audioClass(manager.newUri(LV2_CORE_URI "#AudioPort"));
    Lilv::NodePtr controlClass(manager.newUri(LV2_CORE_URI "#ControlPort"));
    Lilv::NodePtr inputClass(manager.newUri(LV2_CORE_URI "#InputPort"));
    Lilv::NodePtr outputClass(manager.newUri(LV2_CORE_URI "#OutputPort"));

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

    // LilvInstance* instance = lilv_plugin_instantiate(plugin, 44100, m_features);

    m_descriptor = loadPlugin(m_library, m_plugin);

    if (m_descriptor) {
        NodePtr extensionData(manager.newUri(LV2_CORE_URI "#extensionData"));
        NodesPtr extensions(std::make_shared<Nodes>(lilv_plugin_get_value(plugin, extensionData->impl())));

        m_constructor = static_cast<const LV2_RT_Instantiate_Interface*>(
                            m_descriptor->extension_data(LV2_RT_INSTANTIATE__INTERFACE) );

        cout << "Plugin descriptor for " << uri() << " loaded (" << m_descriptor << "):"
             << "    instance size: " << instanceSize() << endl
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

LV2_Handle Plugin::construct(void* location, double sampleRate, const LV2_Feature* const* features) const
{
    LV2_Feature* nullFeatures[1] = { nullptr };
    return m_constructor->instantiate(m_descriptor, sampleRate, m_bundlePath, features == nullptr ? nullFeatures : features, location);
}

const LV2_Descriptor* PluginLibrary::get(uint32_t index)
{
    return m_descFunc ? m_descFunc(index) : m_libDesc->get_plugin(m_libDesc->handle, index);
}

void PluginManager::addFeature(const char* uri, void* data)
{
    LV2_Feature* f = new LV2_Feature;
    f->URI = uri;
    f->data = data;
    m_features.push_back(f);
}

PluginManager::PluginManager(const StaticLibraryMap& libs)
    : m_staticLibs(libs)
{
    m_world = lilv_world_new();
    if (m_world == 0)
        BOOST_THROW_EXCEPTION(Exception() << ErrorInfoString("`lilv_world_new' failed"));

    // Initialize features

    // http://lv2plug.in/ns/lv2core#hardRTCapable
    addFeature( LV2_CORE_URI "#hardRTCapable" );

    // http://methc.la/lv2/ext/rt-instantiate#rtInstantiation
    addFeature( METHCLA_LV2_URI "/ext/rt-instantiate#rtInstantiation" );
}

PluginManager::~PluginManager()
{
    lilv_world_free(m_world);
    for (const LV2_Feature* f : m_features)
        delete f;
}

const LV2_Feature* const* PluginManager::features()
{
    return &m_features[0];
}

const std::shared_ptr<Plugin>& PluginManager::lookup(const char* name) const
{
    return m_plugins.find(name)->second;
}

void PluginManager::loadPlugins(const boost::filesystem::path& directory)
{
    NodePtr extensionData(newUri(LV2_CORE_URI "#extensionData"));
    NodePtr hardRTCapable(newUri(LV2_CORE_URI "#hardRTCapable"));
    NodePtr rtInstantiation(newUri(METHCLA_LV2_URI "/ext/rt-instantiate#rtInstantiation"));
    NodePtr rtInstantiateInterface(newUri(METHCLA_LV2_URI "/ext/rt-instantiate#Interface"));

    setenv("LV2_PATH", directory.c_str(), true);

    lilv_world_load_all(m_world);
    const LilvPlugins* plugins = lilv_world_get_all_plugins(m_world);
    cout << "Methcla::Audio::Plugin::PluginManager: " << lilv_plugins_size(plugins) << " plugins found" << endl;

    LILV_FOREACH(plugins, it, plugins) {
        const LilvPlugin* lilvPlugin = lilv_plugins_get(plugins, it);
        const char* pluginUri = lilv_node_as_uri(lilv_plugin_get_uri(lilvPlugin));

        NodesPtr allFeatures(std::make_shared<Nodes>(lilv_plugin_get_supported_features(lilvPlugin)));
        NodesPtr requiredFeatures(std::make_shared<Nodes>(lilv_plugin_get_required_features(lilvPlugin)));
        NodesPtr optionalFeatures(std::make_shared<Nodes>(lilv_plugin_get_optional_features(lilvPlugin)));
        NodesPtr extensions(std::make_shared<Nodes>(lilv_plugin_get_value(lilvPlugin, extensionData->impl())));

        if (   lilv_nodes_contains(allFeatures->impl(), hardRTCapable->impl())
            && lilv_nodes_contains(allFeatures->impl(), rtInstantiation->impl())
            && lilv_nodes_contains(extensions->impl(), rtInstantiateInterface->impl()) )
        {
            cout << "Loading plugin " << pluginUri << " ... " << endl;

            // load and initialize library if necessary
            const char* libUri = lilv_node_as_uri(lilv_plugin_get_library_uri(lilvPlugin));
            const char* bundlePath = lilv_uri_to_path(lilv_node_as_uri(lilv_plugin_get_bundle_uri(lilvPlugin)));

            // Add library symbol prefix for static loader
            // <plugin> staticLoader:symbolPrefix ?prefix
            //LilvNode* symbolPrefixPred = lilv_new_uri(m_world, "http://methc.la/lv2/ext/static-loader#symbolPrefix");
            //std::string symbolPrefix;

            //LilvNodes* results =
                //lilv_world_find_nodes(m_world, lilv_plugin_get_uri(lilvPlugin), symbolPrefixPred, nullptr);

            //LILV_FOREACH(nodes, i, results) {
                //const LilvNode* node = lilv_nodes_get(results, i);
                //if (lilv_node_is_string(node)) {
                    //symbolPrefix = lilv_node_as_string(node);
                //}
            //}
            //lilv_nodes_free(results);
            //lilv_node_free(symbolPrefixPred);

            //m_loader.addLibrary(boost::filesystem::path(lilv_uri_to_path(libUri)), symbolPrefix);

            auto libIter = m_libs.find(libUri);
            if (libIter == m_libs.end()) {
                auto staticLib = m_staticLibs.find(pluginUri);
                if (staticLib != m_staticLibs.end()) {
                    libIter = m_libs.insert(std::make_pair(libUri, PluginLibrary::create(bundlePath, features(), staticLib->second))).first;
                } else {
                    auto lib = m_loader.open(lilv_uri_to_path(libUri));
                    libIter = m_libs.insert(std::make_pair(libUri, PluginLibrary::create(bundlePath, features(), lib))).first;
                }
            }

            auto plugin = make_shared<Plugin>(*this, libIter->second, lilvPlugin);
            m_plugins[plugin->uri()] = plugin;
        }
    }
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

NodePtr PluginManager::newUri(const char* uri)
{
    return std::make_shared<Node>(lilv_new_uri(m_world, uri));
}
