#include "Methcla/Plugin/Manager.hpp"

using namespace Methcla::Plugin;

Manager::Library::Library(Methcla_Library*                 lib,
                          std::shared_ptr<Plugin::Library> plugin)
: m_lib(lib)
, m_plugin(plugin)
{}

Manager::Library::~Library()
{
    methcla_library_destroy(m_lib);
}

Manager::Manager()
{}

void Manager::loadPlugins(Methcla_Host*                             host,
                          const std::list<Methcla_LibraryFunction>& funcs)
{
    for (auto f : funcs)
    {
        Methcla_Library* lib = f(host, ".");
        if (lib != nullptr)
        {
            m_libs.push_back(std::make_shared<Library>(lib));
        }
    }
}

void Manager::loadPlugins(Methcla_Host* /*host*/,
                          const std::string& /*directory*/)
{
    throw std::runtime_error("PluginManager::loadPlugins not yet implemented");
}
