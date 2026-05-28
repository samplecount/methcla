#include "Methcla/Plugin/Manager.hpp"

#include <filesystem>
#include <stdexcept>

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

Manager::Manager(std::unique_ptr<Loader>&& loader)
: m_loader(std::move(loader))
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

namespace {
    template <typename F>
    void withDirectory(Methcla_Host* host, const std::string& directory, F func)
    {
        std::error_code                     ec;
        std::filesystem::directory_iterator it(directory, ec);
        if (ec)
        {
            methcla_host_log_line(host, kMethcla_LogWarn,
                                  ("Cannot access plugin directory " +
                                   directory + ": " + ec.message())
                                      .c_str());
        }
        else
        {
            for (auto& entry : it)
            {
                if (entry.is_regular_file())
                {
                    func(entry.path().string());
                }
            }
        }
    }
} // namespace

void Manager::loadPlugins(Methcla_Host* host, const std::string& directory)
{
    if (m_loader)
    {
        withDirectory(host, directory, [&](const std::string& path) {
            auto plugin = m_loader->open(path);
            if (plugin)
            {
                auto func = reinterpret_cast<Methcla_LibraryFunction>(
                    plugin->symbol("methcla_plugin_load"));
                if (func)
                {
                    Methcla_Library* lib = func(host, directory.c_str());
                    if (lib != nullptr)
                    {
                        m_libs.push_back(
                            std::make_shared<Library>(lib, plugin));
                    }
                }
            }
        });
    }
    else
    {
        throw std::runtime_error(
            "No dynamic loader available on this platform");
    }
}
