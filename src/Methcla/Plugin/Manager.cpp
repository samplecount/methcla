#include "Methcla/Plugin/Manager.hpp"

#include <tinydir.h>

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
    void withDirectory(const std::string& directory, F func)
    {
        std::shared_ptr<tinydir_dir> dir(new tinydir_dir, tinydir_close);
        tinydir_open(dir.get(), directory.c_str());
        while (dir->has_next)
        {
            tinydir_file file;
            tinydir_readfile(dir.get(), &file);
            if (file.is_reg)
            {
                func(file.path);
            }
            tinydir_next(dir.get());
        }
    }
} // namespace

void Manager::loadPlugins(Methcla_Host* host, const std::string& directory)
{
    if (m_loader)
    {
        withDirectory(directory, [&](const std::string& path) {
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
