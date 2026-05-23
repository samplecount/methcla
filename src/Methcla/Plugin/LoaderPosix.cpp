#include "Methcla/Config.h"
#include "Methcla/Plugin/Loader.hpp"

#include <stdexcept>

#include <dlfcn.h>

using namespace Methcla::Plugin;

Library::~Library()
{}

Loader::~Loader()
{}

namespace {
    const std::string kPluginExtension = METHCLA_PLUGIN_EXTENSION;

    std::string splitExtension(const std::string& s)
    {
        const size_t i = s.rfind('.', s.length());
        if (i != std::string::npos)
        {
            return (s.substr(i, s.length() - i));
        }

        return "";
    }

    void clear_dlerror()
    {
        dlerror();
    }

    void check_dlerror()
    {
        const char* errorString = dlerror();
        if (errorString != nullptr)
        {
            throw std::runtime_error(errorString);
        }
        else
        {
            throw std::runtime_error("dlerror");
        }
    }

    Function dlfunc(void* handle, const char* name)
    {
        clear_dlerror();
        typedef Function (*DLFuncFunction)(void*, const char*);
        DLFuncFunction f = (DLFuncFunction)dlsym;
        Function       g = f(handle, name);
        if (g == nullptr)
        {
            check_dlerror();
        }
        return g;
    }

    class DLLibrary : public Library
    {
        void* m_handle;

    public:
        DLLibrary(void* handle)
        : m_handle(handle)
        {}

        ~DLLibrary()
        {
            dlclose(m_handle);
        }

        virtual Function symbol(const std::string& name) override
        {
            return dlfunc(m_handle, name.c_str());
        }
    };

    class DLLoader : public Loader
    {
    public:
        virtual std::shared_ptr<Library> open(const std::string& path) override
        {
            if (splitExtension(path) == kPluginExtension)
            {
                clear_dlerror();
                void* handle =
                    dlopen(path.c_str(), RTLD_NOW | RTLD_LOCAL);
                if (handle == nullptr)
                {
                    check_dlerror();
                    return nullptr;
                }
                return std::make_shared<DLLibrary>(handle);
            }
            return nullptr;
        }
    };
} // namespace

std::unique_ptr<Loader> Methcla::Plugin::defaultLoader()
{
    return std::unique_ptr<Loader>(new DLLoader());
}
