#include "Methcla/Plugin/Loader.hpp"

#include <dlfcn.h>

using namespace Methcla::Plugin;

void clear_dlerror()
{
    dlerror();
}

void check_dlerror()
{
    const char* errorString = dlerror();
    if (errorString != nullptr) {
        throw std::runtime_error(errorString);
    }    
}

typedef Function (*DLFuncFunction)(void*, const char*);

Function dlfunc(void* handle, const char* name)
{
    clear_dlerror();
    DLFuncFunction f = (DLFuncFunction)dlsym;
    Function g = f(handle, name);
    check_dlerror();
    return g;
}

StaticLibrary::StaticLibrary(const std::string& prefix)
    : m_prefix(prefix)
{
}

Function StaticLibrary::symbol(const std::string& name)
{
    return dlfunc(RTLD_DEFAULT, (m_prefix + name).c_str());
}

void StaticLoader::addLibrary(const std::string& path, const std::string& symbolPrefix)
{
    m_libs[path] = std::shared_ptr<Library>(new StaticLibrary(symbolPrefix));
}

std::shared_ptr<Library> StaticLoader::open(const std::string& path)
{
    return m_libs.at(path);
}
