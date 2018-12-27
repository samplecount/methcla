#include "Methcla/Plugin/Loader.hpp"

#include <stdexcept>

#include <dlfcn.h>

using namespace Methcla::Plugin;

void clear_dlerror() { dlerror(); }

void check_dlerror()
{
    const char* errorString = dlerror();
    if (errorString != nullptr)
    {
        throw std::runtime_error(errorString);
    }
}

typedef Function (*DLFuncFunction)(void*, const char*);

Function dlfunc(void* handle, const char* name)
{
    clear_dlerror();
    DLFuncFunction f = (DLFuncFunction)dlsym;
    Function       g = f(handle, name);
    check_dlerror();
    return g;
}
