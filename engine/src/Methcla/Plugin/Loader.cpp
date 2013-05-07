#include "Methcla/Plugin/Loader.hpp"

#include <dlfcn.h>

// #include "lilv/lilv.h"

using namespace Methcla::Plugin;
using Methcla::Exception;
using Methcla::ErrorInfoString;

void clear_dlerror()
{
    dlerror();
}

void check_dlerror()
{
    const char* errorString = dlerror();
    if (errorString != nullptr) {
        BOOST_THROW_EXCEPTION(Exception() << ErrorInfoString(errorString));
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

// std::string mkSymbol(const std::string& uri, const std::string& name)
// {
//     return uri + ":" + name;
// }
// 
// SymbolTable::SymbolTable(const Methcla_Library_Symbol* symbols)
// {
//     if (symbols != nullptr) {
//         for (const Methcla_Library_Symbol* cur = symbols; cur->uri != nullptr; cur++) {
//             m_symbols[mkSymbol(cur->uri, cur->name)] = cur->function;
//         }
//     }
// }
// 
// Function SymbolTable::lookup(const std::string& uri, const std::string& name) const
// {
//     auto it = m_symbols.find(mkSymbol(uri, name));
//     return it == m_symbols.end() ? nullptr : it->second;
// }
// 
// Library::Library(DynLoader& loader, const std::string& path, void* impl)
//     : m_loader(loader)
//     , m_path(path)
//     , m_impl(impl)
// {
// }
// 
// Library::~Library()
// {
//     if (m_impl != nullptr)
//         dlclose(m_impl);
//     m_loader.remove(m_path);
// }
// 
// Function Library::symbol(const std::string& name)
// {
//     return m_impl == nullptr ? dlfunc(RTLD_MAIN_ONLY, name.c_str()) : dlfunc(m_impl, name.c_str());
// }
// 
// std::shared_ptr<Library> DynLoader::open(const std::string& path) throw(Exception)
// {
//     auto it = m_libs.find(path);
//     if (it != m_libs.end()) {
//         return it->second.lock();
//     }
//     dlerror();
//     void* handle = dlopen(path.c_str(), RTLD_NOW);
//     checkDLError("dlopen failed");
//     auto lib = std::make_shared<Library>(*this, path, handle);
//     m_libs[path] = lib;
//     return lib;
// }

// private:
//     friend class Library;
//     void remove(const std::string& path);
// 
// private:
//     typedef std::unordered_map<std::string,std::weak_ptr<Library>> LibraryMap;
//     LibraryMap m_libs;

// void DynLoader::remove(const std::string& path)
// {
//     m_libs.erase(path);
// }

// Library::Library(LV2_Descriptor_Function df, const LV2_Lib_Descriptor* ld)
//     : m_lv2DescriptorFunction(df)
//     , m_lv2LibDescriptor(ld)
// {
// }
// 
// ~Library()
// {
//     m_lv2LibDescriptor->cleanup(lv2LibDescriptor->handle);
// }
// 
// const LV2_Descriptor* Library::getPlugin(uint32_t index)
// {
//     if (m_lv2DescriptorFunction) {
//         return m_lv2DescriptorFunction(index);
//     } else if (m_lv2LibDescriptor) {
//         return m_lv2LibDescriptor->get_plugin(m_lv2LibDescriptor->handle, index);
//     } else {            
//         return nullptr;
//     }
// }
