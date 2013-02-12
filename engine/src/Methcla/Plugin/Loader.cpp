#include "Methcla/Plugin/Loader.hpp"

using namespace Methcla::Plugin;

StaticBinary::StaticBinary(const std::string& name, Function symbol)
{
    addSymbol(name, symbol);
}

void StaticBinary::addSymbol(const std::string& name, Function symbol)
{
    m_symbols[name] = symbol;
}

Function StaticBinary::symbol(const std::string& name)
{
    return m_symbols.at(name);
}

void StaticLoader::addModule(const std::string& uri, const StaticBinary& module)
{
    m_modules[uri] = std::shared_ptr<Binary>(new StaticBinary(module));
}

std::shared_ptr<Binary> StaticLoader::load(const std::string& uri, const boost::filesystem::path&)
{
    return m_modules.at(uri);
}

std::string mkSymbol(const std::string& uri, const std::string& name)
{
    return uri + ":" + name;
}

SymbolTable::SymbolTable(const Methcla_Library_Symbol* symbols)
{
    if (symbols != nullptr) {
        for (const Methcla_Library_Symbol* cur = symbols; cur->uri != nullptr; cur++) {
            m_symbols[mkSymbol(cur->uri, cur->name)] = cur->function;
        }
    }
}

Function SymbolTable::lookup(const std::string& uri, const std::string& name) const
{
    auto it = m_symbols.find(mkSymbol(uri, name));
    return it == m_symbols.end() ? nullptr : it->second;
}
