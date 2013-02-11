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
