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

#ifndef METHCLA_PLUGIN_LOADER_HPP_INCLUDED
#define METHCLA_PLUGIN_LOADER_HPP_INCLUDED

#include <methcla/engine.h>

#include <boost/filesystem.hpp>
#include <memory>
#include <string>
#include <unordered_map>

namespace Methcla { namespace Plugin {

typedef Methcla_Library_Function Function;

//* Dynamically loaded binary module
class Binary
{
public:
    virtual ~Binary() { }
    virtual Function symbol(const std::string& name) = 0;
};

//* Dynamic loader.
class Loader
{
public:
    virtual ~Loader() { }
    //* Load the binary module for a plugin URI and a module path.
    virtual std::shared_ptr<Binary> load(const std::string& uri, const boost::filesystem::path& path) = 0;
};

// These are for systems without dynamically loadable modules.

//* Static binary module.
class StaticBinary : public Binary
{
public:
    StaticBinary() = default;
    StaticBinary(const StaticBinary&) = default;
    StaticBinary(const std::string& name, Function symbol);

    //* Add a function symbol.
    void addSymbol(const std::string& name, Function symbol);

    virtual Function symbol(const std::string& name) override;

private:
    typedef std::unordered_map<std::string,Function> SymbolMap;
    SymbolMap m_symbols;
};

//* Static binary loader.
class StaticLoader : public Loader
{
public:
    //* Add a binary module.
    void addModule(const std::string& uri, const StaticBinary& module);

    //* There can only be a single binary module registered for a given plugin URI (the module path is ignored).
    virtual std::shared_ptr<Binary> load(const std::string& uri, const boost::filesystem::path& path) override;

private:
    typedef std::unordered_map<std::string,std::shared_ptr<Binary>>
            ModuleMap;

    ModuleMap m_modules;
};

class SymbolTable
{
public:
    SymbolTable(const Methcla_Library_Symbol* symbols);

    Function lookup(const std::string& uri, const std::string& name) const;

private:
    typedef std::unordered_map<std::string,Function> SymbolMap;
    SymbolMap m_symbols;
};

}; };

#endif // METHCLA_PLUGIN_LOADER_HPP_INCLUDED
