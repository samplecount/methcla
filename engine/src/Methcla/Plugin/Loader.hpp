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
#include "Methcla/Exception.hpp"

#include <memory>
#include <string>
#include <unordered_map>

namespace Methcla { namespace Plugin {

typedef void (*Function)();

//* Dynamically loaded binary module
class Library
{
public:
    virtual ~Library() { }
    virtual Function symbol(const std::string& name) = 0;
};


//* Dynamic loader.
class Loader
{
public:
    virtual ~Loader() { }
    virtual std::shared_ptr<Library> open(const std::string& path) = 0;
};

// These are for systems without dynamically loadable modules.

//* Static binary module.
class StaticLibrary : public Library
{
public:
    // typedef std::unordered_map<std::string,Methcla_Library_Function> SymbolMap;

    StaticLibrary() = default;
    StaticLibrary(const StaticLibrary&) = default;
    // StaticLibrary(const SymbolMap& symbols);
    StaticLibrary(const std::string& prefix);

    virtual Function symbol(const std::string& name) override;

private:
    // SymbolMap m_symbols;
    std::string m_prefix;
};

//* Static library loader.
class StaticLoader : public Loader
{
public:
    //* Add a library.
    void addLibrary(const std::string& path, const std::string& symbolPrefix);

    virtual std::shared_ptr<Library> open(const std::string& path) override;

private:
    typedef std::unordered_map<std::string,std::shared_ptr<Library>>
            LibraryMap;
    LibraryMap m_libs;
};

// class SymbolTable
// {
// public:
//     SymbolTable(const Methcla_Library_Symbol* symbols);
//
//     Function lookup(const std::string& uri, const std::string& name) const;
//
// private:
//     typedef std::unordered_map<std::string,Function> SymbolMap;
//     SymbolMap m_symbols;
// };

} }

#endif // METHCLA_PLUGIN_LOADER_HPP_INCLUDED
