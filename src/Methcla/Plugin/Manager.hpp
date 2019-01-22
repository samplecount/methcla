// Copyright 2012-2019 Samplecount S.L.
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

#ifndef METHCLA_PLUGIN_MANAGER_HPP_INCLUDED
#define METHCLA_PLUGIN_MANAGER_HPP_INCLUDED

#include "Methcla/Plugin/Loader.hpp"

#include <methcla/plugin.h>

#include <list>
#include <memory>

namespace Methcla { namespace Plugin {

    class Manager
    {
    public:
        Manager();

        Manager(const Manager&) = delete;
        Manager& operator=(const Manager&) = delete;

        //* Load plugins from static functions.
        void loadPlugins(Methcla_Host*                             host,
                         const std::list<Methcla_LibraryFunction>& funcs);

        //* Load plugins from directory.
        void loadPlugins(Methcla_Host* host, const std::string& directory);

    private:
        //* Plugin library.
        class Library
        {
        public:
            Library(Methcla_Library*                 lib,
                    std::shared_ptr<Plugin::Library> plugin = nullptr);
            ~Library();

            Library(const Library&) = delete;
            Library& operator=(const Library&) = delete;

        private:
            Methcla_Library*                    m_lib;
            Memory::shared_ptr<Plugin::Library> m_plugin;
        };

    private:
        typedef std::list<std::shared_ptr<Library>> Libraries;
        Libraries                                   m_libs;
    };

}} // namespace Methcla::Plugin

#endif // METHCLA_PLUGIN_MANAGER_HPP_INCLUDED
