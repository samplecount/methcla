// Copyright (C) 2012-2019 Samplecount S.L.
// Copyright (C) 2026 Methcla contributors
//
// SPDX-License-Identifier: Apache-2.0

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
        Manager(std::unique_ptr<Loader>&& loader);

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
            Methcla_Library*                 m_lib;
            std::shared_ptr<Plugin::Library> m_plugin;
        };

    private:
        typedef std::list<std::shared_ptr<Library>> Libraries;
        Libraries                                   m_libs;
        std::unique_ptr<Loader>                     m_loader;
    };

}} // namespace Methcla::Plugin

#endif // METHCLA_PLUGIN_MANAGER_HPP_INCLUDED
