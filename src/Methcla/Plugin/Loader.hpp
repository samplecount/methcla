// Copyright 2012-2013 Samplecount S.L.
//
// SPDX-License-Identifier: Apache-2.0

#ifndef METHCLA_PLUGIN_LOADER_HPP_INCLUDED
#define METHCLA_PLUGIN_LOADER_HPP_INCLUDED

#include <memory>
#include <string>
#include <unordered_map>

namespace Methcla { namespace Plugin {

    typedef void (*Function)();

    //* Dynamically loaded binary module
    class Library
    {
    public:
        virtual ~Library();
        virtual Function symbol(const std::string& name) = 0;
    };

    //* Dynamic loader.
    class Loader
    {
    public:
        virtual ~Loader();
        virtual std::shared_ptr<Library> open(const std::string& path) = 0;
    };

    std::unique_ptr<Loader> defaultLoader();

}} // namespace Methcla::Plugin

#endif // METHCLA_PLUGIN_LOADER_HPP_INCLUDED
