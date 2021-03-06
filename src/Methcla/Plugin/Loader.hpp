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
