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

#ifndef METHCLA_ENGINE_HPP_INCLUDED
#define METHCLA_ENGINE_HPP_INCLUDED

#include <methcla/engine.h>
#include "Methcla/LV2/Atom.hpp"

#include <boost/current_function.hpp>
#include <future>
#include <iostream>
#include <memory>
#include <string>
#include <unordered_map>

#include <boost/serialization/strong_typedef.hpp>

#include "lv2/lv2plug.in/ns/ext/patch/patch.h"

namespace Methcla
{
    BOOST_STRONG_TYPEDEF(uint32_t, NodeId);

    class Engine
    {
    public:
        Engine(const Methcla_Option* options)
            : m_engine(methcla_engine_new(options))
            , m_map({ m_engine, engineMapUri })
            , m_unmap({ m_engine, engineUnmapUri })
            , m_forge(&m_map)
            , m_parser(&m_map)
        { }
        ~Engine()
        {
            methcla_engine_free(m_engine);
        }

        void* impl()
        {
            return methcla_engine_impl(m_engine);
        }

        void start()
        {
            methcla_engine_start(m_engine);
        }

        void stop()
        {
            methcla_engine_stop(m_engine);
        }

        LV2_URID map_uri(const char* uri)
        {
            return methcla_engine_map_uri(m_engine, uri);
        }

        const char* unmap_uri(LV2_URID urid)
        {
            return methcla_engine_unmap_uri(m_engine, urid);
        }

        LV2_Atom* request(LV2_Atom* request)
        {
            std::cout << BOOST_CURRENT_FUNCTION << std::endl;
            print(std::cout, request, 4);

            std::promise<LV2_Atom*> sink;
            methcla_engine_request(m_engine, responseHandler, &sink, request);
            return sink.get_future().get();
        }

        const LV2::Forge& forge()
        {
            return m_forge;
        }

        const LV2::Parser& parser()
        {
            return m_parser;
        }

        void print(std::ostream& out, const LV2_Atom* atom, size_t indent=0)
        {
            LV2::Printer(&m_map, &m_unmap).print(out, atom, indent);
            out << std::endl;
        }

    private:
        static void responseHandler(void* data, LV2_Atom* request, const LV2_Atom* inResponse)
        {
            auto sink = static_cast<std::promise<LV2_Atom*>*>(data);
            LV2_Atom* response = nullptr;
            if (request->size >= inResponse->size) {
                memcpy(request, inResponse, sizeof(LV2_Atom) + inResponse->size);
                response = request;
            } else {
                response = (LV2_Atom*)malloc(sizeof(LV2_Atom) + inResponse->size);
                memcpy(response, inResponse, sizeof(LV2_Atom) + inResponse->size);
                free(request);
            }
            sink->set_value(response);
        }

        static LV2_URID engineMapUri(LV2_URID_Map_Handle handle, const char* uri)
        {
            return methcla_engine_map_uri(static_cast<Methcla_Engine*>(handle), uri);
        }

        static const char* engineUnmapUri(LV2_URID_Unmap_Handle handle, LV2_URID urid)
        {
            return methcla_engine_unmap_uri(static_cast<Methcla_Engine*>(handle), urid);
        }

    private:
        Methcla_Engine* m_engine;
        LV2_URID_Map    m_map;
        LV2_URID_Unmap  m_unmap;
        LV2::Forge      m_forge;
        LV2::Parser     m_parser;
    };

    template <class T> struct FreeDeleter
    {
        void operator()(T* ptr) const { free(ptr); }
    };

    NodeId synth(Engine& engine, const char* synthDef)
    {
        uint8_t* buffer = (uint8_t*)malloc(8192);
        LV2::Forge forge(engine.forge());
        forge.setBuffer(buffer, 8192);
        {
            LV2::ObjectFrame frame(forge, 0, engine.map_uri(LV2_PATCH_PREFIX "Insert"));
            forge << LV2::Property(engine.map_uri(LV2_PATCH_PREFIX "subject"));
            {
                LV2::ObjectFrame frame(forge, 0, engine.map_uri(METHCLA_ENGINE_PREFIX "Node"));
                forge << LV2::Property(engine.map_uri(METHCLA_ENGINE_PREFIX "id"))
                      << int32_t(0);
            }
            forge << LV2::Property(engine.map_uri(LV2_PATCH_PREFIX "body"));
            {
                LV2::ObjectFrame frame(forge, 0, engine.map_uri(METHCLA_ENGINE_PREFIX "Synth"));
                forge << LV2::Property(engine.map_uri(METHCLA_ENGINE_PREFIX "plugin"))
                      << engine.map_uri(synthDef);
            }
        }

        LV2_Atom* requestAtom = reinterpret_cast<LV2_Atom*>(buffer);
        auto responseAtom = std::unique_ptr<LV2_Atom, FreeDeleter<LV2_Atom>>(engine.request(requestAtom));
        auto response = engine.parser().cast<const LV2_Atom_Object*>(responseAtom.get());

        if (LV2::isa(response, engine.map_uri(LV2_PATCH_PREFIX "Ack"))) {
            LV2_Atom* idAtom = nullptr;
            lv2_atom_object_get(response, engine.map_uri(METHCLA_ENGINE_PREFIX "id"), &idAtom, nullptr);
            if (idAtom == nullptr)
                throw std::runtime_error("missing id from response");
            return NodeId(engine.parser().cast<int32_t>(idAtom));
        } else if (LV2::isa(response, engine.map_uri(LV2_PATCH_PREFIX "Error"))) {
            throw std::runtime_error("an error occured");
        } else {
            throw std::runtime_error("an unknown error occured");
        }
    }
};

#endif // METHCLA_ENGINE_HPP_INCLUDED
