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
#include <methcla/lv2/atom.hpp>

#include <future>
#include <iostream>
#include <memory>
#include <stdexcept>
#include <string>
#include <thread>
#include <unordered_map>

#include <boost/optional.hpp>
#include <boost/serialization/strong_typedef.hpp>
#include <boost/variant.hpp>
#include <boost/variant/apply_visitor.hpp>

#include <oscpp/client.hpp>
#include <oscpp/server.hpp>

namespace Methcla
{
    inline static void dumpMessage(std::ostream& out, const OSC::Server::Message& msg)
    {
        out << msg.address() << ' ';
        OSC::Server::ArgStream args(msg.args());
        while (!args.atEnd()) {
            const char t = args.tag();
            out << t << ':';
            switch (t) {
                case 'i':
                    out << args.int32();
                    break;
                case 'f':
                    out << args.float32();
                    break;
                case 's':
                    out << args.string();
                    break;
                case 'b':
                    out << args.blob().size;
                    break;
                default:
                    out << '?';
                    break;
            }
            out << ' ';
        }
    }

    inline static void dumpRequest(std::ostream& out, const OSC::Server::Packet& packet)
    {
        out << "Request (send): ";
        dumpMessage(out, packet);
        out << std::endl;
    }

    inline static boost::optional<std::exception_ptr> responseToException(const OSC::Server::Packet& packet)
    {
        if (packet.isMessage()) {
            OSC::Server::Message msg(packet);
            if (msg == "/error") {
                auto args(msg.args());
                args.drop(); // request id
                const char* error = args.string();
                return boost::make_optional(std::make_exception_ptr(std::runtime_error(error)));
            } else {
                return boost::none;
            }
        } else {
            return boost::make_optional(std::make_exception_ptr(std::invalid_argument("Response is not a message")));
        }
    }

    inline static void checkResponse(const OSC::Server::Packet& packet)
    {
        if (packet.isMessage()) {
            OSC::Server::Message msg(packet);
            if (msg == "/error") {
                auto args(msg.args());
                args.drop(); // request id
                const char* error = args.string();
                throw std::runtime_error(error);
            }
        } else {
            throw std::invalid_argument("Response is not a message");
        }
    }

    BOOST_STRONG_TYPEDEF(int32_t, SynthId);
    BOOST_STRONG_TYPEDEF(int32_t, AudioBusId);

    template <class T> class ExceptionVisitor : public boost::static_visitor<T>
    {
    public:
        T operator()(const std::exception_ptr& e) const
        {
            std::rethrow_exception(e);
        }

        T operator()(const T& x) const
        {
            return x;
        }
    };

    class Engine
    {
    public:
        Engine(const Methcla_Option* options)
            : m_engine(methcla_engine_new(handlePacket, this, options))
            , m_requestId(kMethcla_Notification+1)
        {
            check(m_engine);
        }
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
            check(m_engine);
        }

        void stop()
        {
            methcla_engine_stop(m_engine);
            check(m_engine);
        }

        inline SynthId synth(const char* synthDef)
        {
            Methcla_RequestId requestId = getRequestId();

            OSC::Client::StaticPacket<8192> request;
            request
                .openMessage("/s_new", 4)
                    .int32(requestId)
                    .string(synthDef)
                    .int32(0)
                    .int32(0)
                .closeMessage();

            dumpRequest(std::cerr, OSC::Server::Packet(request.data(), request.size()));

            std::promise<boost::variant<std::exception_ptr,SynthId>> promise;

            registerResponse(requestId, [&promise,&requestId](const void* buffer, size_t size){
                OSC::Server::Packet response(buffer, size);
                auto error = responseToException(response);
                if (error) {
                    promise.set_value(*error);
                } else {
                    auto args = ((OSC::Server::Message)response).args();
                    int32_t requestId_ = args.int32();
                    assert(requestId_ == requestId);
                    int32_t nodeId = args.int32();
                    promise.set_value(SynthId(nodeId));
                }
            });

            send(request);

            auto response = promise.get_future().get();
            return boost::apply_visitor(ExceptionVisitor<SynthId>(), response);
        }

        inline void mapOutput(const SynthId& synth, size_t index, AudioBusId bus)
        {
            Methcla_RequestId requestId = getRequestId();

            OSC::Client::StaticPacket<8192> request;
            request
                .openMessage("/synth/map/output", 4)
                    .int32(requestId)
                    .int32(synth)
                    .int32(index)
                    .int32(bus)
                .closeMessage();

            dumpRequest(std::cerr, OSC::Server::Packet(request.data(), request.size()));

            std::promise<boost::optional<std::exception_ptr>> promise;
            registerResponse(requestId, [&promise](const void* buffer, size_t size){
                promise.set_value(responseToException(OSC::Server::Packet(buffer, size)));
            });

            send(request);

            auto response = promise.get_future().get();
            if (response)
                std::rethrow_exception(*response);
        }

    private:
        static void check(const Methcla_Engine* engine)
        {
            Methcla_Error err = methcla_engine_error(engine);
            if (err != kMethcla_NoError) {
                const char* msg = methcla_engine_error_message(engine);
                if (err == kMethcla_InvalidArgument)
                    throw std::invalid_argument(msg);
                else if (err == kMethcla_BadAlloc)
                    throw std::bad_alloc();
                else
                    throw std::runtime_error(msg);
            }
        }

        static void handlePacket(void* data, Methcla_RequestId requestId, const void* packet, size_t size)
        {
            static_cast<Engine*>(data)->handlePacket(requestId, packet, size);
        }

        void handlePacket(Methcla_RequestId requestId, const void* packet, size_t size)
        {
            std::lock_guard<std::mutex> lock(m_callbacksMutex);
            // look up request id and invoke callback
            auto it = m_callbacks.find(requestId);
            if (it != m_callbacks.end()) {
                it->second(packet, size);
                m_callbacks.erase(it);
            }
        }

        void send(const void* packet, size_t size)
        {
            methcla_engine_send(m_engine, packet, size);
        }

        void send(const OSC::Client::Packet& packet)
        {
            send(packet.data(), packet.size());
        }

        Methcla_RequestId getRequestId()
        {
            std::lock_guard<std::mutex> lock(m_requestIdMutex);
            Methcla_RequestId result = m_requestId;
            if (result == kMethcla_Notification) {
                result++;
            }
            m_requestId = result + 1;
            return result;
        }

        void registerResponse(Methcla_RequestId requestId, std::function<void (const void*, size_t)> callback)
        {
            std::lock_guard<std::mutex> lock(m_callbacksMutex);
            m_callbacks[requestId] = callback;
        }


    private:
        Methcla_Engine*     m_engine;
        Methcla_RequestId   m_requestId;
        std::unordered_map<Methcla_RequestId,std::function<void (const void*, size_t)>> m_callbacks;
        std::mutex          m_requestIdMutex;
        std::mutex          m_callbacksMutex;
    };
};

#endif // METHCLA_ENGINE_HPP_INCLUDED
