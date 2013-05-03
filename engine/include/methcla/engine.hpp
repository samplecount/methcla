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

    inline static std::exception_ptr responseToException(const OSC::Server::Packet& packet)
    {
        if (packet.isMessage()) {
            OSC::Server::Message msg(packet);
            if (msg == "/error") {
                auto args(msg.args());
                args.drop(); // request id
                const char* error = args.string();
                return std::make_exception_ptr(std::runtime_error(error));
            } else {
                return std::exception_ptr();
            }
        } else {
            return std::make_exception_ptr(std::invalid_argument("Response is not a message"));
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

    namespace impl
    {
        struct Result : boost::noncopyable
        {
            Result()
                : m_cond(false)
            { }

            inline void notify()
            {
                m_cond = true;
                m_cond_var.notify_one();
            }

            inline void wait()
            {
                std::unique_lock<std::mutex> lock(m_mutex);
                while (!m_cond) {
                    m_cond_var.wait(lock);
                }
                if (m_exc) {
                    std::rethrow_exception(m_exc);
                }
            }

            void set_exception(std::exception_ptr exc)
            {
                BOOST_ASSERT(m_cond);
                std::lock_guard<std::mutex> lock(m_mutex);
                m_exc = exc;
                notify();
            }

            std::mutex m_mutex;
            std::condition_variable m_cond_var;
            bool m_cond;
            std::exception_ptr m_exc;
        };
    };

    template <class T> class Result : impl::Result
    {
    public:
        void set(std::exception_ptr exc)
        {
            set_exception(exc);
        }

        void set(const T& value)
        {
            BOOST_ASSERT(!m_cond);
            std::lock_guard<std::mutex> lock(m_mutex);
            m_value = value;
            notify();
        }

        const T& get()
        {
            wait();
            return m_value;
        }

    private:
        T m_value;
    };

    template <> class Result<void> : impl::Result
    {
    public:
        void set(std::exception_ptr exc)
        {
            set_exception(exc);
        }

        void set()
        {
            BOOST_ASSERT(!m_cond);
            std::lock_guard<std::mutex> lock(m_mutex);
            notify();
        }

        void get()
        {
            wait();
        }
    };

    template <typename T> bool checkResponse(const OSC::Server::Packet& response, Result<T>& result)
    {
        auto error = responseToException(response);
        if (error) {
            result.set(error);
            return false;
        }
        return true;
    }

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

        SynthId synth(const char* synthDef)
        {
            const char address[] = "/s_new";
            const size_t numArgs = 4;
            const size_t packetSize = OSC::Size::message(address, numArgs)
                                         + OSC::Size::int32()
                                         + OSC::Size::string(256)
                                         + 2 * OSC::Size::int32();

            const Methcla_RequestId requestId = getRequestId();

            OSC::Client::StaticPacket<packetSize> request;
            request
                .openMessage(address, numArgs)
                    .int32(requestId)
                    .string(synthDef)
                    .int32(0)
                    .int32(0)
                .closeMessage();

            dumpRequest(std::cerr, OSC::Server::Packet(request.data(), request.size()));

            Result<SynthId> result;

            withRequest(requestId, request, [&result](Methcla_RequestId requestId, const void* buffer, size_t size){
                OSC::Server::Packet response(buffer, size);
                if (checkResponse(response, result)) {
                    auto args = ((OSC::Server::Message)response).args();
                    int32_t requestId_ = args.int32();
                    BOOST_ASSERT_MSG( requestId_ == requestId, "Request id mismatch");
                    int32_t nodeId = args.int32();
                    std::cerr << "synth: " << requestId << " " << nodeId << std::endl;
                    result.set(SynthId(nodeId));
                }
            });

            return result.get();
        }

        void mapOutput(const SynthId& synth, size_t index, AudioBusId bus)
        {
            const char address[] = "/synth/map/output";
            const size_t numArgs = 4;
            const size_t packetSize = OSC::Size::message(address, numArgs)
                                        + numArgs * OSC::Size::int32();

            Methcla_RequestId requestId = getRequestId();

            OSC::Client::StaticPacket<packetSize> request;
            request
                .openMessage(address, numArgs)
                    .int32(requestId)
                    .int32(synth)
                    .int32(index)
                    .int32(bus)
                .closeMessage();

            dumpRequest(std::cerr, OSC::Server::Packet(request.data(), request.size()));

            Result<void> result;

            withRequest(requestId, request, [&result](Methcla_RequestId requestId, const void* buffer, size_t size){
                if (checkResponse(OSC::Server::Packet(buffer, size), result)) {
                    result.set();
                }
            });

            result.get();
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
                try {
                    it->second(requestId, packet, size);
                    m_callbacks.erase(it);
                } catch (...) {
                    m_callbacks.erase(it);
                    throw;
                }
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

        void registerResponse(Methcla_RequestId requestId, std::function<void (Methcla_RequestId, const void*, size_t)> callback)
        {
            std::lock_guard<std::mutex> lock(m_callbacksMutex);
            BOOST_ASSERT_MSG( m_callbacks.find(requestId) == m_callbacks.end(), "Duplicate request id" );
            m_callbacks[requestId] = callback;
        }

        void withRequest(Methcla_RequestId requestId, const OSC::Client::Packet& request, std::function<void (Methcla_RequestId, const void*, size_t)> callback)
        {
            registerResponse(requestId, callback);
            send(request);
        }

    private:
        Methcla_Engine*     m_engine;
        Methcla_RequestId   m_requestId;
        std::mutex          m_requestIdMutex;
        std::unordered_map<Methcla_RequestId,std::function<void (Methcla_RequestId, const void*, size_t)>> m_callbacks;
        std::mutex          m_callbacksMutex;
    };
};

#endif // METHCLA_ENGINE_HPP_INCLUDED
