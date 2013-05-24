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
#include <methcla/plugin.h>

#include <future>
#include <iostream>
#include <list>
#include <memory>
#include <stdexcept>
#include <string>
#include <thread>
#include <unordered_map>
#include <vector>

#include <boost/optional.hpp>
#include <boost/serialization/strong_typedef.hpp>
#include <boost/utility.hpp>

#include <oscpp/client.hpp>
#include <oscpp/server.hpp>
#include <oscpp/print.hpp>

namespace Methcla
{
    inline static void dumpRequest(std::ostream& out, const OSC::Server::Packet& packet)
    {
#if DEBUG
        out << "Request (send): " << packet << std::endl;
#endif
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
                BOOST_ASSERT(!m_cond);
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

    class Value
    {
    public:
        enum Type
        {
            kInt,
            kFloat,
            kString
        };

        explicit Value(int x)
            : m_type(kInt)
            , m_int(x)
        { }
        explicit Value(float x)
            : m_type(kFloat)
            , m_float(x)
        { }
        Value(const std::string& x)
            : m_type(kString)
            , m_string(x)
        { }

        explicit Value(bool x)
            : m_type(kInt)
            , m_int(x)
        { }

        void put(OSC::Client::Packet& packet) const
        {
            switch (m_type) {
                case kInt:
                    packet.int32(m_int);
                    break;
                case kFloat:
                    packet.float32(m_float);
                    break;
                case kString:
                    packet.string(m_string.c_str());
                    break;
            }
        }

    private:
        Type m_type;
        int m_int;
        float m_float;
        std::string m_string;
    };

    class Option
    {
    public:
        virtual ~Option() { }
        virtual void put(OSC::Client::Packet& packet) const = 0;
    };

    class OptionPluginLibrary : public Option
    {
    public:
        OptionPluginLibrary(Methcla_LibraryFunction f)
            : m_function(f)
        { }

        virtual void put(OSC::Client::Packet& packet) const override
        {
            char buffer[sizeof(Methcla_LibraryFunction)];
            std::memcpy(buffer, &m_function, sizeof(Methcla_LibraryFunction));
            OSC::Blob b = {
                .data = buffer,
                .size = sizeof(Methcla_LibraryFunction)
            };
            packet
                .openMessage("/engine/option/plugin-library", 1)
                .blob(b)
                .closeMessage();
        }

    private:
        Methcla_LibraryFunction m_function;
    };

    inline std::shared_ptr<Option> optionPluginLibrary(Methcla_LibraryFunction f)
    {
        return std::make_shared<OptionPluginLibrary>(f);
    }

    typedef std::vector<std::shared_ptr<Option>> Options;

    class Engine
    {
    public:
        Engine(const Options& options)
            : m_requestId(kMethcla_Notification+1)
        {
            OSC::Client::DynamicPacket bundle(8192);
            bundle.openBundle(1);
            for (auto option : options) {
                option->put(bundle);
            }
            bundle.closeBundle();
            const Methcla_OSCPacket packet = { .data = bundle.data(), .size = bundle.size() };
            check(nullptr, methcla_engine_new(handlePacket, this, &packet, &m_engine));
        }
        ~Engine()
        {
            methcla_engine_free(m_engine);
        }

        operator const Methcla_Engine* () const
        {
            return m_engine;
        }

        operator Methcla_Engine* ()
        {
            return m_engine;
        }

        void start()
        {
            check(m_engine, methcla_engine_start(m_engine));
        }

        void stop()
        {
            check(m_engine, methcla_engine_stop(m_engine));
        }

        SynthId synth(const char* synthDef, const std::vector<float>& controls, const std::list<Value>& options=std::list<Value>())
        {
            const char address[] = "/s_new";
            const size_t numArgs = 4 + OSC::Size::array(controls.size()) + OSC::Size::array(options.size());
            const size_t packetSize = OSC::Size::message(address, numArgs)
                                         + OSC::Size::int32()
                                         + OSC::Size::string(256)
                                         + 2 * OSC::Size::int32()
                                         + controls.size() * OSC::Size::float32()
                                         + 256; // margin for options. better: pool allocator with fixed size packets.

            const Methcla_RequestId requestId = getRequestId();

            OSC::Client::DynamicPacket request(packetSize);
            request
                .openMessage(address, numArgs)
                .int32(requestId)
                .string(synthDef)
                .int32(0)
                .int32(0)
                .putArray(controls.begin(), controls.end());

            request.openArray();
            for (const auto& x : options) {
                x.put(request);
            }
            request.closeArray();

            request.closeMessage();

            dumpRequest(std::cerr, OSC::Server::Packet(request.data(), request.size()));

            Result<SynthId> result;

            withRequest(requestId, request, [&result](Methcla_RequestId requestId, const void* buffer, size_t size){
                OSC::Server::Packet response(buffer, size);
                if (checkResponse(response, result)) {
                    auto args = ((OSC::Server::Message)response).args();
                    int32_t requestId_ = args.int32();
                    BOOST_ASSERT_MSG( requestId_ == requestId, "Request id mismatch");
                    int32_t nodeId = args.int32();
                    // std::cerr << "synth: " << requestId << " " << nodeId << std::endl;
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
            execRequest(requestId, request);
        }

        void set(const SynthId& synth, size_t index, double value)
        {
            const char address[] = "/n_set";
            const size_t numArgs = 4;
            const size_t packetSize = OSC::Size::message(address, numArgs)
                                        + 3 * OSC::Size::int32()
                                        + OSC::Size::float32();

            Methcla_RequestId requestId = getRequestId();

            OSC::Client::StaticPacket<packetSize> request;
            request
                .openMessage(address, numArgs)
                    .int32(requestId)
                    .int32(synth)
                    .int32(index)
                    .float32(value)
                .closeMessage();

            dumpRequest(std::cerr, OSC::Server::Packet(request.data(), request.size()));
            execRequest(requestId, request);
        }

        void freeNode(const SynthId& synth)
        {
            const char address[] = "/n_free";
            const size_t numArgs = 2;
            const size_t packetSize = OSC::Size::message(address, numArgs) + numArgs * OSC::Size::int32();
            const Methcla_RequestId requestId = getRequestId();
            OSC::Client::StaticPacket<packetSize> request;
            request
                .openMessage(address, numArgs)
                    .int32(requestId)
                    .int32(synth)
                .closeMessage();
            execRequest(requestId, request);
        }

    private:
        static void check(const Methcla_Engine* engine, Methcla_Error err)
        {
            if (err != kMethcla_NoError) {
                const char* msg = methcla_engine_error_message(engine, err);
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

        void execRequest(Methcla_RequestId requestId, const OSC::Client::Packet& request)
        {
            Result<void> result;
            withRequest(requestId, request, [&result](Methcla_RequestId, const void* buffer, size_t size){
                if (checkResponse(OSC::Server::Packet(buffer, size), result)) {
                    result.set();
                }
            });
            result.get();
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
