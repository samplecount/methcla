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

#include <exception>
#include <future>
#include <iostream>
#include <list>
#include <memory>
#include <stdexcept>
#include <string>
#include <thread>
#include <unordered_map>
#include <vector>

#include <oscpp/client.hpp>
#include <oscpp/server.hpp>
#include <oscpp/print.hpp>

namespace Methcla
{
    inline static void dumpRequest(std::ostream& out, const OSCPP::Client::Packet& packet)
    {
#if 0
        out << "Request (send): " << packet << std::endl;
#endif
    }

#if 0
    inline static std::exception_ptr responseToException(const OSCPP::Server::Packet& packet)
    {
        if (packet.isMessage()) {
            OSCPP::Server::Message msg(packet);
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
#endif

    namespace detail
    {
        template <class D, typename T> class Id
        {
        public:
            explicit Id(T id)
                : m_id(id)
            { }
            Id(const D& other)
                : m_id(other.m_id)
            { }

            T id() const
            {
                return m_id;
            }

            bool operator==(const D& other) const
            {
                return m_id == other.m_id;
            }

            bool operator!=(const D& other) const
            {
                return m_id != other.m_id;
            }

        private:
            T m_id;
        };
    };

    class NodeId : public detail::Id<NodeId,int32_t>
    {
    public:
        NodeId(int32_t id)
            : Id<NodeId,int32_t>(id)
        { }
        NodeId()
            : NodeId(-1)
        { }
    };

    class GroupId : public NodeId
    {
    public:
        // Inheriting constructors not supported by clang 3.2
        // using NodeId::NodeId;
        GroupId(int32_t id)
            : NodeId(id)
        { }
        GroupId()
            : NodeId()
        { }
    };

    class SynthId : public NodeId
    {
    public:
        SynthId(int32_t id)
            : NodeId(id)
        { }
        SynthId()
            : NodeId()
        { }
    };

    class AudioBusId : public detail::Id<AudioBusId,int32_t>
    {
    public:
        AudioBusId(int32_t id)
            : Id<AudioBusId,int32_t>(id)
        { }
        AudioBusId()
            : AudioBusId(0)
        { }
    };

    enum BusMappingFlags
    {
        kBusMappingInternal = 0x0,
        kBusMappingExternal = 0x1
    };

    template <typename T> class ResourceIdAllocator
    {
    public:
        ResourceIdAllocator(T minValue, size_t n)
            : m_offset(minValue)
            , m_bits(n)
            , m_pos(0)
        { }

        T alloc()
        {
            for (size_t i=m_pos; i < m_bits.size(); i++) {
                if (!m_bits[i]) {
                    m_bits[i] = true;
                    m_pos = (i+1) == m_bits.size() ? 0 : i+1;
                    return T(m_offset + i);
                }
            }
            for (size_t i=0; i < m_pos; i++) {
                if (!m_bits[i]) {
                    m_bits[i] = true;
                    m_pos = i+1;
                    return T(m_offset + i);
                }
            }
            throw std::runtime_error("No free ids");
        }

        void free(T id)
        {
            T i = id - m_offset;
            if ((i >= 0) && (i < (T)m_bits.size())) {
                m_bits[i] = false;
            } else {
                throw std::runtime_error("Invalid id");
            }
        }

    private:
        T                 m_offset;
        std::vector<bool> m_bits;
        size_t            m_pos;
    };

    class PacketPool
    {
    public:
        PacketPool(const PacketPool&) = delete;
        PacketPool& operator=(const PacketPool&) = delete;

        PacketPool(size_t packetSize)
            : m_packetSize(packetSize)
        { }
        ~PacketPool()
        {
            std::lock_guard<std::mutex> lock(m_mutex);
            while (!m_freeList.empty()) {
                void* ptr = m_freeList.front();
                delete [] (char*)ptr;
                m_freeList.pop_front();
            }
        }

        size_t packetSize() const
        {
            return m_packetSize;
        }

        void* alloc()
        {
            std::lock_guard<std::mutex> lock(m_mutex);
            if (m_freeList.empty())
                return new char[m_packetSize];
            void* result = m_freeList.back();
            m_freeList.pop_back();
            return result;
        }

        void free(void* ptr)
        {
            std::lock_guard<std::mutex> lock(m_mutex);
            m_freeList.push_back(ptr);
        }

    private:
        size_t m_packetSize;
        // TODO: Use boost::lockfree::queue for free list
        std::list<void*> m_freeList;
        std::mutex m_mutex;
    };

    class Packet
    {
    public:
        Packet(PacketPool& pool)
            : m_pool(pool)
            , m_packet(pool.alloc(), pool.packetSize())
        { }
        ~Packet()
        {
            m_pool.free(m_packet.data());
        }

        const OSCPP::Client::Packet& packet() const
        {
            return m_packet;
        }

        OSCPP::Client::Packet& packet()
        {
            return m_packet;
        }

    private:
        PacketPool&         m_pool;
        OSCPP::Client::Packet m_packet;
    };

#if 0
    namespace detail
    {
        struct Result
        {
            Result()
                : m_cond(false)
            { }
            Result(const Result&) = delete;
            Result& operator=(const Result&) = delete;

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

    template <class T> class Result : detail::Result
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

    template <> class Result<void> : detail::Result
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

    template <typename T> bool checkResponse(const OSCPP::Server::Packet& response, Result<T>& result)
    {
        auto error = responseToException(response);
        if (error) {
            result.set(error);
            return false;
        }
        return true;
    }
#endif

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

        void put(OSCPP::Client::Packet& packet) const
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
        virtual void put(OSCPP::Client::Packet& packet) const = 0;

        inline static std::shared_ptr<Option> pluginLibrary(Methcla_LibraryFunction f);
    };

    class OptionPluginLibrary : public Option
    {
    public:
        OptionPluginLibrary(Methcla_LibraryFunction f)
            : m_function(f)
        { }

        virtual void put(OSCPP::Client::Packet& packet) const override
        {
            char buffer[sizeof(Methcla_LibraryFunction)];
            std::memcpy(buffer, &m_function, sizeof(Methcla_LibraryFunction));
            packet
                .openMessage("/engine/option/plugin-library", 1)
                .blob(OSCPP::Blob(buffer, sizeof(Methcla_LibraryFunction)))
                .closeMessage();
        }

    private:
        Methcla_LibraryFunction m_function;
    };

    std::shared_ptr<Option> Option::pluginLibrary(Methcla_LibraryFunction f)
    {
        return std::make_shared<OptionPluginLibrary>(f);
    }

    typedef std::vector<std::shared_ptr<Option>> Options;

    class Engine
    {
    public:
        Engine(const Options& options)
            : m_nodeIds(1, 1023)
            , m_requestId(kMethcla_Notification+1)
            , m_packets(8192)
        {
            OSCPP::Client::DynamicPacket bundle(8192);
            bundle.openBundle(1);
            for (auto option : options) {
                option->put(bundle);
            }
            bundle.closeBundle();
            const Methcla_OSCPacket packet = { .data = bundle.data(), .size = bundle.size() };
            check(methcla_engine_new(handlePacket, this, &packet, &m_engine));
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
            check(methcla_engine_start(m_engine));
        }

        void stop()
        {
            check(methcla_engine_stop(m_engine));
        }

        GroupId root() const
        {
            return GroupId(0);
        }

        GroupId group(GroupId parent)
        {
            const char address[] = "/group/new";
            const size_t numArgs = 3;

            const int32_t nodeId = m_nodeIds.alloc();

            Packet request(m_packets);
            request.packet()
                .openMessage(address, numArgs)
                .int32(nodeId)
                .int32(parent.id())
                .int32(0) // add action
                .closeMessage();

            dumpRequest(std::cerr, request.packet());

            send(request);

            return GroupId(nodeId);
        }

        SynthId synth(const char* synthDef, GroupId parent, const std::vector<float>& controls, const std::list<Value>& options=std::list<Value>())
        {
            const char address[] = "/synth/new";
            const size_t numTags = 4 + OSCPP::Tags::array(controls.size()) + OSCPP::Tags::array(options.size());
            // const size_t packetSize = OSCPP::Size::message(address, numTags)
            //                              + OSCPP::Size::string(256)
            //                              + OSCPP::Size::int32()
            //                              + 2 * OSCPP::Size::int32()
            //                              + controls.size() * OSCPP::Size::float32()
            //                              + 256; // margin for options. better: pool allocator with fixed size packets.

            const int32_t nodeId = m_nodeIds.alloc();
            // const Methcla_RequestId requestId = getRequestId();

            Packet request(m_packets);
            request.packet()
                .openMessage(address, numTags)
                .string(synthDef)
                .int32(nodeId)
                .int32(parent.id())
                .int32(0)
                .putArray(controls.begin(), controls.end());

            request.packet().openArray();
            for (const auto& x : options) {
                x.put(request.packet());
            }
            request.packet().closeArray();

            request.packet().closeMessage();

            dumpRequest(std::cerr, request.packet());

            send(request);

            // Result<SynthId> result;
            // 
            // withRequest(requestId, request, [&result](Methcla_RequestId requestId, const void* buffer, size_t size){
            //     OSCPP::Server::Packet response(buffer, size);
            //     if (checkResponse(response, result)) {
            //         auto args = ((OSCPP::Server::Message)response).args();
            //         int32_t requestId_ = args.int32();
            //         BOOST_ASSERT_MSG( requestId_ == requestId, "Request id mismatch");
            //         int32_t nodeId = args.int32();
            //         // std::cerr << "synth: " << requestId << " " << nodeId << std::endl;
            //         result.set(SynthId(nodeId));
            //     }
            // });
            // 
            // return result.get();

            return SynthId(nodeId);
        }

        void mapInput(const SynthId& synth, size_t index, AudioBusId bus, BusMappingFlags flags=kBusMappingInternal)
        {
            const char address[] = "/synth/map/input";
            const size_t numArgs = 4;

            // Methcla_RequestId requestId = getRequestId();

            Packet request(m_packets);
            request.packet()
                .openMessage(address, numArgs)
                    // .int32(requestId)
                    .int32(synth.id())
                    .int32(index)
                    .int32(bus.id())
                    .int32(flags)
                .closeMessage();

            dumpRequest(std::cerr, request.packet());
            // execRequest(requestId, request);
            send(request);
        }

        void mapOutput(const SynthId& synth, size_t index, AudioBusId bus, BusMappingFlags flags=kBusMappingInternal)
        {
            const char address[] = "/synth/map/output";
            const size_t numArgs = 4;
            // const size_t packetSize = OSCPP::Size::message(address, numArgs)
            //                             + numArgs * OSCPP::Size::int32();

            // Methcla_RequestId requestId = getRequestId();

            Packet request(m_packets);
            request.packet()
                .openMessage(address, numArgs)
                    // .int32(requestId)
                    .int32(synth.id())
                    .int32(index)
                    .int32(bus.id())
                    .int32(flags)
                .closeMessage();

            dumpRequest(std::cerr, request.packet());
            // execRequest(requestId, request);
            send(request);
        }

        void set(const NodeId& node, size_t index, double value)
        {
            const char address[] = "/node/set";
            const size_t numArgs = 3;
            // const size_t packetSize = OSCPP::Size::message(address, numArgs)
            //                             + 2 * OSCPP::Size::int32()
            //                             + OSCPP::Size::float32();

            // Methcla_RequestId requestId = getRequestId();

            Packet request(m_packets);
            request.packet()
                .openMessage(address, numArgs)
                    // .int32(requestId)
                    .int32(node.id())
                    .int32(index)
                    .float32(value)
                .closeMessage();

            dumpRequest(std::cerr, request.packet());
            // execRequest(requestId, request);
            send(request);
        }

        void freeNode(const NodeId& node)
        {
            const char address[] = "/node/free";
            const size_t numArgs = 1;
            // const size_t packetSize = OSCPP::Size::message(address, numArgs) + numArgs * OSCPP::Size::int32();
            // const Methcla_RequestId requestId = getRequestId();
            Packet request(m_packets);
            request.packet()
                .openMessage(address, numArgs)
                    // .int32(requestId)
                    .int32(node.id())
                .closeMessage();
            // execRequest(requestId, request);
            dumpRequest(std::cerr, request.packet());
            send(request);
            m_nodeIds.free(node.id());
        }

    private:
        static void check(Methcla_Error err)
        {
            if (err != kMethcla_NoError) {
                const char* msg = methcla_error_message(err);
                if (err == kMethcla_ArgumentError) {
                    throw std::invalid_argument(msg);
                } else if (err == kMethcla_LogicError) {
                    throw std::logic_error(msg);
                } else if (err == kMethcla_MemoryError) {
                    throw std::bad_alloc();
                } else {
                    throw std::runtime_error(msg);
                }
            }
        }

        static void handlePacket(void* data, Methcla_RequestId requestId, const void* packet, size_t size)
        {
            if (requestId == kMethcla_Notification)
                static_cast<Engine*>(data)->handleNotification(packet, size);
            else
                static_cast<Engine*>(data)->handleReply(requestId, packet, size);
        }

        void handleNotification(const void* packet, size_t size)
        {
        }

        void handleReply(Methcla_RequestId requestId, const void* packet, size_t size)
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
            check(methcla_engine_send(m_engine, packet, size));
        }

        void send(const OSCPP::Client::Packet& packet)
        {
            send(packet.data(), packet.size());
        }

        void send(const Packet& packet)
        {
            send(packet.packet());
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
            if (m_callbacks.find(requestId) != m_callbacks.end()) {
                throw std::logic_error("Duplicate request id");
            }
            m_callbacks[requestId] = callback;
        }

#if 0
        void withRequest(Methcla_RequestId requestId, const OSCPP::Client::Packet& request, std::function<void (Methcla_RequestId, const void*, size_t)> callback)
        {
            registerResponse(requestId, callback);
            send(request);
        }

        void execRequest(Methcla_RequestId requestId, const OSCPP::Client::Packet& request)
        {
            Result<void> result;
            withRequest(requestId, request, [&result](Methcla_RequestId, const void* buffer, size_t size){
                if (checkResponse(OSCPP::Server::Packet(buffer, size), result)) {
                    result.set();
                }
            });
            result.get();
        }
#endif

    private:
        Methcla_Engine*     m_engine;
        ResourceIdAllocator<int32_t> m_nodeIds;
        Methcla_RequestId   m_requestId;
        std::mutex          m_requestIdMutex;
        std::unordered_map<Methcla_RequestId,std::function<void (Methcla_RequestId, const void*, size_t)>> m_callbacks;
        std::mutex          m_callbacksMutex;
        PacketPool          m_packets;
    };
};

#endif // METHCLA_ENGINE_HPP_INCLUDED
