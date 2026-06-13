// Copyright (C) 2013 Samplecount S.L.
// Copyright (C) 2026 Methcla contributors
//
// SPDX-License-Identifier: Apache-2.0

#pragma once

#include <methcla/log.hpp>
#include <methcla/plugin.h>

#include <cstring>
#include <functional>

#include <oscpp/server.hpp>

// NOTE: This API is unstable and subject to change!

namespace Methcla { namespace Plugin {

    template <class Synth> class World
    {
        Methcla_World* m_context;

    public:
        World(Methcla_World* context)
        : m_context(context)
        {}

        double sampleRate() const
        {
            return methcla_world_samplerate(m_context);
        }

        size_t blockSize() const
        {
            return methcla_world_block_size(m_context);
        }

        Methcla_Time currentTime() const
        {
            return methcla_world_current_time(m_context);
        }

        void* alloc(size_t size) const
        {
            return methcla_world_alloc(m_context, size);
        }

        void* allocAligned(size_t alignment, size_t size) const
        {
            return methcla_world_alloc_aligned(m_context, alignment, size);
        }

        void free(void* ptr)
        {
            methcla_world_free(m_context, ptr);
        }

        void performCommand(Methcla_HostPerformFunction perform, void* data)
        {
            methcla_world_perform_command(m_context, perform, data);
        }

        LogStream log(Methcla_LogLevel logLevel = kMethcla_LogInfo) const
        {
            using namespace std::placeholders;
            return LogStream(std::bind(m_context->log_line, m_context, _1, _2),
                             logLevel);
        }

        void synthDone(Synth* synth) const
        {
            methcla_world_synth_done(m_context, synth);
        }
    };

    class HostContext
    {
        Methcla_Host* m_context;

    public:
        HostContext(Methcla_Host* context)
        : m_context(context)
        {}

        LogStream log(Methcla_LogLevel logLevel = kMethcla_LogInfo)
        {
            using namespace std::placeholders;
            return LogStream(std::bind(m_context->log_line, m_context, _1, _2),
                             logLevel);
        }
    };

    class NoPorts
    {
    public:
        enum Port
        {
        };

        static size_t numPorts()
        {
            return 0;
        }

        static Methcla_PortDescriptor descriptor(Port)
        {
            Methcla_PortDescriptor result;
            std::memset(&result, 0, sizeof(result));
            return result;
        }
    };

    class PortDescriptor
    {
    public:
        static Methcla_PortDescriptor
        make(Methcla_PortDirection direction, Methcla_PortType type,
             Methcla_PortFlags flags = kMethcla_PortFlags)
        {
            Methcla_PortDescriptor pd;
            pd.direction = direction;
            pd.type = type;
            pd.flags = flags;
            return pd;
        }

        static Methcla_PortDescriptor
        audioInput(Methcla_PortFlags flags = kMethcla_PortFlags)
        {
            return make(kMethcla_Input, kMethcla_AudioPort, flags);
        }

        static Methcla_PortDescriptor
        audioOutput(Methcla_PortFlags flags = kMethcla_PortFlags)
        {
            return make(kMethcla_Output, kMethcla_AudioPort, flags);
        }

        static Methcla_PortDescriptor
        controlInput(Methcla_PortFlags flags = kMethcla_PortFlags)
        {
            return make(kMethcla_Input, kMethcla_ControlPort, flags);
        }

        static Methcla_PortDescriptor
        controlOutput(Methcla_PortFlags flags = kMethcla_PortFlags)
        {
            return make(kMethcla_Output, kMethcla_ControlPort, flags);
        }
    };

    struct NoOptions
    {
        NoOptions(OSCPP::Server::ArgStream)
        {}
    };

    template <class Options, class PortDescriptor> class StaticSynthOptions
    {
    public:
        typedef Options Type;

        static void configure(const void* tag_buffer, size_t tag_buffer_size,
                              const void* arg_buffer, size_t arg_buffer_size,
                              Methcla_SynthOptions* options)
        {
            OSCPP::Server::ArgStream args(
                OSCPP::ReadStream(tag_buffer, tag_buffer_size),
                OSCPP::ReadStream(arg_buffer, arg_buffer_size));
            new (options) Type(args);
        }

        static bool port_descriptor(const Methcla_SynthOptions*,
                                    Methcla_PortCount       index,
                                    Methcla_PortDescriptor* port)
        {
            if (index < PortDescriptor::numPorts())
            {
                *port = PortDescriptor::descriptor(
                    static_cast<typename PortDescriptor::Port>(index));
                return true;
            }
            return false;
        }
    };

    namespace detail {
        template <class Synth, bool Condition> class IfSynthDefHasActivate
        {
        public:
            static inline void exec(Methcla_World*, Synth*)
            {}
        };

        template <class Synth> class IfSynthDefHasActivate<Synth, true>
        {
        public:
            static inline void exec(Methcla_World* context, Synth* synth)
            {
                synth->activate(World<Synth>(context));
            }
        };

        template <class Synth, bool Condition> class IfSynthDefHasCleanup
        {
        public:
            static inline void exec(Methcla_World*, Synth*)
            {}
        };

        template <class Synth> class IfSynthDefHasCleanup<Synth, true>
        {
        public:
            static inline void exec(Methcla_World* context, Synth* synth)
            {
                synth->cleanup(World<Synth>(context));
            }
        };
    } // namespace detail

    enum SynthDefFlags
    {
        kSynthDefDefaultFlags = 0x00,
        kSynthDefHasActivate = 0x01,
        kSynthDefHasCleanup = 0x02
    };

    template <class Synth, class Options, class PortDescriptor,
              SynthDefFlags Flags = kSynthDefDefaultFlags>
    class SynthDef
    {
        static void construct(Methcla_World*              context,
                              const Methcla_SynthDef*     synthDef,
                              const Methcla_SynthOptions* options,
                              Methcla_Synth*              synth)
        {
            assert(context != nullptr);
            assert(options != nullptr);
            new (synth)
                Synth(World<Synth>(context), synthDef,
                      *static_cast<const typename Options::Type*>(options));
        }

        static void connect(Methcla_Synth* synth, Methcla_PortCount port,
                            void* data)
        {
            static_cast<Synth*>(synth)->connect(
                static_cast<typename PortDescriptor::Port>(port), data);
        }

        static void activate(Methcla_World* context, Methcla_Synth* synth)
        {
            detail::IfSynthDefHasActivate<
                Synth, (Flags & kSynthDefHasActivate) ==
                           kSynthDefHasActivate>::exec(context,
                                                       static_cast<Synth*>(
                                                           synth));
        }

        static void process(Methcla_World* context, Methcla_Synth* synth,
                            size_t numFrames)
        {
            static_cast<Synth*>(synth)->process(World<Synth>(context),
                                                numFrames);
        }

        static void destroy(Methcla_World* context, Methcla_Synth* synth)
        {
            // Call cleanup method
            detail::IfSynthDefHasActivate<
                Synth, (Flags & kSynthDefHasCleanup) ==
                           kSynthDefHasCleanup>::exec(context,
                                                      static_cast<Synth*>(
                                                          synth));
            // Call destructor
            static_cast<Synth*>(synth)->~Synth();
        }

    public:
        void operator()(Methcla_Host* host, const char* uri)
        {
            static const Methcla_SynthDef kSynthDef = {
                uri,
                sizeof(Synth),
                sizeof(typename Options::Type),
                Options::configure,
                Options::port_descriptor,
                construct,
                connect,
                activate,
                process,
                destroy};
            methcla_host_register_synthdef(host, &kSynthDef);
        }
    };

    template <class Synth, class Options, class Ports,
              SynthDefFlags Flags = kSynthDefDefaultFlags>
    using StaticSynthDef =
        SynthDef<Synth, StaticSynthOptions<Options, Ports>, Ports, Flags>;

    // RAII handle for an acquired resource. `Resource` is a wrapper type that
    // exposes a static `uri()` and a nested `c_type` typedef naming the C ABI
    // layout (see e.g. the AudioBuffer wrapper). Acquire happens in the
    // constructor; release happens in the destructor.
    //
    // The constructed handle is empty (`operator bool() == false`) if the
    // engine refused the acquire — typically because the id is not Live or
    // its URI does not match the wrapper's. Callers must check before use.
    template <class Resource> class ResourceRef
    {
    public:
        using c_type = typename Resource::c_type;

        ResourceRef(Methcla_World* world, Methcla_ResourceId id)
        : m_world(world)
        , m_id(id)
        , m_data(static_cast<c_type*>(
              methcla_world_resource_acquire(world, id, Resource::uri())))
        {}

        ~ResourceRef()
        {
            if (m_data)
                methcla_world_resource_release(m_world, m_id);
        }

        ResourceRef(const ResourceRef&) = delete;
        ResourceRef& operator=(const ResourceRef&) = delete;

        ResourceRef(ResourceRef&& other) noexcept
        : m_world(other.m_world)
        , m_id(other.m_id)
        , m_data(other.m_data)
        {
            other.m_data = nullptr;
        }

        ResourceRef& operator=(ResourceRef&& other) noexcept
        {
            if (this != &other)
            {
                if (m_data)
                    methcla_world_resource_release(m_world, m_id);
                m_world = other.m_world;
                m_id = other.m_id;
                m_data = other.m_data;
                other.m_data = nullptr;
            }
            return *this;
        }

        Methcla_ResourceId id() const
        {
            return m_id;
        }
        c_type* data() const
        {
            return m_data;
        }
        explicit operator bool() const
        {
            return m_data != nullptr;
        }
        Resource operator*() const
        {
            return Resource(m_data);
        }

    private:
        Methcla_World*     m_world;
        Methcla_ResourceId m_id;
        c_type*            m_data;
    };

    // Plugin-author helper that bridges the C ABI for a resource type to a
    // C++ class. The Resource class must be constructible from
    // (HostContext, const typename Options::Type&). On registration the
    // resource def is created with static storage duration so it remains
    // valid for the engine's lifetime.
    template <class Resource, class Options = NoOptions> class ResourceDef
    {
        static Methcla_ErrorCode configure(const void* tag_buffer,
                                           size_t      tag_size,
                                           const void* arg_buffer,
                                           size_t arg_size, void* options)
        {
            try
            {
                OSCPP::Server::ArgStream args(
                    OSCPP::ReadStream(tag_buffer, tag_size),
                    OSCPP::ReadStream(arg_buffer, arg_size));
                new (options) typename Options::Type(args);
                return kMethcla_NoError;
            }
            catch (...)
            {
                return kMethcla_ArgumentError;
            }
        }

        static Methcla_Error construct(Methcla_Host* host,
                                       const Methcla_ResourceDef*,
                                       const void* options, void* instance)
        {
            try
            {
                new (instance) Resource(
                    HostContext(host),
                    *static_cast<const typename Options::Type*>(options));
                return methcla_no_error();
            }
            catch (const std::exception& e)
            {
                return methcla_error_new_with_message(kMethcla_ArgumentError,
                                                      e.what());
            }
            catch (...)
            {
                return methcla_error_new(kMethcla_ArgumentError);
            }
        }

        static void destroy(Methcla_Host*, void* instance)
        {
            static_cast<Resource*>(instance)->~Resource();
        }

    public:
        void operator()(Methcla_Host* host, const char* uri,
                        Methcla_ResourceMutability mutability)
        {
            static const Methcla_ResourceDef kDef = {
                uri,        sizeof(Resource), sizeof(typename Options::Type),
                mutability, configure,        construct,
                destroy};
            methcla_host_register_resource_def(host, &kDef);
        }
    };
}} // namespace Methcla::Plugin
