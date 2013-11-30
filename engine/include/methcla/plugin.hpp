// Copyright 2013 Samplecount S.L.
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

#ifndef METHCLA_PLUGIN_HPP_INCLUDED
#define METHCLA_PLUGIN_HPP_INCLUDED

#include <methcla/plugin.h>
#include <oscpp/server.hpp>

#include <cstring>

// NOTE: This API is unstable and subject to change!

namespace Methcla { namespace Plugin {

    template <class Synth> class World
    {
        const Methcla_World* m_world;

    public:
        World(const Methcla_World* world)
            : m_world(world)
        { }

        double sampleRate() const
        {
            return methcla_world_samplerate(m_world);
        }

        size_t blockSize() const
        {
            return methcla_world_block_size(m_world);
        }

        void* alloc(size_t size) const
        {
            return methcla_world_alloc(m_world, size);
        }

        void* allocAligned(size_t alignment, size_t size) const
        {
            return methcla_world_alloc_aligned(m_world, alignment, size);
        }

        void free(void* ptr)
        {
            methcla_world_free(m_world, ptr);
        }

        void performCommand(Methcla_HostPerformFunction perform, void* data)
        {
            methcla_world_perform_command(m_world, perform, data);
        }

        void synthRetain(Synth* synth) const
        {
            methcla_world_synth_retain(m_world, synth);
        }

        void synthRelease(Synth* synth) const
        {
            methcla_world_synth_release(m_world, synth);
        }

        void synthDone(Synth* synth) const
        {
            methcla_world_synth_done(m_world, synth);
        }
    };

    class NoPorts
    {
    public:
        enum Port { };

        static size_t numPorts() { return 0; }

        static Methcla_PortDescriptor descriptor(Port)
        {
            Methcla_PortDescriptor result;
            std::memset(&result, 0, sizeof(result));
            return result;
        }
    };

    template <class Options, class PortDescriptor> class StaticSynthOptions
    {
    public:
        typedef Options Type;

        static void
        configure( const void* tag_buffer
                 , size_t tag_buffer_size
                 , const void* arg_buffer
                 , size_t arg_buffer_size
                 , Methcla_SynthOptions* options )
        {
            OSCPP::Server::ArgStream args(
                OSCPP::ReadStream(tag_buffer, tag_buffer_size),
                OSCPP::ReadStream(arg_buffer, arg_buffer_size)
            );
            new (options) Type(args);
        }

        static bool
        port_descriptor( const Methcla_SynthOptions*
                       , Methcla_PortCount index
                       , Methcla_PortDescriptor* port )
        {
            if (index < PortDescriptor::numPorts())
            {
                *port = PortDescriptor::descriptor(static_cast<typename PortDescriptor::Port>(index));
                return true;
            }
            return false;
        }
    };

    template <class Synth, class Options, class PortDescriptor> class SynthClass
    {
        static void
        construct( const Methcla_World* world
                 , const Methcla_SynthDef* synthDef
                 , const Methcla_SynthOptions* options
                 , Methcla_Synth* synth )
        {
            assert(world != nullptr);
            assert(options != nullptr);
            new (synth) Synth(World<Synth>(world), synthDef, *static_cast<const typename Options::Type*>(options));
        }

        static void
        connect( Methcla_Synth* synth
               , Methcla_PortCount port
               , void* data)
        {
            static_cast<Synth*>(synth)->connect(static_cast<typename PortDescriptor::Port>(port), data);
        }

        static void
        activate(const Methcla_World* world, Methcla_Synth* synth)
        {
            static_cast<Synth*>(synth)->activate(World<Synth>(world));
        }

        static void
        process(const Methcla_World* world, Methcla_Synth* synth, size_t numFrames)
        {
            static_cast<Synth*>(synth)->process(World<Synth>(world), numFrames);
        }

        static void
        destroy(const Methcla_World*, Methcla_Synth* synth)
        {
            static_cast<Synth*>(synth)->~Synth();
        }

    public:
        void operator()(const Methcla_Host* host, const char* uri)
        {
            static const Methcla_SynthDef kClass =
            {
                uri,
                sizeof(Synth),
                sizeof(typename Options::Type),
                Options::configure,
                Options::port_descriptor,
                construct,
                connect,
                activate,
                process,
                destroy
            };
            methcla_host_register_synthdef(host, &kClass);
        }
    };

    template <class Synth, class Options, class Ports> using StaticSynthClass
        = SynthClass<Synth, StaticSynthOptions<Options,Ports>, Ports>;
} }

#endif // METHCLA_PLUGIN_HPP_INCLUDED
