// Copyright (C) 2026 Methcla contributors
//
// SPDX-License-Identifier: Apache-2.0

#pragma once

// Test synth plugins that exercise the RT-side resource primitives
// (`methcla_world_resource_acquire`, `methcla_world_resource_release`,
// `methcla_world_perform_with_resources`). Used by resource_tests.cpp; not
// shipped to clients.

#include "test_resource.hpp"

#include <methcla/plugin.h>

#include <atomic>
#include <cstring>
#include <new>

#include <oscpp/client.hpp>
#include <oscpp/server.hpp>

// Holds a resource ref for its full lifetime. Options: (int resourceId).
// On construct: acquires the resource id with METHCLA_TEST_RESOURCE_URI; if
// acquire returns null the synth records the failure and produces no output.
// On destroy: releases if acquired.
#define METHCLA_TEST_HOLD_SYNTH_URI METHCLA_PLUGINS_URI "/test-hold-resource"

namespace TestHoldSynth {

    struct Options
    {
        int32_t resourceId;
    };

    struct Synth
    {
        Methcla_ResourceId id;
        void*              data;
    };

    static void configure(const void* tag_buffer, size_t tag_size,
                          const void* arg_buffer, size_t arg_size,
                          Methcla_SynthOptions* options)
    {
        auto* opts = static_cast<Options*>(options);
        new (opts) Options{};
        OSCPP::Server::ArgStream args(OSCPP::ReadStream(tag_buffer, tag_size),
                                      OSCPP::ReadStream(arg_buffer, arg_size));
        opts->resourceId = args.int32();
    }

    static bool port_descriptor(const Methcla_SynthOptions*, Methcla_PortCount,
                                Methcla_PortDescriptor*)
    {
        return false;
    }

    static void construct(Methcla_World* world, const Methcla_SynthDef*,
                          const Methcla_SynthOptions* options,
                          Methcla_Synth*              instance)
    {
        const auto* opts = static_cast<const Options*>(options);
        auto*       self = new (instance) Synth{};
        self->id = opts->resourceId;
        self->data = methcla_world_resource_acquire(world, self->id,
                                                    METHCLA_TEST_RESOURCE_URI);
    }

    static void connect(Methcla_Synth*, Methcla_PortCount, void*)
    {}

    static void activate(Methcla_World*, Methcla_Synth*)
    {}

    static void process(Methcla_World*, Methcla_Synth*, size_t)
    {}

    static void destroy(Methcla_World* world, Methcla_Synth* instance)
    {
        auto* self = static_cast<Synth*>(instance);
        if (self->data)
            methcla_world_resource_release(world, self->id);
        self->~Synth();
    }

    static const Methcla_SynthDef def = {METHCLA_TEST_HOLD_SYNTH_URI,
                                         sizeof(Synth),
                                         sizeof(Options),
                                         configure,
                                         port_descriptor,
                                         construct,
                                         connect,
                                         activate,
                                         process,
                                         destroy};

} // namespace TestHoldSynth

// On activate, dispatches perform_with_resources for one resource. The NRT
// callback emits `/test/perform-with-resources <synthId> <ptrNonNull:1>` so
// tests can observe both the dispatch and that the acquired pointer was
// non-null. Options: (int resourceId, int synthMarker).
#define METHCLA_TEST_PERFORM_SYNTH_URI \
    METHCLA_PLUGINS_URI "/test-perform-with-resources"

namespace TestPerformSynth {

    struct Options
    {
        int32_t resourceId;
        int32_t marker;
    };

    struct Synth
    {
        Methcla_ResourceId id;
        int32_t            marker;
        int32_t            dispatched;
        int32_t            _pad;
    };

    static void configure(const void* tag_buffer, size_t tag_size,
                          const void* arg_buffer, size_t arg_size,
                          Methcla_SynthOptions* options)
    {
        auto* opts = static_cast<Options*>(options);
        new (opts) Options{};
        OSCPP::Server::ArgStream args(OSCPP::ReadStream(tag_buffer, tag_size),
                                      OSCPP::ReadStream(arg_buffer, arg_size));
        opts->resourceId = args.int32();
        opts->marker = args.int32();
    }

    static bool port_descriptor(const Methcla_SynthOptions*, Methcla_PortCount,
                                Methcla_PortDescriptor*)
    {
        return false;
    }

    static void construct(Methcla_World*, const Methcla_SynthDef*,
                          const Methcla_SynthOptions* options,
                          Methcla_Synth*              instance)
    {
        const auto* opts = static_cast<const Options*>(options);
        auto*       self = new (instance) Synth{};
        self->id = opts->resourceId;
        self->marker = opts->marker;
        self->dispatched = 0;
    }

    static void connect(Methcla_Synth*, Methcla_PortCount, void*)
    {}

    static void onNRT(Methcla_Host* host, Methcla_Resource* const* resources,
                      size_t num_resources, void* user_data)
    {
        // Marker passed through the void* parameter (test-only convenience —
        // avoids cross-pool allocation lifetimes).
        const int32_t marker =
            static_cast<int32_t>(reinterpret_cast<intptr_t>(user_data));
        const int32_t ptrOk =
            (num_resources == 1 && resources[0] != nullptr) ? 1 : 0;
        const char*                  address = "/test/perform-with-resources";
        OSCPP::Client::DynamicPacket packet(OSCPP::Size::message(address, 2) +
                                            OSCPP::Size::int32(2));
        packet.openMessage(address, 2)
            .int32(marker)
            .int32(ptrOk)
            .closeMessage();
        host->notify(host, packet.data(), packet.size());
    }

    static void activate(Methcla_World* world, Methcla_Synth* instance)
    {
        auto* self = static_cast<Synth*>(instance);
        if (self->dispatched)
            return;
        self->dispatched = true;
        void* userData =
            reinterpret_cast<void*>(static_cast<intptr_t>(self->marker));
        methcla_world_perform_with_resources(world, &self->id, 1, onNRT,
                                             userData);
    }

    static void process(Methcla_World*, Methcla_Synth*, size_t)
    {}

    static void destroy(Methcla_World*, Methcla_Synth* instance)
    {
        static_cast<Synth*>(instance)->~Synth();
    }

    static const Methcla_SynthDef def = {METHCLA_TEST_PERFORM_SYNTH_URI,
                                         sizeof(Synth),
                                         sizeof(Options),
                                         configure,
                                         port_descriptor,
                                         construct,
                                         connect,
                                         activate,
                                         process,
                                         destroy};

} // namespace TestPerformSynth

static Methcla_Library* methcla_plugins_test_resource_synths(Methcla_Host* host,
                                                             const char*)
{
    methcla_host_register_resource_def(host, &TestResourcePlugin::def);
    methcla_host_register_synthdef(host, &TestHoldSynth::def);
    methcla_host_register_synthdef(host, &TestPerformSynth::def);
    return nullptr;
}
