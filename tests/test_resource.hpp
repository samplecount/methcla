// Copyright (C) 2026 Methcla contributors
//
// SPDX-License-Identifier: Apache-2.0

#pragma once

#include <methcla/plugin.h>

#include <new>

#include <oscpp/server.hpp>

#define METHCLA_TEST_RESOURCE_URI METHCLA_PLUGINS_URI "/test-resource"

namespace TestResourcePlugin {

    struct TestResource
    {
        int dummy = 0;
    };

    struct TestResourceOptions
    {
        bool fail_construct = false;
    };

    static Methcla_ErrorCode configure(const void* tag_buffer, size_t tag_size,
                                       const void* arg_buffer, size_t arg_size,
                                       void* options_ptr)
    {
        auto* opts = static_cast<TestResourceOptions*>(options_ptr);
        new (opts) TestResourceOptions{};
        OSCPP::Server::ArgStream args(OSCPP::ReadStream(tag_buffer, tag_size),
                                      OSCPP::ReadStream(arg_buffer, arg_size));
        if (!args.atEnd())
            opts->fail_construct = args.int32() != 0;
        return kMethcla_NoError;
    }

    static Methcla_Error construct(Methcla_Host*, const Methcla_ResourceDef*,
                                   const void* options_ptr, void* instance)
    {
        const auto* opts = static_cast<const TestResourceOptions*>(options_ptr);
        if (opts && opts->fail_construct)
            return methcla_error_new_with_message(kMethcla_ArgumentError,
                                                  "boom");
        new (instance) TestResource();
        return methcla_no_error();
    }

    static void destroy(Methcla_Host*, void* instance)
    {
        static_cast<TestResource*>(instance)->~TestResource();
    }

    static const Methcla_ResourceDef def = {METHCLA_TEST_RESOURCE_URI,
                                            sizeof(TestResource),
                                            sizeof(TestResourceOptions),
                                            kMethcla_Immutable,
                                            configure,
                                            construct,
                                            destroy};

} // namespace TestResourcePlugin

static Methcla_Library* methcla_plugins_test_resource(Methcla_Host* host,
                                                      const char*)
{
    methcla_host_register_resource_def(host, &TestResourcePlugin::def);
    return nullptr;
}
