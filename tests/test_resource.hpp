// Copyright (C) 2026 Methcla contributors
//
// SPDX-License-Identifier: Apache-2.0

#pragma once

#include <methcla/plugin.h>

#include <new>

#define METHCLA_TEST_RESOURCE_URI METHCLA_PLUGINS_URI "/test-resource"

namespace TestResourcePlugin {

    struct TestResource
    {
        int dummy = 0;
    };

    static void construct(Methcla_Host*, const Methcla_ResourceDef*,
                          const void*, void* instance)
    {
        new (instance) TestResource();
    }

    static void destroy(Methcla_Host*, void* instance)
    {
        static_cast<TestResource*>(instance)->~TestResource();
    }

    static const Methcla_ResourceDef def = {METHCLA_TEST_RESOURCE_URI,
                                            sizeof(TestResource),
                                            0,
                                            kMethcla_Immutable,
                                            nullptr,
                                            construct,
                                            destroy};

} // namespace TestResourcePlugin

static Methcla_Library* methcla_plugins_test_resource(Methcla_Host* host,
                                                      const char*)
{
    methcla_host_register_resource_def(host, &TestResourcePlugin::def);
    return nullptr;
}
