// Copyright (C) 2012-2013 Samplecount S.L.
// Copyright (C) 2026 Methcla contributors
//
// SPDX-License-Identifier: Apache-2.0

#pragma once

#include <methcla/engine.hpp>
#include <methcla/plugins/sine.h>

Methcla::Engine* makeEngine()
{
    //    NSString* resources = [[NSBundle mainBundle] resourcePath];
    //    NSString* bundles = [resources
    //    stringByAppendingPathComponent:@"lv2/bundles"];

    Methcla::Engine* engine = new Methcla::Engine(
        {Methcla::Option::driverBufferSize(256),
         Methcla::Option::pluginLibrary(methcla_plugins_sine)});
    engine->start();

    return engine;
}
