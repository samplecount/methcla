// Copyright 2012-2013 Samplecount S.L.
//
// SPDX-License-Identifier: Apache-2.0

#ifndef MethclaMobile_Engine_h
#define MethclaMobile_Engine_h

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

#endif
