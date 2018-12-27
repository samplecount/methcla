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
