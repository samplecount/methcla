/*
    Copyright 2012-2013 Samplecount S.L.

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

        http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.
*/

#ifndef METHCLA_ENGINE_H_INCLUDED
#define METHCLA_ENGINE_H_INCLUDED

#include <methcla/common.h>
#include "lv2/lv2plug.in/ns/ext/atom/atom.h"
#include "lv2/lv2plug.in/ns/ext/urid/urid.h"

#define METHCLA_ENGINE_PREFIX   "http://methc.la/engine#"
#define METHCLA_LV2_URI         "http://methc.la/lv2"

//* Generic function pointer.
typedef void (*Methcla_Library_Function)();

struct Methcla_Library_Symbol
{
    const char*              uri;       //*< Resource the symbol is defined for.
    const char*              name;      //*< Symbol name.
    Methcla_Library_Function function;  //*< Symbol function address.
};
typedef struct Methcla_Library_Symbol Methcla_Library_Symbol;

#define METHCLA_END_SYMBOLS { NULL, NULL, NULL }

struct Methcla_Option
{
    const char* uri;    //*< Option URI.
    const void* value;  //*< Option value.
};
typedef struct Methcla_Option Methcla_Option;

#define METHCLA_END_OPTIONS { NULL, NULL }

#define METHCLA_OPTIONS_URI "http://methc.la/engine/options"
#define METHCLA_OPTIONS_PREFIX METHCLA_OPTIONS_URI "#"

#define METHCLA_OPTION__LV2_PATH METHCLA_OPTIONS_PREFIX "lv2Path"

typedef struct Methcla_Engine Methcla_Engine;

METHCLA_EXPORT Methcla_Engine* methcla_engine_new_with_backend(const char* backend_uri, const Methcla_Option* options, const Methcla_Library_Symbol* symbol_table);
METHCLA_EXPORT void methcla_engine_free(Methcla_Engine* engine);

METHCLA_EXPORT void methcla_engine_start(Methcla_Engine* engine);
METHCLA_EXPORT void methcla_engine_stop(Methcla_Engine* engine);

METHCLA_EXPORT LV2_URID methcla_engine_map_uri(Methcla_Engine* engine, const char* uri);
METHCLA_EXPORT const char* methcla_engine_unmap_uri(Methcla_Engine* engine, LV2_URID urid);

typedef void (*Methcla_Response_Handler)(const LV2_Atom* response, void* data);
METHCLA_EXPORT void methcla_engine_request(Methcla_Engine* engine, const LV2_Atom* request, Methcla_Response_Handler handler, void* handler_data);

//* Temporarily exported.
METHCLA_EXPORT void* methcla_engine_impl(Methcla_Engine* engine);

#endif /* METHCLA_ENGINE_H_INCLUDED */
