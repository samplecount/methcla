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

#include "lv2/lv2plug.in/ns/lv2core/lv2.h"
#include "lv2/lv2plug.in/ns/ext/atom/atom.h"
#include "lv2/lv2plug.in/ns/ext/urid/urid.h"

#define METHCLA_ENGINE_PREFIX   "http://methc.la/engine#"
#define METHCLA_LV2_URI         "http://methc.la/lv2"

struct Methcla_LV2_Library
{
    const char*                 plugin;    //*< Plugin URI (e.g. "http://methc.la/lv2/plugins/sine").
    LV2_Lib_Descriptor_Function function;  //*< Symbol function address.
};
typedef struct Methcla_LV2_Library Methcla_LV2_Library;

#define METHCLA_END_LIBRARIES { NULL, NULL }

struct Methcla_Option
{
    const char* key;    //*< Option key URI.
    const void* value;  //*< Option value.
};
typedef struct Methcla_Option Methcla_Option;

#define METHCLA_END_OPTIONS { NULL, NULL }

#define METHCLA_OPTIONS_URI "http://methc.la/engine/options"
#define METHCLA_OPTIONS_PREFIX METHCLA_OPTIONS_URI "#"

#define METHCLA_OPTION__LV2_PATH METHCLA_OPTIONS_PREFIX "lv2Path"
#define METHCLA_OPTION__LV2_LIBRARIES METHCLA_OPTIONS_PREFIX "lv2Libraries"

typedef struct Methcla_Engine Methcla_Engine;

METHCLA_EXPORT Methcla_Engine* methcla_engine_new(const Methcla_Option* options);
METHCLA_EXPORT void methcla_engine_free(Methcla_Engine* engine);

typedef enum Methcla_Error
{
    kMethcla_NoError,
    kMethcla_InvalidArgument,
    kMethcla_BadAlloc,
    kMethcla_Error
} Methcla_Error;

METHCLA_EXPORT Methcla_Error methcla_engine_error(const Methcla_Engine* engine);
METHCLA_EXPORT const char* methcla_engine_error_message(const Methcla_Engine* engine);

METHCLA_EXPORT void methcla_engine_start(Methcla_Engine* engine);
METHCLA_EXPORT void methcla_engine_stop(Methcla_Engine* engine);

METHCLA_EXPORT LV2_URID methcla_engine_map_uri(Methcla_Engine* engine, const char* uri);
METHCLA_EXPORT const char* methcla_engine_unmap_uri(Methcla_Engine* engine, LV2_URID urid);

typedef void (*Methcla_Response_Handler)(void* handler_data, LV2_Atom* request, const LV2_Atom* response);

METHCLA_EXPORT void methcla_engine_request(Methcla_Engine* engine, Methcla_Response_Handler handler, void* handler_data, const LV2_Atom* request);

//* Temporarily exported.
METHCLA_EXPORT void* methcla_engine_impl(Methcla_Engine* engine);

#endif /* METHCLA_ENGINE_H_INCLUDED */
