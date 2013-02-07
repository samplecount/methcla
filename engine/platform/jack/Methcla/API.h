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

#ifndef METHCLA_API_H
#define METHCLA_API_H

#include "lv2/lv2plug.in/ns/ext/atom/atom.h"
#include "lv2/lv2plug.in/ns/ext/urid/urid.h"

#if defined(__cplusplus)
#   define METHCLA_C_LINKAGE extern "C"
#else
#   define METHCLA_C_LINKAGE
#endif

#if defined _WIN32 || defined __CYGWIN__
  #ifdef BUILDING_DLL
    #ifdef __GNUC__
      #define METHCLA_VISIBLE __attribute__ ((dllexport))
    #else
      #define METHCLA_VISIBLE __declspec(dllexport) // Note: actually gcc seems to also supports this syntax.
    #endif
  #else
    #ifdef __GNUC__
      #define METHCLA_VISIBLE __attribute__ ((dllimport))
    #else
      #define METHCLA_VISIBLE __declspec(dllimport) // Note: actually gcc seems to also supports this syntax.
    #endif
  #endif
#else
  #if __GNUC__ >= 4
    #define METHCLA_VISIBLE __attribute__ ((visibility ("default")))
  #else
    #define METHCLA_VISIBLE
  #endif
#endif

#define METHCLA_EXPORT METHCLA_C_LINKAGE METHCLA_VISIBLE

typedef struct Methcla_Engine Methcla_Engine;

METHCLA_EXPORT Methcla_Engine* Methcla_Engine_new();
METHCLA_EXPORT void Methcla_Engine_free(Methcla_Engine* engine);

METHCLA_EXPORT void Methcla_Engine_start(Methcla_Engine* engine);
METHCLA_EXPORT void Methcla_Engine_stop(Methcla_Engine* engine);

METHCLA_EXPORT LV2_URID Methcla_Engine_mapUri(Methcla_Engine* engine, const char* uri);
METHCLA_EXPORT const char* Methcla_Engine_unmapUri(Methcla_Engine* engine, LV2_URID urid);

typedef void (*Methcla_HandleResponse)(const LV2_Atom* response, void* data);
METHCLA_EXPORT void Methcla_Engine_request(Methcla_Engine* engine, const LV2_Atom* request, Methcla_HandleResponse handler, void* handlerData);

#endif /* METHCLA_API_H */
