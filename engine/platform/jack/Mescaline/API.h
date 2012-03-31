#ifndef Mescaline_API_h
#define Mescaline_API_h

#include "lv2/lv2plug.in/ns/ext/atom/atom.h"
#include "lv2/lv2plug.in/ns/ext/urid/urid.h"

#if defined(__cplusplus)
#   define MESCALINE_C_LINKAGE extern "C"
#else
#   define MESCALINE_C_LINKAGE
#endif

#if defined _WIN32 || defined __CYGWIN__
  #ifdef BUILDING_DLL
    #ifdef __GNUC__
      #define MESCALINE_VISIBLE __attribute__ ((dllexport))
    #else
      #define MESCALINE_VISIBLE __declspec(dllexport) // Note: actually gcc seems to also supports this syntax.
    #endif
  #else
    #ifdef __GNUC__
      #define MESCALINE_VISIBLE __attribute__ ((dllimport))
    #else
      #define MESCALINE_VISIBLE __declspec(dllimport) // Note: actually gcc seems to also supports this syntax.
    #endif
  #endif
#else
  #if __GNUC__ >= 4
    #define MESCALINE_VISIBLE __attribute__ ((visibility ("default")))
  #else
    #define MESCALINE_VISIBLE
  #endif
#endif

#define MESCALINE_EXPORT MESCALINE_C_LINKAGE MESCALINE_VISIBLE

typedef struct Mescaline_Engine Mescaline_Engine;

MESCALINE_EXPORT Mescaline_Engine* Mescaline_Engine_new();
MESCALINE_EXPORT void Mescaline_Engine_free(Mescaline_Engine* engine);

MESCALINE_EXPORT void Mescaline_Engine_start(Mescaline_Engine* engine);
MESCALINE_EXPORT void Mescaline_Engine_stop(Mescaline_Engine* engine);

MESCALINE_EXPORT LV2_URID Mescaline_Engine_mapUri(Mescaline_Engine* engine, const char* uri);
MESCALINE_EXPORT const char* Mescaline_Engine_unmapUri(Mescaline_Engine* engine, LV2_URID urid);

typedef void (*Mescaline_HandleResponse)(const LV2_Atom* response, void* data);
MESCALINE_EXPORT void Mescaline_Engine_request(Mescaline_Engine* engine, const LV2_Atom* request, Mescaline_HandleResponse handler, void* handlerData);

#endif /* Mescaline_API_h */
