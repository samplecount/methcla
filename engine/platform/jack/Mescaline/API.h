#ifndef Mescaline_API_h
#define Mescaline_API_h

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

typedef struct Engine Engine;

MESCALINE_EXPORT Engine* Mescaline_Engine_new();
MESCALINE_EXPORT void Mescaline_Engine_free(Engine* engine);

MESCALINE_EXPORT void Mescaline_Engine_start(Engine* engine);
MESCALINE_EXPORT void Mescaline_Engine_stop(Engine* engine);

#endif /* Mescaline_API_h */
