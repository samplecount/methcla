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

#ifndef METHCLA_COMMON_H_INCLUDED
#define METHCLA_COMMON_H_INCLUDED

#if defined(__cplusplus)
#   define METHCLA_C_LINKAGE extern "C"
#else
#   define METHCLA_C_LINKAGE
#endif

#if defined _WIN32 || defined __CYGWIN__
  #if defined(BUILDING_DLL)
    #if defined(__GNUC__) || defined(__clang__)
      #define METHCLA_VISIBLE __attribute__ ((dllexport))
    #else
      #define METHCLA_VISIBLE __declspec(dllexport) // Note: actually gcc seems to also supports this syntax.
    #endif
  #else
    #if defined(__GNUC__) || defined(__clang__)
      #define METHCLA_VISIBLE __attribute__ ((dllimport))
    #else
      #define METHCLA_VISIBLE __declspec(dllimport) // Note: actually gcc seems to also supports this syntax.
    #endif
  #endif
#else
  #if (__GNUC__ >= 4) || (defined(__clang__) && (__clang_major__ >= 4))
    #define METHCLA_VISIBLE __attribute__ ((visibility ("default")))
  #else
    #define METHCLA_VISIBLE
  #endif
#endif

#define METHCLA_EXPORT METHCLA_C_LINKAGE METHCLA_VISIBLE

//* Audio sample type
typedef float Methcla_AudioSample;

#endif /* METHCLA_COMMON_H_INCLUDED */
