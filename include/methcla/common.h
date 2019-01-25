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

#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#if defined(__cplusplus)
extern "C" {
#endif

#if defined(__cplusplus)
#    define METHCLA_C_LINKAGE extern "C"
#else
#    define METHCLA_C_LINKAGE
#endif

#if defined _WIN32 || defined __CYGWIN__
#    if defined(BUILDING_DLL)
#        define METHCLA_VISIBILITY __declspec(dllexport)
#    else
#        if __GNUC__
#            define METHCLA_VISIBILITY
#        else
#            define METHCLA_VISIBILITY __declspec(dllimport)
#        endif
#    endif
#else
#    if (__GNUC__ >= 4) || (defined(__clang__) && (__clang_major__ >= 4))
#        define METHCLA_VISIBILITY __attribute__((visibility("default")))
#    else
#        define METHCLA_VISIBILITY
#    endif
#endif

#define METHCLA_EXPORT METHCLA_C_LINKAGE METHCLA_VISIBILITY

//* Audio sample type
typedef float Methcla_AudioSample;

//* Time in seconds.
typedef double Methcla_Time;

typedef struct
{
    const void* data;
    size_t      size;
} Methcla_OSCPacket;

typedef enum
{
    kMethcla_NoError = 0,

    /* Generic error codes */
    kMethcla_UnspecifiedError,
    kMethcla_LogicError,
    kMethcla_ArgumentError,
    kMethcla_MemoryError,
    kMethcla_UnimplementedError,
    kMethcla_SystemError,

    /* Engine errors */
    kMethcla_SynthDefNotFoundError = 1000,
    kMethcla_NodeIdError,
    kMethcla_NodeTypeError,

    /* File errors */
    kMethcla_FileNotFoundError = 2000,
    kMethcla_FileExistsError,
    kMethcla_PermissionsError,
    kMethcla_UnsupportedFileTypeError,
    kMethcla_UnsupportedDataFormatError,
    kMethcla_InvalidFileError,

    /* Audio driver errors */
    kMethcla_DeviceUnavailableError = 3000,
} Methcla_ErrorCode;

static inline const char* methcla_error_code_description(Methcla_ErrorCode code)
{
    switch (code)
    {
        case kMethcla_NoError:
            return "No error";

        /* Generic error codes */
        case kMethcla_UnspecifiedError:
            return "Unspecified error";
        case kMethcla_LogicError:
            return "Logic error";
        case kMethcla_ArgumentError:
            return "Invalid argument";
        case kMethcla_MemoryError:
            return "Out of memory";
        case kMethcla_UnimplementedError:
            return "Operation not implemented";
        case kMethcla_SystemError:
            return "Generic operating system error";

        /* Engine errors */
        case kMethcla_SynthDefNotFoundError:
            return "SynthDef not found";
        case kMethcla_NodeIdError:
            return "Invalid node id";
        case kMethcla_NodeTypeError:
            return "Invalid node type";

        /* File errors */
        case kMethcla_FileNotFoundError:
            return "File not found";
        case kMethcla_FileExistsError:
            return "File already exists";
        case kMethcla_PermissionsError:
            return "Insufficient file permissions";
        case kMethcla_UnsupportedFileTypeError:
            return "Unsupported file type";
        case kMethcla_UnsupportedDataFormatError:
            return "Unsupported data format";
        case kMethcla_InvalidFileError:
            return "Malformed file contents";

        /* Audio driver errors */
        case kMethcla_DeviceUnavailableError:
            return "Audio device not available";
    }

    return "Invalid Methcla_ErrorCode value";
}

typedef struct Methcla_Error
{
    Methcla_ErrorCode error_code;
    char*             error_message;
} Methcla_Error;

static inline bool methcla_is_ok(const Methcla_Error error)
{
    return error.error_code == kMethcla_NoError;
}

static inline bool methcla_is_error(const Methcla_Error error)
{
    return error.error_code != kMethcla_NoError;
}

static inline bool methcla_error_has_code(const Methcla_Error error,
                                          Methcla_ErrorCode   code)
{
    return error.error_code == code;
}

static inline Methcla_ErrorCode methcla_error_code(const Methcla_Error error)
{
    return error.error_code;
}

static inline const char* methcla_error_message(const Methcla_Error error)
{
    return error.error_message;
}

//* Create a new Methcla_Error with a specific error code.
//  The error message is set to NULL.
static inline Methcla_Error methcla_error_new(Methcla_ErrorCode code)
{
    Methcla_Error result;
    result.error_code = code;
    result.error_message = NULL;
    return result;
}

//* Create a new Methcla_Error with a specific error code and message.
static inline Methcla_Error
methcla_error_new_with_message(Methcla_ErrorCode code, const char* message)
{
    Methcla_Error result;
    result.error_code = code;
    result.error_message = strdup(message);
    return result;
}

//* Free the resources associated with a Methcla_Error.
static inline void methcla_error_free(Methcla_Error error)
{
    if (error.error_message != NULL)
    {
        free(error.error_message);
    }
}

//* Return a Methcla_Error indicating that no error has occurred.
static inline Methcla_Error methcla_no_error()
{
    return methcla_error_new(kMethcla_NoError);
}

#if defined(__cplusplus)
}
#endif

#endif /* METHCLA_COMMON_H_INCLUDED */
