// Copyright (C) 2012-2013 Samplecount S.L.
// Copyright (C) 2026 Methcla contributors
//
// SPDX-License-Identifier: Apache-2.0

#pragma once

#include <methcla/common.h>
#include <methcla/file.h>
#include <methcla/log.h>

#include <assert.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#if defined(__cplusplus)
extern "C" {
#endif

#define METHCLA_PLUGINS_URI "http://methc.la/plugins"

#if METHCLA_STATIC_PLUGIN
#    define METHCLA_PLUGIN_LOAD(func) func
#else
#    define METHCLA_PLUGIN_LOAD(func) methcla_plugin_load
#endif // METHCLA_STATIC_PLUGIN

//* Realtime interface.
typedef struct Methcla_World Methcla_World;

//* Non-realtime interface.
typedef struct Methcla_Host Methcla_Host;

//* Synth handle managed by a plugin.
typedef void Methcla_Synth;

//* Callback function type for performing commands in the non-realtime context.
typedef void (*Methcla_HostPerformFunction)(Methcla_Host* host, void* data);

//* Callback function type for performing commands in the realtime context.
typedef void (*Methcla_WorldPerformFunction)(Methcla_World* world, void* data);

//* Resource handle exposed by the engine via resource_acquire. Resources are
//* identified by integer id (Methcla_ResourceId, declared below) and typed by
//* URI; consumers cast this opaque pointer to the layout published in the
//* resource type's header after a successful URI-checked acquire.
typedef void Methcla_Resource;

typedef int32_t Methcla_ResourceId;

//* Callback function type for accessing one or more resources from the
//* non-realtime context. The engine acquires each listed resource on the RT
//* thread, invokes this callback on a worker thread with the data pointer
//* array, and releases the resources after the callback returns.
typedef void (*Methcla_PerformWithResourcesFunction)(
    Methcla_Host* host, Methcla_Resource* const* resources,
    size_t num_resources, void* user_data);

//* Realtime interface
struct Methcla_World
{
    //* Handle for implementation specific data.
    void* handle;

    //* Return engine sample rate.
    double (*samplerate)(const Methcla_World* world);

    //* Return maximum audio block size.
    size_t (*block_size)(const Methcla_World* world);

    //* Return the time at the start of the current audio block in seconds.
    Methcla_Time (*current_time)(const Methcla_World* world);

    // Realtime memory allocation
    void* (*alloc)(Methcla_World* world, size_t size);
    void (*free)(Methcla_World* world, void* ptr);
    void* (*alloc_aligned)(Methcla_World* world, size_t alignment, size_t size);
    void (*free_aligned)(Methcla_World* world, void* ptr);

    //* Schedule a command for execution in the non-realtime context.
    void (*perform_command)(Methcla_World*              world,
                            Methcla_HostPerformFunction perform, void* data);

    //* Log a message and a newline character.
    void (*log_line)(Methcla_World* world, Methcla_LogLevel level,
                     const char* message);

    //* Free synth.
    void (*synth_done)(Methcla_World* world, Methcla_Synth* synth);

    //* Acquire a Live resource by id, checking that its type URI matches
    //* `expected_uri` (compared by string equality). Returns the resource's
    //* data pointer (cast by the consumer to the type published in the
    //* resource's header) and increments its refcount. Returns NULL if the
    //* id is out of range, the slot is not Live, or the URI does not match.
    Methcla_Resource* (*resource_acquire)(Methcla_World*     world,
                                          Methcla_ResourceId id,
                                          const char*        expected_uri);

    //* Decrement the refcount of a resource previously acquired by
    //* resource_acquire. Each successful acquire must be paired with exactly
    //* one release.
    void (*resource_release)(Methcla_World* world, Methcla_ResourceId id);

    //* Bracket a non-realtime callback with RT-side acquire/release of the
    //* listed resources. The engine acquires every resource (incrementing
    //* its refcount) on the RT thread, dispatches the callback to a worker
    //* thread with the data pointer array, and releases the resources back
    //* on the RT thread after the callback returns. The resource pointers
    //* are valid only for the duration of the callback. If any acquire
    //* fails the engine releases the resources it has already acquired and
    //* the callback is not invoked.
    void (*perform_with_resources)(Methcla_World*                       world,
                                   const Methcla_ResourceId*            ids,
                                   size_t                               num_ids,
                                   Methcla_PerformWithResourcesFunction perform,
                                   void* user_data);
};

static inline double methcla_world_samplerate(const Methcla_World* world)
{
    assert(world && world->samplerate);
    return world->samplerate(world);
}

static inline size_t methcla_world_block_size(const Methcla_World* world)
{
    assert(world && world->block_size);
    return world->block_size(world);
}

static inline Methcla_Time
methcla_world_current_time(const Methcla_World* world)
{
    assert(world);
    assert(world->current_time);
    return world->current_time(world);
}

static inline void* methcla_world_alloc(Methcla_World* world, size_t size)
{
    assert(world && world->alloc);
    return world->alloc(world, size);
}

static inline void methcla_world_free(Methcla_World* world, void* ptr)
{
    assert(world && world->free);
    world->free(world, ptr);
}

static inline void* methcla_world_alloc_aligned(Methcla_World* world,
                                                size_t alignment, size_t size)
{
    assert(world && world->alloc_aligned);
    return world->alloc_aligned(world, alignment, size);
}

static inline void methcla_world_free_aligned(Methcla_World* world, void* ptr)
{
    assert(world && world->free_aligned);
    world->free_aligned(world, ptr);
}

static inline void
methcla_world_perform_command(Methcla_World*              world,
                              Methcla_HostPerformFunction perform, void* data)
{
    assert(world && world->perform_command);
    assert(perform);
    world->perform_command(world, perform, data);
}

static inline void methcla_world_log_line(Methcla_World*   world,
                                          Methcla_LogLevel level,
                                          const char*      message)
{
    assert(world);
    assert(world->log_line);
    assert(message);
    world->log_line(world, level, message);
}

static inline void methcla_world_synth_done(Methcla_World* world,
                                            Methcla_Synth* synth)
{
    assert(world);
    assert(world->synth_done);
    assert(synth);
    world->synth_done(world, synth);
}

static inline Methcla_Resource*
methcla_world_resource_acquire(Methcla_World* world, Methcla_ResourceId id,
                               const char* expected_uri)
{
    assert(world && world->resource_acquire);
    assert(expected_uri);
    return world->resource_acquire(world, id, expected_uri);
}

static inline void methcla_world_resource_release(Methcla_World*     world,
                                                  Methcla_ResourceId id)
{
    assert(world && world->resource_release);
    world->resource_release(world, id);
}

static inline void methcla_world_perform_with_resources(
    Methcla_World* world, const Methcla_ResourceId* ids, size_t num_ids,
    Methcla_PerformWithResourcesFunction perform, void* user_data)
{
    assert(world && world->perform_with_resources);
    assert(perform);
    assert(num_ids == 0 || ids != NULL);
    world->perform_with_resources(world, ids, num_ids, perform, user_data);
}

typedef enum
{
    kMethcla_Immutable,
    kMethcla_Mutable
} Methcla_ResourceMutability;

typedef struct Methcla_ResourceDef Methcla_ResourceDef;

struct Methcla_ResourceDef
{
    //* Unique resource type URI.
    const char* uri;

    //* Size of an instance in bytes.
    size_t instance_size;

    //* Size of options struct in bytes.
    size_t options_size;

    //* Mutability hint.
    Methcla_ResourceMutability mutability;

    //* Parse OSC options and fill options struct. Returns kMethcla_NoError on
    //* success. Runs on the RT thread; must not allocate.
    Methcla_ErrorCode (*configure)(const void* tag_buffer, size_t tag_size,
                                   const void* arg_buffer, size_t arg_size,
                                   void* options);

    //* Construct a resource instance at the given location. Returns
    //* methcla_no_error() on success. On failure the engine takes ownership of
    //* the returned Methcla_Error and emits /resource/error; destroy will not
    //* be called.
    Methcla_Error (*construct)(Methcla_Host*              host,
                               const Methcla_ResourceDef* def,
                               const void* options, void* instance);

    //* Destroy a resource instance.
    void (*destroy)(Methcla_Host* host, void* instance);
};

typedef enum
{
    kMethcla_Input,
    kMethcla_Output
} Methcla_PortDirection;

typedef enum
{
    kMethcla_ControlPort,
    kMethcla_AudioPort
} Methcla_PortType;

typedef enum
{
    kMethcla_PortFlags = 0x0,
    kMethcla_Trigger = 0x1
} Methcla_PortFlags;

typedef struct Methcla_PortDescriptor Methcla_PortDescriptor;

struct Methcla_PortDescriptor
{
    Methcla_PortDirection direction;
    Methcla_PortType      type;
    Methcla_PortFlags     flags;
};

typedef uint16_t Methcla_PortCount;

typedef void Methcla_SynthOptions;

typedef struct Methcla_SynthDef Methcla_SynthDef;

struct Methcla_SynthDef
{
    //* Synth definition URI.
    const char* uri;

    //* Size of an instance in bytes.
    size_t instance_size;

    //* Size of options struct in bytes.
    size_t options_size;

    //* Parse OSC options and fill options struct.
    void (*configure)(const void* tag_buffer, size_t tag_size,
                      const void* arg_buffer, size_t arg_size,
                      Methcla_SynthOptions* options);

    //* Get port descriptor at index.
    bool (*port_descriptor)(const Methcla_SynthOptions* options,
                            Methcla_PortCount           index,
                            Methcla_PortDescriptor*     port);

    //* Construct a synth instance at the location given.
    void (*construct)(Methcla_World* world, const Methcla_SynthDef* def,
                      const Methcla_SynthOptions* options,
                      Methcla_Synth*              synth);

    //* Connect port at index to data.
    void (*connect)(Methcla_Synth* synth, Methcla_PortCount index, void* data);

    //* Activate the synth instance just before starting to call `process`.
    void (*activate)(Methcla_World* world, Methcla_Synth* synth);

    //* Process numFrames of audio samples.
    void (*process)(Methcla_World* world, Methcla_Synth* synth,
                    size_t numFrames);

    //* Destroy a synth instance.
    void (*destroy)(Methcla_World* world, Methcla_Synth* synth);
};

struct Methcla_Host
{
    //* Handle for implementation specific data.
    void* handle;

    //* Register a synth definition.
    void (*register_synthdef)(Methcla_Host* host, const Methcla_SynthDef* def);

    //* Register a resource type definition.
    void (*register_resource_def)(Methcla_Host*              host,
                                  const Methcla_ResourceDef* def);

    //* Register sound file API.
    void (*register_soundfile_api)(Methcla_Host*         host,
                                   Methcla_SoundFileAPI* api);

    //* Allocate a block of memory
    void* (*alloc)(Methcla_Host* context, size_t size);

    //* Free a block of memory previously allocated by alloc or alloc_aligned.
    void (*free)(Methcla_Host* context, void* ptr);

    //* Allocate a block of aligned memory.
    void* (*alloc_aligned)(Methcla_Host* context, size_t alignment,
                           size_t size);

    //* Free a block of memory previously allocated by alloc or alloc_aligned.
    void (*free_aligned)(Methcla_Host* context, void* ptr);

    //* Open sound file.
    Methcla_Error (*soundfile_open)(const Methcla_Host* host, const char* path,
                                    Methcla_FileMode       mode,
                                    Methcla_SoundFile**    file,
                                    Methcla_SoundFileInfo* info);

    //* Schedule a command for execution in the realtime context.
    void (*perform_command)(Methcla_Host*                      host,
                            const Methcla_WorldPerformFunction perform,
                            void*                              data);

    //* Send an OSC notification packet to the client.
    void (*notify)(Methcla_Host* host, const void* packet, size_t size);

    //* Log a message and a newline character.
    void (*log_line)(Methcla_Host* host, Methcla_LogLevel level,
                     const char* message);
};

static inline void methcla_host_register_synthdef(Methcla_Host*           host,
                                                  const Methcla_SynthDef* def)
{
    assert(host && host->register_synthdef);
    assert(def);
    host->register_synthdef(host, def);
}

static inline void
methcla_host_register_resource_def(Methcla_Host*              host,
                                   const Methcla_ResourceDef* def)
{
    assert(host && host->register_resource_def);
    assert(def);
    host->register_resource_def(host, def);
}

static inline void
methcla_host_register_soundfile_api(Methcla_Host*         host,
                                    Methcla_SoundFileAPI* api)
{
    assert(host && host->register_soundfile_api && api);
    host->register_soundfile_api(host, api);
}

static inline void* methcla_host_alloc(Methcla_Host* context, size_t size)
{
    assert(context);
    assert(context->alloc);
    return context->alloc(context, size);
}

static inline void* methcla_host_alloc_aligned(Methcla_Host* context,
                                               size_t alignment, size_t size)
{
    assert(context);
    assert(context->alloc_aligned);
    return context->alloc_aligned(context, alignment, size);
}

static inline void methcla_host_free(Methcla_Host* context, void* ptr)
{
    assert(context);
    assert(context->free);
    context->free(context, ptr);
}

static inline Methcla_Error
methcla_host_soundfile_open(const Methcla_Host* host, const char* path,
                            Methcla_FileMode mode, Methcla_SoundFile** file,
                            Methcla_SoundFileInfo* info)
{
    assert(host && host->soundfile_open);
    assert(path);
    assert(file);
    assert(info);
    return host->soundfile_open(host, path, mode, file, info);
}

static inline void
methcla_host_perform_command(Methcla_Host*                host,
                             Methcla_WorldPerformFunction perform, void* data)
{
    assert(host && host->perform_command);
    host->perform_command(host, perform, data);
}

static inline void methcla_host_log_line(Methcla_Host*    host,
                                         Methcla_LogLevel level,
                                         const char*      message)
{
    assert(host);
    assert(host->log_line);
    assert(message);
    host->log_line(host, level, message);
}

typedef struct Methcla_Library Methcla_Library;

struct Methcla_Library
{
    //* Handle for implementation specific data.
    void* handle;

    //* Destroy the library and clean up associated resources.
    void (*destroy)(Methcla_Library* library);
};

typedef Methcla_Library* (*Methcla_LibraryFunction)(Methcla_Host* host,
                                                    const char*   bundlePath);

static inline void methcla_library_destroy(Methcla_Library* library)
{
    assert(library);
    if (library->destroy)
        library->destroy(library);
}

#if defined(__cplusplus)
}
#endif
