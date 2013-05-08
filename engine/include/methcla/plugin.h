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

#ifndef METHCLA_PLUGIN_H_INCLUDED
#define METHCLA_PLUGIN_H_INCLUDED

#include <methcla/common.h>
#include <methcla/file.h>
#include <stdbool.h>
#include <stddef.h>

typedef void Methcla_Synth;

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
    kMethcla_PortFlags  = 0x0
  , kMethcla_Trigger    = 0x1
} Methcla_PortFlags;

typedef struct Methcla_Port Methcla_Port;

struct Methcla_Port
{
    Methcla_PortDirection direction;
    Methcla_PortType      type;
    Methcla_PortFlags     flags;
};

typedef struct Methcla_World Methcla_World;

typedef void* Methcla_WorldHandle;

struct Methcla_World
{
    Methcla_WorldHandle handle;

    double (*sampleRate)(Methcla_WorldHandle handle);
    // Realtime memory allocation
    // Asynchronous commands
};

static inline double methcla_world_samplerate(const Methcla_World* world)
{
    return world->sampleRate(world->handle);
}

typedef struct Methcla_SynthDef Methcla_SynthDef;

struct Methcla_SynthDef
{
    //* Synth definition URI.
    const char* uri;

    //* Size of an instance in bytes.
    size_t size;

    //* Get port at index.
    bool (*port)(const Methcla_SynthDef* def, size_t index, Methcla_Port* port);

    //* Construct a synth instance at the location given.
    void (*construct)(const Methcla_SynthDef* def, const Methcla_World* world, Methcla_Synth* synth);

    //* Connect port at index to data.
    void (*connect)(Methcla_Synth* synth, size_t index, void* data);

    //* Process numFrames of audio samples.
    void (*process)(Methcla_Synth* synth, size_t numFrames);

    //* Destroy a synth instance.
    void (*destroy)(const Methcla_SynthDef* def, const Methcla_World* world, Methcla_Synth* synth);
};

typedef struct Methcla_Host
{
    void* handle;

    //* Register a synth definition.
    void (*registerSynthDef)(const struct Methcla_Host* host, const Methcla_SynthDef* synthDef);

    //* Sound file access
    const Methcla_SoundFileAPI* (*soundFileAPI)(const struct Methcla_Host* host, const char* mimeType);
} Methcla_Host;

static inline void methcla_host_register_synthdef(const Methcla_Host* host, const Methcla_SynthDef* synthDef)
{
    host->registerSynthDef(host, synthDef);
}

static inline Methcla_SoundFile* methcla_host_soundfile_open(const Methcla_Host* host, const char* path, Methcla_FileMode mode, Methcla_SoundFileInfo* info)
{
    const Methcla_SoundFileAPI* api = host->soundFileAPI(host, "audio/*");
    return api == NULL ? NULL : api->open(api, path, mode, info);
}

typedef struct Methcla_Library Methcla_Library;

typedef void* Methcla_LibraryHandle;

struct Methcla_Library
{
    Methcla_LibraryHandle handle;

    void (*destroy)(Methcla_LibraryHandle handle);
};

typedef const Methcla_Library* (*Methcla_LibraryFunction)(const Methcla_Host* host, const char* bundlePath);

// #define MESCALINE_MAKE_INIT_FUNC(name) MethclaInit_##name
// #define MESCALINE_INIT_FUNC(name) MESCALINE_MAKE_INIT_FUNC(name)

#endif /* METHCLA_PLUGIN_H_INCLUDED */
