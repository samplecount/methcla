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
#include <stddef.h>

typedef void Methcla_Synth;

typedef enum
{
    kMethclaInput,
    kMethclaOutput
} Methcla_PortDirection;

typedef enum
{
    kMethclaControlPort,
    kMethclaAudioPort
} Methcla_PortType;

typedef enum
{
    kMethclaPortFlags   = 0x0
  , kMethclaPortTrigger = 0x1
} Methcla_PortFlags;

typedef struct Methcla_PortDescriptor Methcla_PortDescriptor;

struct Methcla_Port
{
    Methcla_PortDirection direction;
    Methcla_PortType      type;
    Methcla_PortFlags     flags;
};

typedef struct Methcla_Host Methcla_Host;

typedef struct Methcla_SynthDef Methcla_SynthDef;

struct Methcla_SynthDef
{
    //* Synth definition URI.
    const char* uri;

    //* Size of an instance in bytes.
    size_t size;

    //* Number of ports.
    size_t numPorts;

    //* Get port at index.
    const Methcla_Port* (*port)(const Methcla_SynthDef* def, size_t index);

    //* Retrieve metadata as an OSC packet.
    const void* (*metaData)();

    //* Construct a synth instance at the location given.
    void (*construct)(const Methcla_SynthDef* def, Methcla_Synth* synth);

    //* Destroy a synth instance.
    void (*destroy)(const Methcla_SynthDef* def, Methcla_Synth* synth);

    //* Connect port at index to data.
    void (*connect)(Methcla_Synth* synth, size_t index, void* data);

    //* Process numFrames of audio samples.
    void (*run)(Methcla_Synth* self, size_t numFrames);

    //* Activate the synth.
    void (*activate)(Methcla_Synth* self);

    //* Deactivate the synth.
    void (*deactivate)(Methcla_Synth* self);
};

struct Methcla_Host
{
    //* Return the host's audio sample rate.
    double (*sampleRate)(const Methcla_Host* self);
    //* Register a synth definition.
    void (*registerSynthDef)(Methcla_Host* self, const Methcla_SynthDef* synthDef);
};

typedef void (*Methcla_PluginLoadFunction)(Methcla_Host* host);

static inline double Methcla_Host_sampleRate(Methcla_Host* self)
{
    return self->sampleRate(self);
}

static inline void Methcla_Host_registerSynthDef(Methcla_Host* self, Methcla_SynthDef* synthDef)
{
    self->registerSynthDef(self, synthDef);
}

typedef struct Methcla_LibraryDescriptor Methcla_LibraryDescriptor;

typedef void* Methcla_LibraryHandle;

struct Methcla_LibraryDescriptor
{
    Methcla_LibraryHandle handle;
    void (*destroy)(Methcla_LibraryHandle handle);
};

typedef const Methcla_LibraryDescriptor* (*Methcla_LibraryDescriptorFunction)(Methcla_Host* host);

// #define MESCALINE_MAKE_INIT_FUNC(name) MethclaInit_##name
// #define MESCALINE_INIT_FUNC(name) MESCALINE_MAKE_INIT_FUNC(name)

#endif /* METHCLA_PLUGIN_H_INCLUDED */
