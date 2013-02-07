/*
  LV2 Amp Example Plugin
  Copyright 2006-2011 Steve Harris <steve@plugin.org.uk>,
                      David Robillard <d@drobilla.net>

  Permission to use, copy, modify, and/or distribute this software for any
  purpose with or without fee is hereby granted, provided that the above
  copyright notice and this permission notice appear in all copies.

  THIS SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
  WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
  MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
  ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
  ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
*/

#include <math.h>
#include <stdlib.h>
#include <string.h>

#include <lv2/lv2plug.in/ns/lv2core/lv2.h>
#include <lv2/methc.la/ext/rt-instantiate/rt-instantiate.h>
#include <lv2/methc.la/plugins/scope.lv2/scope.hpp>

#define SCOPE_URI "http://methc.la/lv2/plugins/sine"

using namespace Methcla::LV2;

typedef enum {
    SCOPE_INPUT = 0
  , SCOPE_OUTPUT = 1
} PortIndex;

typedef struct {
    float* input;
    LV2_Atom_Port_Buffer* output;
} Scope;

static uint32_t instance_size(const LV2_Descriptor* descriptor)
{
    return sizeof(Scope);
}

static uint32_t instance_alignment(const LV2_Descriptor* descriptor)
{
    return __alignof__(Scope);
}

static LV2_Handle
placement_instantiate(const LV2_Descriptor*     descriptor,
                      void*                     location,
                      double                    rate,
                      const char*               bundle_path,
                      const LV2_Feature* const* features)
{
    Scope* scope = new (location) Scope();
    return (LV2_Handle)scope;
}

static LV2_Handle
instantiate(const LV2_Descriptor*     descriptor,
            double                    rate,
            const char*               bundle_path,
            const LV2_Feature* const* features)
{
    void* location = malloc(instance_size(descriptor));
    return location == 0 ? 0 : placement_instantiate(
                                    descriptor
                                  , location
                                  , rate
                                  , bundle_path
                                  , features );
}

static void
activate(LV2_Handle instance)
{
}

static void
connect_port(LV2_Handle instance,
             uint32_t   port,
             void*      data)
{
    Scope* scope = (Scope*)instance;

    switch ((PortIndex)port) {
    case SCOPE_INPUT:
        scope->input = (float*)data;
        break;
    case SCOPE_OUTPUT:
        scope->output = (LV2_Atom_Port_Buffer*)data;
        break;
    }
}

static void
run(LV2_Handle instance, uint32_t numFrames)
{
    Scope* self = (Scope*)instance;

    float* const input = self->input;
    (LV2_Atom_Sequence*)self->output->data

    for (uint32_t k = 0; k < numFrames; k++) {
        output[k] = sinf(phase);
        phase += phaseInc;
    }
    
    sine->phase = phase;
}

static void
deactivate(LV2_Handle instance)
{
}

static void
cleanup(LV2_Handle instance)
{
    free(instance);
}

static const LV2_RT_Instantiate_Interface rtiInterface = {
   instance_size
 , instance_alignment
 , placement_instantiate
};

const void*
extension_data(const char* uri)
{
    if (strcmp(uri, LV2_RT_INSTANTIATE__INTERFACE) == 0) {
        return &rtiInterface;
    }
    return NULL;
}

static const LV2_Descriptor descriptor = {
    SINE_URI,
    instantiate,
    connect_port,
    activate,
    run,
    deactivate,
    cleanup,
    extension_data
};

LV2_SYMBOL_EXPORT
const LV2_Descriptor*
methcla_scope_lv2_descriptor(uint32_t index)
{
    switch (index) {
    case 0:
        return &descriptor;
    default:
        return NULL;
    }
}
