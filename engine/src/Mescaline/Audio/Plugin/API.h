#ifndef MESCALINE_API_H_INCLUDED
#define MESCALINE_API_H_INCLUDED

#include <Mescaline/Audio/Plugin/Types.h>
#include <stddef.h>

#if defined(__cplusplus)
#   define MESCALINE_C_LINKAGE extern "C"
#else
#   define MESCALINE_C_LINKAGE
#endif

#define MESCALINE_EXPORT MESCALINE_C_LINKAGE

typedef struct MescalineHost MescalineHost;

struct MescalineSynth
{
    void (*fProcess)(MescalineSynth* self, unsigned int numFrames, sample_t** inputs, sample_t** outputs);
    float* (*fGetControlInput)(MescalineSynth* self, size_t index);
    float* (*fGetControlOutput)(MescalineSynth* self, size_t index);
};

static inline void MescalineSynthProcess(MescalineSynth* self, unsigned int numFrames, sample_t** inputs, sample_t** outputs)
{
    if (self->fProcess != NULL) (*self->fProcess)(self, numFrames, inputs, outputs);
}

static inline float* MescalineSynthGetControlInput(MescalineSynth* self, size_t index)
{
    return self->fGetControlInput == NULL ? NULL : (*self->fGetControlInput)(self, index);
}

static inline float* MescalineSynthGetControlOutput(MescalineSynth* self, size_t index)
{
    return self->fGetControlOutput == NULL ? NULL : (*self->fGetControlOutput)(self, index);
}

typedef struct MescalineSynth MescalineSynth;

struct MescalineSynthDef
{
    const char*     name;

    unsigned int    instanceSize;
    unsigned int    instanceAlignment;

    unsigned int    numAudioInputs;
    unsigned int    numAudioOutputs;
    unsigned int    numControlInputs;
    unsigned int    numControlOutputs;

    // Class initialization/cleanup
    void            (*fInitialize)(MescalineHost* host, const MescalineSynthDef* self);
    void            (*fCleanup)(MescalineHost* host, const MescalineSynthDef* self);

    // Instance creation/destruction
    void            (*fConstruct)( MescalineHost* host
                                 , const MescalineSynthDef* self
                                 , MescalineSynth* instance );
    void            (*fDestroy)( MescalineHost* host
                               , const MescalineSynthDef* self
                               , MescalineSynth* instance );

    // Querying
    const char*     (*fGetControlInputName)(const MescalineSynthDef* self, size_t index);
    const char*     (*fGetOutputControlName)(const MescalineSynthDef* self, size_t index);

    const char*     (*fGetDescription)(const MescalineSynthDef* self);    
};
typedef struct MescalineSynthDef MescalineSynthDef;

static inline void MescalineSynthDefConstruct(MescalineHost* host, MescalineSynthDef* self, MescalineSynth* instance)
{
    if (self->fConstruct != NULL) (*self->fConstruct)(host, self, instance);
}

static inline void MescalineSynthDefDestroy(MescalineHost* host, MescalineSynthDef* self, MescalineSynth* instance)
{
    if (self->fDestroy != NULL) (*self->fDestroy)(host, self, instance);
}

static inline const char* MescalineSynthDefGetControlInputName(MescalineSynthDef* self, size_t index)
{
    return self->fGetControlInputName == NULL ? NULL : (*self->fGetControlInputName)(self, index);
}

static inline const char* MescalineSynthDefGetOutputControlName(MescalineSynthDef* self, size_t index)
{
    return self->fGetOutputControlName == NULL ? NULL : (*self->fGetOutputControlName)(self, index);
}

static inline const char* MescalineSynthDefGetDescription(MescalineSynthDef* self)
{
    return self->fGetDescription == NULL ? NULL : (*self->fGetDescription)(self);
}

struct MescalineHost
{
    unsigned int (*fGetSampleRate)(const MescalineHost* self);
    void (*fRegisterSynthDef)(MescalineHost* self, MescalineSynthDef* synthDef);
};

typedef void (*MescalinePluginLoadFunc)(MescalineHost* host);

static inline unsigned int MescalineHostGetSampleRate(MescalineHost* self)
{
    return (*self->fGetSampleRate)(self);
}

static inline void MescalineHostRegisterSynthDef(MescalineHost* self, MescalineSynthDef* synthDef)
{
    (*self->fRegisterSynthDef)(self, synthDef);
}

typedef void (*MescalineInitFunc)(MescalineHost* host);

#define MESCALINE_MAKE_INIT_FUNC(name) MescalineInit_##name
#define MESCALINE_INIT_FUNC(name) MESCALINE_MAKE_INIT_FUNC(name)

#endif /* MESCALINE_API_H_INCLUDED */
