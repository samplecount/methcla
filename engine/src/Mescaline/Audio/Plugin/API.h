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
    sample_t* (*fGetInputControl)(MescalineSynth* self, unsigned int index);
    sample_t* (*fGetOutputControl)(MescalineSynth* self, unsigned int index);
};

static inline void MescalineSynthProcess(MescalineSynth* self, unsigned int numFrames, sample_t** inputs, sample_t** outputs)
{
    if (self->fProcess != NULL) (*self->fProcess)(self, numFrames, inputs, outputs);
}

static inline sample_t* MescalineSynthGetInputControl(MescalineSynth* self, unsigned int index)
{
    return self->fGetInputControl == NULL ? NULL : (*self->fGetInputControl)(self, index);
}

static inline sample_t* MescalineSynthGetOutputControl(MescalineSynth* self, unsigned int index)
{
    return self->fGetOutputControl == NULL ? NULL : (*self->fGetOutputControl)(self, index);
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
    const char*     (*fGetInputControlName)(const MescalineSynthDef* self, unsigned int index);
    const char*     (*fGetOutputControlName)(const MescalineSynthDef* self, unsigned int index);

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

static inline const char* MescalineSynthDefGetInputControlName(MescalineSynthDef* self, unsigned int index)
{
    return self->fGetInputControlName == NULL ? NULL : (*self->fGetInputControlName)(self, index);
}

static inline const char* MescalineSynthDefGetOutputControlName(MescalineSynthDef* self, unsigned int index)
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
