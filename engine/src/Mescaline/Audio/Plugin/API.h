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

struct MescalineSynth
{
    /// Process numFrames of audio samples from inputs to outputs.
    void (*fProcess)(MescalineSynth* self, unsigned int numFrames, sample_t** inputs, sample_t** outputs);
    /// Get the address of the control input at index.
    float* (*fGetControlInput)(MescalineSynth* self, size_t index);
    /// Get the address of the control output at index.
    float* (*fGetControlOutput)(MescalineSynth* self, size_t index);
};
typedef struct MescalineSynth MescalineSynth;

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

typedef struct MescalineHost MescalineHost;

typedef enum
{
    kMescalineNoFlags      = 0x0
  , kMescalineBoundedBelow = 0x1
  , kMescalineBoundedAbove = 0x2
  , kMescalineTrigger	   = 0x4
} MescalineControlFlags;

typedef enum
{
    kMescalineWarpLinear
  , kMescalineWarpExponential
  , kMescalineWarpSine
  , kMescalineWarpCosine
  , kMescalineWarpDecibel
  , kMescalineWarpCurve
  , kMescalineWarpFunction
} MescalineWarpType;

typedef struct MescalineControlSpec MescalineControlSpec;

typedef struct
{
    MescalineWarpType type;
    union {
        float curve;
        struct {
            float (*fMap)(const MescalineControlSpec* spec, float value);
            float (*fUnmap)(const MescalineControlSpec* spec, float value);
        } function;
    } data;
} MescalineWarp;

struct MescalineControlSpec
{
    const char*             name;
    float                   minValue;
    float                   maxValue;
    float                   stepSize;
    float                   defaultValue;
    MescalineControlFlags   flags;
    MescalineWarp           warp;
};

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
    const MescalineControlSpec* (*fGetControlInputSpec)(const MescalineSynthDef* self, size_t index);
    const MescalineControlSpec* (*fGetControlOutputSpec)(const MescalineSynthDef* self, size_t index);

	/// Return interface description in JSON format.
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

static inline const MescalineControlSpec* MescalineSynthDefGetControlInputSpec(MescalineSynthDef* self, size_t index)
{
    return self->fGetControlInputSpec == NULL ? NULL : (*self->fGetControlInputSpec)(self, index);
}

static inline const MescalineControlSpec* MescalineSynthDefGetControlOutputSpec(MescalineSynthDef* self, size_t index)
{
    return self->fGetControlOutputSpec == NULL ? NULL : (*self->fGetControlOutputSpec)(self, index);
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
