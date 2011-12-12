#ifndef MESCALINE_API_H_INCLUDED
#define MESCALINE_API_H_INCLUDED

#include <Mescaline/Audio/Plugin/Types.h>

#include <assert.h>
#include <stddef.h>
#include <string.h>

#if defined(__cplusplus)
#   define MESCALINE_C_LINKAGE extern "C"
#else
#   define MESCALINE_C_LINKAGE
#endif

#define MESCALINE_EXPORT MESCALINE_C_LINKAGE

struct MescalineSynth
{
    /// Process numFrames of audio samples from inputs to outputs.
    void (*fProcess)(MescalineSynth* self, size_t numFrames, sample_t** inputs, sample_t** outputs);
    /// Get the address of the control input at index.
    float* (*fGetControlInput)(MescalineSynth* self, size_t index);
    /// Get the address of the control output at index.
    float* (*fGetControlOutput)(MescalineSynth* self, size_t index);
};
typedef struct MescalineSynth MescalineSynth;

static inline void MescalineSynthProcess(MescalineSynth* self, size_t numFrames, sample_t** inputs, sample_t** outputs)
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
  , kMescalineTrigger      = 0x4
} MescalineControlFlags;

typedef enum
{
    kMescalineMappingLinear
  , kMescalineMappingExponential
  , kMescalineMappingSine
  , kMescalineMappingCosine
  , kMescalineMappingDecibel
  , kMescalineMappingCurve
} MescalineMappingFunction;

typedef struct
{
    MescalineMappingFunction function;
    union {
        float curve;
    } params;
} MescalineMapping;

struct MescalineMetaData
{
    const struct MescalineMetaData* next;
    const char* key;
    const char* value;
};
typedef struct MescalineMetaData MescalineMetaData;

static inline void MescalineMetaDataInit(MescalineMetaData* metadata, const char* key, const char* value)
{
    metadata->next = NULL;
    metadata->key = key;
    metadata->value = value;
}

static inline const MescalineMetaData* MescalineMetaDataTail(const MescalineMetaData* x)
{
    return x == NULL ? NULL : x->next;
}

static inline MescalineMetaData* MescalineMetaDataCons(MescalineMetaData* x1, const MescalineMetaData* x2)
{
    assert( x1->next == NULL );
    x1->next = x2;
    return x1;
}

struct MescalineControlSpec
{
    MescalineControlFlags       flags;
    float                       minValue;
    float                       maxValue;
    float                       stepSize;
    float                       defaultValue;
    MescalineMapping            mapping;
    const MescalineMetaData*    metaData;
};
typedef struct MescalineControlSpec MescalineControlSpec;

static inline void MescalineControlSpecInit(MescalineControlSpec* self)
{
    memset(self, 0, sizeof(MescalineControlSpec));
}

enum MescalineUINodeType
{
    kMescalineUIContainer
  , kMescalineUIControl
};

struct MescalineUINode
{
    MescalineUINodeType type;
    MescalineUINode*    next;
};
typedef struct MescalineUINode MescalineUINode;

struct MescalineUIControl
{
    MescalineUINode             node;
    const MescalineControlSpec* spec;
    const char*                 type;
    const char*                 label;
};
typedef struct MescalineUIElement MescalineUIElement;

static inline void MescalineControlInit(MescalineUIControl* self, const MescalineControlSpec* spec, const char* type, const char* label)
{
    self->node.type = kMescalineUIControl;
    self->node.next = NULL;
    self->spec = spec;
    self->type = type;
    self->label = label;
}

struct MescalineUIContainer
{
    MescalineUINode     node;
    const char*         type;
    const char*         label;
    MescalineUINode*    children;
};
typedef struct MescalineUIContainer MescalineUIContainer;

static inline void MescalineContainerInit(MescalineUIContainer* self, const char* type, const char* label)
{
    self->node.type = kMescalineUIContainer;
    self->node.next = NULL;
    self->type = type;
    self->label = label;
    self->children = NULL;
}

struct MescalineSynthDef
{
    const char*                 name;

    size_t                      instanceSize;
    size_t                      instanceAlignment;

    size_t                      numAudioInputs;
    size_t                      numAudioOutputs;
    size_t                      numControlInputs;
    size_t                      numControlOutputs;

    const MescalineMetaData*    metadata;

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
    const MescalineUINode* (*fGetUIDescription)(const MescalineSynthDef* self);
};
typedef struct MescalineSynthDef MescalineSynthDef;

static inline void MescalineSynthDefInit(MescalineSynthDef* self, const char* name, size_t instanceSize, size_t instanceAlignment)
{
    memset(self, 0, sizeof(MescalineSynthDef));
    self->name = name;
    self->instanceSize = instanceSize;
    self->instanceAlignment = instanceAlignment;
}

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

static inline const MescalineUINode* MescalineSynthDefGetUIDescription(MescalineSynthDef* self)
{
    return self->fGetUIDescription == NULL ? NULL : (*self->fGetUIDescription)(self);
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
