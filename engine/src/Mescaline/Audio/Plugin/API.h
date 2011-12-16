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

typedef enum
{
    kMescalineString
  , kMescalineInt
  , kMescalineFloat
} MescalineValueType;

struct MescalineValue
{
    MescalineValueType type;
    union {
        const char* stringValue;
        int         intValue;
        float       floatValue;
    } data;
};
typedef struct MescalineValue MescalineValue;

struct MescalineAssoc
{
    struct MescalineAssoc*  next;
    const char*             key;
    MescalineValue          value;
};
typedef struct MescalineAssoc MescalineAssoc;

struct MescalineMetaData
{
    MescalineAssoc* data;
};

static inline void MescalineAssocInitString(MescalineAssoc* self, const char* key, const char* value)
{
    self->next = NULL;
    self->key = key;
    self->value.type = kMescalineString;
    self->value.data.stringValue = value;
}

static inline void MescalineAssocInitInt(MescalineAssoc* self, const char* key, int value)
{
    self->next = NULL;
    self->key = key;
    self->value.type = kMescalineInt;
    self->value.data.intValue = value;
}

static inline void MescalineAssocInitFloat(MescalineAssoc* self, const char* key, float value)
{
    self->next = NULL;
    self->key = key;
    self->value.type = kMescalineFloat;
    self->value.data.floatValue = value;
}

static inline const char* MescalineAssocGetString(MescalineAssoc* self, const char* defaultValue)
{
    return self->value.type == kMescalineString ? self->value.data.stringValue : defaultValue;
}

static inline int MescalineAssocGetInt(MescalineAssoc* self)
{
    switch (self->value.type) {
        case kMescalineString:
            return atoi(self->value.data.stringValue);
        case kMescalineInt:
            return self->value.data.intValue;
        case kMescalineFloat:
            return (int)self->value.data.floatValue;
    }
    return 0;
}

static inline int MescalineAssocGetFloat(MescalineAssoc* self)
{
    switch (self->value.type) {
        case kMescalineString:
            return atof(self->value.data.stringValue);
        case kMescalineInt:
            return (float)self->value.data.intValue;
        case kMescalineFloat:
            return self->value.data.floatValue;
    }
    return 0;
}

static inline void MescalineMetaDataInit(MescalineMetaData* self)
{
    self->data = NULL;
}

static inline void MescalineMetaDataInsert(MescalineMetaData* self, MescalineAssoc* assoc)
{
    assoc->next = self->data;
    self->data = assoc;
}

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
    kMescalineControlFlags      = 0x0
  , kMescalineControlTrigger    = 0x1
} MescalineControlFlags;

typedef struct MescalineControlSpec MescalineControlSpec;
struct MescalineControlSpec
{
    MescalineControlFlags       flags;
    const MescalineMetaData*    metaData;
};

static inline void MescalineControlSpecInit(MescalineControlSpec* self)
{
    self->flags = kMescalineControlFlags;
    self->metaData = NULL;
}

// static inline const char* MescalineControlSpecGetMetaData(MescalineControlSpec* self, const char* key)
// {
//     return self->fGetMetaData == NULL ? NULL : (*self->fGetMetaData)(self, key);
// }

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

static inline void MescalineUIControlInit(MescalineUIControl* self, const MescalineControlSpec* spec, const char* type, const char* label)
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

static inline void MescalineUIContainerInit(MescalineUIContainer* self, const char* type, const char* label)
{
    self->node.type = kMescalineUIContainer;
    self->node.next = NULL;
    self->type = type;
    self->label = label;
    self->children = NULL;
}

typedef struct MescalineSynthDef MescalineSynthDef;
struct MescalineSynthDef
{
    const char*                 name;

    size_t                      instanceSize;
    size_t                      instanceAlignment;

    size_t                      numAudioInputs;
    size_t                      numAudioOutputs;
    size_t                      numControlInputs;
    size_t                      numControlOutputs;

    const MescalineMetaData*    metaData;

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

// static inline const char* MescalineSynthDefGetMetaData(MescalineSynthDef* self, const char* key)
// {
//     return self->fGetMetaData == NULL ? NULL : (*self->fGetMetaData)(self, key);
// }

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
