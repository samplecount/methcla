#ifndef MESCALINE_API_H_INCLUDED
#define MESCALINE_API_H_INCLUDED

#if defined(__cplusplus)
#   define MESCALINE_C_LINKAGE extern "C"
#else
#   define MESCALINE_C_LINKAGE
#endif

#define MESCALINE_EXPORT MESCALINE_C_LINKAGE

typedef float sample_t;

typedef struct MescalineHost MescalineHost;

struct MescalineSynth
{
    void (*fProcess)(MescalineSynth* self, unsigned int numFrames, sample_t** inputs, sample_t** outputs);
};
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
    void            (*fInitialize)(MescalineHost* host, MescalineSynthDef* self);
    void            (*fCleanup)(MescalineHost* host, MescalineSynthDef* self);

    // Instance creation/destruction
    void            (*fConstruct)( MescalineHost* host
                                 , MescalineSynthDef* self
                                 , MescalineSynth* instance
                                 , sample_t** inputControls
                                 , sample_t** outputControls );
    void            (*fDestroy)( MescalineHost* host
                               , MescalineSynthDef* self
                               , MescalineSynth* instance );

    // Querying
    const char*     (*fGetControlInputName)(MescalineSynthDef* self, unsigned int index);
    const char*     (*fGetControlOutputName)(MescalineSynthDef* self, unsigned int index);

    const char*     (*fGetDescription)(MescalineSynthDef* self);    
};
typedef struct MescalineSynthDef MescalineSynthDef;

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

#define MESCALINE_MAKE_INIT_FUNC__PRIVATE(name) MescalineInit_##name
#define MESCALINE_DECLARE_INIT_FUNC(name) \
    MESCALINE_EXPORT void MESCALINE_MAKE_INIT_FUNC__PRIVATE(name) (MescalineHost* host)
#define MESCALINE_INIT_FUNC(name) MESCALINE_MAKE_INIT_FUNC__PRIVATE(name)

#endif /* MESCALINE_API_H_INCLUDED */
