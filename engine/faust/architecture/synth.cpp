#include <Mescaline/Faust.hpp>
#include <Mescaline/API.h>
#include <boost/type_traits.hpp>
#include <cmath>
#include <algorithm>
#include <map>
#include <string>

using namespace Faust;
using namespace std;

#define FAUSTFLOAT sample_t

<<includeIntrinsic>>

<<includeclass>>

class MetaData : public Meta
{
public:
    virtual void declare(const char* key, const char* value)
    {
        m_map[key] = value;
    }

    const char* lookup(const char* key) { return m_map[key]; }

private:
    map<const char*,const char*> m_map;
};

class MescalineFaustSynth : public MescalineSynth
{
public:
    MescalineFaustSynth(double sampleRate)
    {
        fProcess = &Process;
        m_dsp.instanceInit(sampleRate);
    }

private:
    static void Process(MescalineSynth* self, unsigned int numFrames, sample_t** inputs, sample_t** outputs)
    {
        static_cast<MescalineFaustSynth*>(self)->m_dsp.compute(numFrames, inputs, outputs);
    }

private:
    FAUSTCLASS m_dsp;
};

#define MESCALINE_TO_STRING(x) #x
#define MESCALINE_STRINGIFY(x) MESCALINE_TO_STRING(x)

class SynthDef : public MescalineSynthDef
{
public:
    SynthDef(dsp* dsp)
        : m_name(MESCALINE_STRINGIFY(FAUSTCLASS))
    {
        FAUSTCLASS::metadata(&m_metaData);
        name = m_name.c_str();
        instanceSize = sizeof(MescalineFaustSynth);
        instanceAlignment = boost::alignment_of<MescalineFaustSynth>();
        numAudioInputs = dsp->getNumInputs();
        numAudioOutputs = dsp->getNumOutputs();
        numControlInputs = 0;
        numControlOutputs = 0;
        fConstruct = &Construct;
        fDestroy = &Destroy;
    }

private:
    static SynthDef* cast(MescalineSynthDef* self)
        { return reinterpret_cast<SynthDef*>(self); }

    static void Construct( MescalineHost* host
                         , MescalineSynthDef* self
                         , MescalineSynth* instance
                         , sample_t** inputControls
                         , sample_t** outputControls )
    {
        new (instance) MescalineFaustSynth(MescalineHostGetSampleRate(host));
        // TODO: get control pointers
    }
    
    static void Destroy( MescalineHost* host
                       , MescalineSynthDef* self
                       , MescalineSynth* instance )
    {
        reinterpret_cast<MescalineFaustSynth*>(instance)->~MescalineFaustSynth();
    }

private:
    string      m_name;
    MetaData    m_metaData;
};

MESCALINE_DECLARE_INIT_FUNC(FAUSTCLASS)
{
    FAUSTCLASS* dsp = new FAUSTCLASS;
    dsp->classInit(MescalineHostGetSampleRate(host));
    MescalineSynthDef* def = new SynthDef(dsp);
    MescalineHostRegisterSynthDef(host, def);
    delete dsp;
}
