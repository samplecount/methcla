#include <Mescaline/Faust.hpp>
#include <Mescaline/Audio/Plugin/API.hpp>

#include <boost/type_traits.hpp>

#include <algorithm>
#include <cmath>
#include <map>
#include <string>

using namespace Faust;
using namespace Mescaline::Audio;
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
    MescalineFaustSynth(MescalineHost* host, const Plugin::SynthDef<MescalineFaustSynth>* synthDef)
    {
        m_dsp.instanceInit(MescalineHostGetSampleRate(host));
    }

    void process(unsigned int numFrames, sample_t** inputs, sample_t** outputs)
    {
        m_dsp.compute(numFrames, inputs, outputs);
    }

    sample_t* inputControl(size_t index)
    {
        return 0;
    }
    sample_t* outputControl(size_t index)
    {
        return 0;
    }

private:
    FAUSTCLASS m_dsp;
};

#define MESCALINE_TO_STRING(x) #x
#define MESCALINE_STRINGIFY(x) MESCALINE_TO_STRING(x)

class SynthDef : public Plugin::SynthDef<MescalineFaustSynth>
{
public:
    SynthDef(const char* name, dsp* dsp)
    : Plugin::SynthDef<MescalineFaustSynth>(name, dsp->getNumInputs(), dsp->getNumOutputs(), 0, 0)
    {
        FAUSTCLASS::metadata(&m_metaData);
    }

private:
    string      m_name;
    MetaData    m_metaData;
};

MESCALINE_EXPORT void MESCALINE_INIT_FUNC(FAUSTCLASS)(MescalineHost* host)
{
    FAUSTCLASS* dsp = new FAUSTCLASS;
    dsp->classInit(MescalineHostGetSampleRate(host));
    MescalineSynthDef* def = new SynthDef(MESCALINE_STRINGIFY(FAUSTCLASS), dsp);
    MescalineHostRegisterSynthDef(host, def);
    delete dsp;
}
