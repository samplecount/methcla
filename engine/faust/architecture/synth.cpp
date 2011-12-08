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

class MetaUI : public UI
{
public:
    MetaUI()
        : m_numControlInputs(0)
        , m_numControlOutputs(0)
    { }

    size_t numControlInputs() const { return m_numControlInputs; }
    size_t numControlOutputs() const { return m_numControlOutputs; }

    // -- widget's layouts

    virtual void openFrameBox(const char* label) { }
    virtual void openTabBox(const char* label) { }
    virtual void openHorizontalBox(const char* label) { }
    virtual void openVerticalBox(const char* label) { }
    virtual void closeBox() { };

    // -- active widgets

    virtual void addButton(const char* label, float* zone) { addControlInput(); }
    virtual void addToggleButton(const char* label, float* zone) { addControlInput(); }
    virtual void addCheckButton(const char* label, float* zone) { addControlInput(); }
    virtual void addVerticalSlider(const char* label, float* zone, float init, float min, float max, float step) { addControlInput(); }
    virtual void addHorizontalSlider(const char* label, float* zone, float init, float min, float max, float step) { addControlInput(); }
    virtual void addNumEntry(const char* label, float* zone, float init, float min, float max, float step) { addControlInput(); }

    // -- passive widgets

    virtual void addNumDisplay(const char* label, float* zone, int precision) { addControlOutput(); }
    virtual void addTextDisplay(const char* label, float* zone, const char* names[], float min, float max) { addControlOutput(); }
    virtual void addHorizontalBargraph(const char* label, float* zone, float min, float max) { addControlOutput(); }
    virtual void addVerticalBargraph(const char* label, float* zone, float min, float max) { addControlOutput(); }

	// -- metadata declarations

    virtual void declare(float* , const char* , const char* ) { }

protected:
    void addControlInput() { m_numControlInputs++; }
    void addControlOutput() { m_numControlOutputs++; }

private:
    size_t m_numControlInputs;
    size_t m_numControlOutputs;
};

class ControlAllocator : public UI
{
public:
    ControlAllocator(float** controlInputs, float** controlOutputs)
        : m_controlInputs(controlInputs)
        , m_controlOutputs(controlOutputs)
        , m_currentControlInput(0)
        , m_currentControlOutput(0)
    { }

    // layout widgets
    virtual void openFrameBox(const char* label) { }
    virtual void openTabBox(const char* label) { }
    virtual void openHorizontalBox(const char* label) { }
    virtual void openVerticalBox(const char* label) { }
    virtual void closeBox() { }

    // active widgets
    virtual void addButton(const char* label, float* zone)
        { addControlInput(zone); }
    virtual void addToggleButton(const char* label, float* zone)
        { addControlInput(zone); }
    virtual void addCheckButton(const char* label, float* zone)
        { addControlInput(zone); }
    virtual void addVerticalSlider(const char* label, float* zone, float init, float min, float max, float step)
        { addControlInput(zone); }
    virtual void addHorizontalSlider(const char* label, float* zone, float init, float min, float max, float step)
        { addControlInput(zone); }
    virtual void addNumEntry(const char* label, float* zone, float init, float min, float max, float step)
        { addControlInput(zone); }

    // passive widgets
    virtual void addNumDisplay(const char* label, float* zone, int precision)
        { addControlOutput(zone); }
    virtual void addTextDisplay(const char* label, float* zone, const char* names[], float min, float max)
        { addControlOutput(zone); }
    virtual void addHorizontalBargraph(const char* label, float* zone, float min, float max)
        { addControlOutput(zone); }
    virtual void addVerticalBargraph(const char* label, float* zone, float min, float max)
        { addControlOutput(zone); }

    virtual void declare(float*, const char*, const char*) { }

private:
    void addControlInput(float* zone)
    {
        m_controlInputs[m_currentControlInput] = zone;
        m_currentControlInput++;
    }
    void addControlOutput(float* zone)
    {
        m_controlOutputs[m_currentControlOutput] = zone;
        m_currentControlOutput++;
    }

private:
    float**     m_controlInputs;
    float**     m_controlOutputs;
    size_t      m_currentControlInput;
    size_t      m_currentControlOutput;
};

class MescalineFaustSynth : public MescalineSynth
{
public:
    MescalineFaustSynth(MescalineHost* host, const Plugin::SynthDef<MescalineFaustSynth>* synthDef)
        : m_controlInputs(reinterpret_cast<float**>(reinterpret_cast<char*>(this) + sizeof(MescalineFaustSynth)))
        , m_controlOutputs(m_controlInputs + synthDef->numControlInputs)
    {
        m_dsp.instanceInit(MescalineHostGetSampleRate(host));
        ControlAllocator ui(m_controlInputs, m_controlOutputs);
        m_dsp.buildUserInterface(&ui);
    }

    void process(unsigned int numFrames, sample_t** inputs, sample_t** outputs)
    {
        m_dsp.compute(numFrames, inputs, outputs);
    }

    float* controlInput(size_t index)
    {
        return m_controlInputs[index];
    }
    float* controlOutput(size_t index)
    {
        return m_controlOutputs[index];
    }

private:
    FAUSTCLASS  m_dsp;
    float**     m_controlInputs;
    float**     m_controlOutputs;
};

#define MESCALINE_TO_STRING(x) #x
#define MESCALINE_STRINGIFY(x) MESCALINE_TO_STRING(x)

class SynthDef : public Plugin::SynthDef<MescalineFaustSynth>
{
public:
    SynthDef(const char* name, dsp& dsp, const MetaUI& ui)
        : Plugin::SynthDef<MescalineFaustSynth>(
            name
          , dsp.getNumInputs()
          , dsp.getNumOutputs()
          , ui.numControlInputs()
          , ui.numControlOutputs() )
    {
        instanceSize = sizeof(MescalineFaustSynth)
                        // Reserve space for the control pointers
                        + (numControlInputs + numControlOutputs) * sizeof(float*);
        FAUSTCLASS::metadata(&m_metaData);
    }

private:
    string      m_name;
    MetaData    m_metaData;
};

MESCALINE_EXPORT void MESCALINE_INIT_FUNC(FAUSTCLASS)(MescalineHost* host)
{
    FAUSTCLASS* dsp = new FAUSTCLASS;
    MetaUI ui;
    dsp->init(MescalineHostGetSampleRate(host));
    dsp->buildUserInterface(&ui);
    MescalineSynthDef* def = new SynthDef(MESCALINE_STRINGIFY(FAUSTCLASS), *dsp, ui);
    MescalineHostRegisterSynthDef(host, def);
    delete dsp;
}
