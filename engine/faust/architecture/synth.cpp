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

// ==============================================================================
// Begin generated code

<<includeIntrinsic>>

<<includeclass>>

// End generated code
// ==============================================================================

class MetaData : public Meta
{
public:
    MetaData(Plugin::MetaData* metaData)
        : m_metaData(metaData)
    { }

    virtual void declare(const char* key, const char* value)
    {
        m_metaData->insert(key, value);
    }

private:
    Plugin::MetaData* m_metaData;
};

class ControlSpecAllocator : public UI
{
public:
    typedef vector<Plugin::ControlSpec*> ControlSpecVector;

    void finish()
    {
        m_controlInputSpecs.reserve(m_inputZones.size());
        for (vector<float*>::iterator zone = m_inputZones.begin(); zone != m_inputZones.end(); zone++) {
            Plugin::MetaData* metaData = 0;
            MetaDataMap::iterator it = m_metaData.find(*zone);
            if (it != m_metaData.end()) {
                metaData = it->second;
                m_metaData.erase(*zone);
            }
            const MescalineControlSpec& spec = m_controlSpecs[*zone];
            m_controlInputSpecs.push_back(
                new Plugin::ControlSpec(spec, metaData)
                );
        }

        m_controlOutputSpecs.reserve(m_outputZones.size());
        for (vector<float*>::iterator zone = m_outputZones.begin(); zone != m_outputZones.end(); zone++) {
            Plugin::MetaData* metaData = 0;
            MetaDataMap::iterator it = m_metaData.find(*zone);
            if (it != m_metaData.end()) {
                metaData = it->second;
                m_metaData.erase(*zone);
            }
            const MescalineControlSpec& spec = m_controlSpecs[*zone];
            m_controlOutputSpecs.push_back(
                new Plugin::ControlSpec(spec, metaData)
                );
        }

        BOOST_ASSERT_MSG( m_metaData.empty(), "zone metadata left over" );
    }

    const ControlSpecVector& controlInputSpecs() const { return m_controlInputSpecs; }
    const ControlSpecVector& controlOutputSpecs() const { return m_controlOutputSpecs; }

    // Layout widgets

    virtual void openFrameBox(const char* label) { }
    virtual void openTabBox(const char* label) { }
    virtual void openHorizontalBox(const char* label) { }
    virtual void openVerticalBox(const char* label) { }
    virtual void closeBox() { };

    // Active widgets

    virtual void addButton(const char* label, float* zone)
        { addInputZone(zone, 0, 1, 1, 0, kMescalineControlTrigger); }
    virtual void addToggleButton(const char* label, float* zone)
        { addInputZone(zone, 0, 1, 1, 0); }
    virtual void addCheckButton(const char* label, float* zone)
        { addInputZone(zone, 0, 1, 1, 0); }
    virtual void addVerticalSlider(const char* label, float* zone, float init, float min, float max, float step)
        { addInputZone(zone, min, max, step, init); }
    virtual void addHorizontalSlider(const char* label, float* zone, float init, float min, float max, float step)
        { addInputZone(zone, min, max, step, init); }
    virtual void addNumEntry(const char* label, float* zone, float init, float min, float max, float step)
        { addInputZone(zone, min, max, step, init); }

    // Passive widgets

    virtual void addNumDisplay(const char* label, float* zone, int precision)
        { addOutputZone(zone, -INFINITY, INFINITY, 0.f, 0.f); }
    virtual void addTextDisplay(const char* label, float* zone, const char* names[], float min, float max)
        { addOutputZone(zone, min, max, 1, min); }
    virtual void addHorizontalBargraph(const char* label, float* zone, float min, float max)
        { addOutputZone(zone, min, max, 0, min); }
    virtual void addVerticalBargraph(const char* label, float* zone, float min, float max)
        { addOutputZone(zone, min, max, 0, min); }

	// Metadata

    virtual void declare(float* zone, const char* key, const char* value)
    {
        MetaDataMap::iterator it = m_metaData.find(zone);
        if (it == m_metaData.end()) {
            Plugin::MetaData* md = new Plugin::MetaData();
            md->insert(key, value);
            m_metaData[zone] = md;
        } else {
            it->second->insert(key, value);
        }
    }

protected:
    void addInputZone(float* zone, float minValue, float maxValue, float stepSize, float defaultValue, MescalineControlFlags flags=kMescalineControlFlags)
    {
        BOOST_ASSERT_MSG( find(m_inputZones.begin(), m_inputZones.end(), zone) == m_inputZones.end(), "duplicate input zone" );
        MescalineControlSpec spec;
        MescalineControlSpecInit(&spec);
        spec.flags = flags;
        spec.minValue = minValue;
        spec.maxValue = maxValue;
        spec.stepSize = stepSize;
        spec.defaultValue = defaultValue;
        m_controlSpecs[zone] = spec;
        m_inputZones.push_back(zone);
    }

    void addOutputZone(float* zone, float minValue, float maxValue, float stepSize, float defaultValue, MescalineControlFlags flags=kMescalineControlFlags)
    {
        BOOST_ASSERT_MSG( find(m_outputZones.begin(), m_outputZones.end(), zone) == m_outputZones.end(), "duplicate output zone" );
        MescalineControlSpec spec;
        MescalineControlSpecInit(&spec);
        spec.flags = flags;
        spec.minValue = minValue;
        spec.maxValue = maxValue;
        spec.stepSize = stepSize;
        spec.defaultValue = defaultValue;
        m_controlSpecs[zone] = spec;
        m_outputZones.push_back(zone);
    }

private:
    typedef boost::unordered_map<const float*,Plugin::MetaData*> MetaDataMap;
    typedef boost::unordered_map<const float*,MescalineControlSpec> ControlSpecMap;

    vector<float*>                  m_inputZones;
    vector<float*>                  m_outputZones;
    MetaDataMap                     m_metaData;
    ControlSpecMap                  m_controlSpecs;
    vector<Plugin::ControlSpec*>    m_controlInputSpecs;
    vector<Plugin::ControlSpec*>    m_controlOutputSpecs;
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

    // Layout widgets
    virtual void openFrameBox(const char* label) { }
    virtual void openTabBox(const char* label) { }
    virtual void openHorizontalBox(const char* label) { }
    virtual void openVerticalBox(const char* label) { }
    virtual void closeBox() { }

    // Active widgets
    virtual void addButton(const char*, float* zone)
        { addControlInput(zone); }
    virtual void addToggleButton(const char*, float* zone)
        { addControlInput(zone); }
    virtual void addCheckButton(const char*, float* zone)
        { addControlInput(zone); }
    virtual void addVerticalSlider(const char*, float* zone, float, float, float, float)
        { addControlInput(zone); }
    virtual void addHorizontalSlider(const char*, float* zone, float, float, float, float)
        { addControlInput(zone); }
    virtual void addNumEntry(const char*, float* zone, float, float, float, float)
        { addControlInput(zone); }

    // Passive widgets
    virtual void addNumDisplay(const char*, float* zone, int)
        { addControlOutput(zone); }
    virtual void addTextDisplay(const char*, float* zone, const char**, float, float)
        { addControlOutput(zone); }
    virtual void addHorizontalBargraph(const char*, float* zone, float, float)
        { addControlOutput(zone); }
    virtual void addVerticalBargraph(const char*, float* zone, float, float)
        { addControlOutput(zone); }

    // Metadata
    virtual void declare(float*, const char*, const char*)
        { }

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
    SynthDef( const char* name
            , size_t numAudioInputs
            , size_t numAudioOutputs
            , const vector<Plugin::ControlSpec*>& controlInputs
            , const vector<Plugin::ControlSpec*>& controlOutputs )
        : Plugin::SynthDef<MescalineFaustSynth>(
            name
          , numAudioInputs
          , numAudioOutputs
          , controlInputs
          , controlOutputs
          , getMetaData() )
    {
        instanceSize = sizeof(MescalineFaustSynth)
                        // Reserve space for the control pointers
                        + (numControlInputs + numControlOutputs) * sizeof(float*);
    }

    static Plugin::MetaData* getMetaData()
    {
        Plugin::MetaData* metaData = new Plugin::MetaData();
        MetaData adapter(metaData);
        FAUSTCLASS::metadata(&adapter);
        return metaData;
    }

private:
    string m_name;
};

MESCALINE_EXPORT void MESCALINE_INIT_FUNC(FAUSTCLASS)(MescalineHost* host)
{
    FAUSTCLASS* dsp = new FAUSTCLASS;
    try {
        ControlSpecAllocator ui;
        dsp->init(MescalineHostGetSampleRate(host));
        dsp->buildUserInterface(&ui);
        ui.finish();
        MescalineSynthDef* def =
            new SynthDef(
                MESCALINE_STRINGIFY(FAUSTCLASS)
              , dsp->getNumInputs()
              , dsp->getNumOutputs()
              , ui.controlInputSpecs()
              , ui.controlOutputSpecs() );
        MescalineHostRegisterSynthDef(host, def);
    } catch (...) {
        delete dsp;
        throw;
    }
}
