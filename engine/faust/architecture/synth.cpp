// Copyright 2012-2013 Samplecount S.L.
// 
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
// 
//     http://www.apache.org/licenses/LICENSE-2.0
// 
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

#include "Methcla/Faust.hpp"
#include "Methcla/Audio/Plugin/API.hpp"

#include <boost/type_traits.hpp>

#include <algorithm>
#include <cmath>
#include <map>
#include <string>

using namespace Faust;
using namespace Methcla::Audio;
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
        for (MetaDataMap::iterator md; md != m_metaData.end(); md++) {
            ControlSpecMap::iterator spec = m_controlSpecs.find(md->first);
            if (spec == m_controlSpecs.end()) {
                delete md->second;
            } else {
                spec->second->metaData = md->second;
            }
        }
        m_metaData.clear();
    }

    const ControlSpecVector& controlInputSpecs() const { return m_controlInputSpecs; }
    const ControlSpecVector& controlOutputSpecs() const { return m_controlOutputSpecs; }

    // Layout widgets

    virtual void openFrameBox(const char* label) { }
    virtual void openTabBox(const char* label) { }
    virtual void openHorizontalBox(const char* label) { }
    virtual void openVerticalBox(const char* label) { }
    virtual void closeBox() { }

    // Active widgets

    virtual void addButton(const char*, float* zone)
    {
        Plugin::MetaData* md = addInputZone(zone, kMethclaControlTrigger);
        md->insert("min", 0.f);
        md->insert("max", 1.f);
        md->insert("step", 1.f);
        md->insert("default", 0.f);
    }
    virtual void addToggleButton(const char*, float* zone)
    {
        Plugin::MetaData* md = addInputZone(zone);
        md->insert("min", 0.f);
        md->insert("max", 1.f);
        md->insert("step", 1.f);
        md->insert("default", 0.f);
    }
    virtual void addCheckButton(const char*, float* zone)
    {
        Plugin::MetaData* md = addInputZone(zone);
        md->insert("min", 0.f);
        md->insert("max", 1.f);
        md->insert("step", 1.f);
        md->insert("default", 0.f);
    }
    virtual void addVerticalSlider(const char*, float* zone, float init, float min, float max, float step)
    {
        Plugin::MetaData* md = addInputZone(zone);
        md->insert("min", min);
        md->insert("max", max);
        md->insert("step", step);
        md->insert("default", init);
    }
    virtual void addHorizontalSlider(const char*, float* zone, float init, float min, float max, float step)
    {
        Plugin::MetaData* md = addInputZone(zone);
        md->insert("min", min);
        md->insert("max", max);
        md->insert("step", step);
        md->insert("default", init);
    }
    virtual void addNumEntry(const char*, float* zone, float init, float min, float max, float step)
    {
        Plugin::MetaData* md = addInputZone(zone);
        md->insert("min", min);
        md->insert("max", max);
        md->insert("step", step);
        md->insert("default", init);
    }

    // Passive widgets

    virtual void addNumDisplay(const char*, float* zone, int precision)
    {
        Plugin::MetaData* md = addOutputZone(zone);
        md->insert("precision", precision);
    }
    virtual void addTextDisplay(const char*, float* zone, const char* names[], float min, float max)
    {
        Plugin::MetaData* md = addOutputZone(zone);
        md->insert("min", min);
        md->insert("max", max);
    }
    virtual void addHorizontalBargraph(const char*, float* zone, float min, float max)
    {
        Plugin::MetaData* md = addOutputZone(zone);
        md->insert("min", min);
        md->insert("max", max);
    }
    virtual void addVerticalBargraph(const char*, float* zone, float min, float max)
    {
        Plugin::MetaData* md = addOutputZone(zone);
        md->insert("min", min);
        md->insert("max", max);
    }

    // Metadata

    virtual void declare(float* zone, const char* key, const char* value)
    {
        getMetaData(zone)->insert(key, value);
    }

protected:
    Plugin::MetaData* getMetaData(float* zone)
    {
        MetaDataMap::iterator it = m_metaData.find(zone);
        if (it == m_metaData.end()) {
            Plugin::MetaData* md = new Plugin::MetaData();
            m_metaData[zone] = md;
            return md;
        }
        return it->second;
    }

    Plugin::MetaData* addInputZone(float* zone, MethclaControlFlags flags=kMethclaControlFlags)
    {
        BOOST_ASSERT_MSG( m_controlSpecs.find(zone) == m_controlSpecs.end(), "duplicate input zone" );
        Plugin::ControlSpec* spec = new Plugin::ControlSpec(flags);
        m_controlInputSpecs.push_back(spec);
        m_controlSpecs[zone] = spec;
        return getMetaData(zone);
    }

    Plugin::MetaData* addOutputZone(float* zone, MethclaControlFlags flags=kMethclaControlFlags)
    {
        BOOST_ASSERT_MSG( m_controlSpecs.find(zone) == m_controlSpecs.end(), "duplicate output zone" );
        Plugin::ControlSpec* spec = new Plugin::ControlSpec(flags);
        m_controlOutputSpecs.push_back(spec);
        m_controlSpecs[zone] = spec;
        return getMetaData(zone);
    }

private:
    typedef boost::unordered_map<const float*,Plugin::MetaData*> MetaDataMap;
    typedef boost::unordered_map<const float*,Plugin::ControlSpec*> ControlSpecMap;

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

class MethclaFaustSynth : public MethclaSynth
{
public:
    MethclaFaustSynth(MethclaHost* host, const Plugin::SynthDef<MethclaFaustSynth>* synthDef)
        : m_controlInputs(reinterpret_cast<float**>(reinterpret_cast<char*>(this) + sizeof(MethclaFaustSynth)))
        , m_controlOutputs(m_controlInputs + synthDef->numControlInputs)
    {
        m_dsp.instanceInit(MethclaHostGetSampleRate(host));
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

#define METHCLA_TO_STRING(x) #x
#define METHCLA_STRINGIFY(x) METHCLA_TO_STRING(x)

class SynthDef : public Plugin::SynthDef<MethclaFaustSynth>
{
public:
    SynthDef( const char* name
            , size_t numAudioInputs
            , size_t numAudioOutputs
            , const vector<Plugin::ControlSpec*>& controlInputs
            , const vector<Plugin::ControlSpec*>& controlOutputs )
        : Plugin::SynthDef<MethclaFaustSynth>(
            name
          , numAudioInputs
          , numAudioOutputs
          , controlInputs
          , controlOutputs
          , getMetaData() )
    {
        instanceSize = sizeof(MethclaFaustSynth)
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

METHCLA_EXPORT void METHCLA_INIT_FUNC(FAUSTCLASS)(MethclaHost* host)
{
    FAUSTCLASS* dsp = new FAUSTCLASS;
    try {
        ControlSpecAllocator ui;
        dsp->init(MethclaHostGetSampleRate(host));
        dsp->buildUserInterface(&ui);
        ui.finish();
        MethclaSynthDef* def =
            new SynthDef(
                METHCLA_STRINGIFY(FAUSTCLASS)
              , dsp->getNumInputs()
              , dsp->getNumOutputs()
              , ui.controlInputSpecs()
              , ui.controlOutputSpecs() );
        MethclaHostRegisterSynthDef(host, def);
    } catch (...) {
        delete dsp;
        throw;
    }
}
