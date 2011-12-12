//-----------------------------------------------------
// name: "osc"
// version: "1.0"
// author: "Grame"
// license: "BSD"
// copyright: "(c)GRAME 2009"
//
// Code generated with Faust 0.9.44 (http://faust.grame.fr)
//-----------------------------------------------------
/* link with  */
#include <math.h>
#include <Mescaline/Faust.hpp>
#include <Mescaline/Audio/Plugin/API.hpp>

#include <boost/type_traits.hpp>
#include <boost/unordered_map.hpp>

#include <algorithm>
#include <cmath>
#include <map>
#include <string>
#include <vector>

using namespace Faust;
using namespace Mescaline::Audio;
using namespace std;

#define FAUSTFLOAT sample_t


#ifndef FAUSTFLOAT
#define FAUSTFLOAT float
#endif  

typedef long double quad;

#define FAUSTCLASS osc

class osc : public dsp {
  private:
	class SIG0 {
	  private:
		int 	fSamplingFreq;
		int 	iRec2[2];
	  public:
		int getNumInputs() 	{ return 0; }
		int getNumOutputs() 	{ return 1; }
		void init(int samplingFreq) {
			fSamplingFreq = samplingFreq;
			for (int i=0; i<2; i++) iRec2[i] = 0;
		}
		void fill (int count, float output[]) {
			// SECTION : 1
			for (int i=0; i<count; i++) {
				iRec2[0] = (1 + iRec2[1]);
				output[i] = sinf((9.587379924285257e-05f * float((iRec2[0] - 1))));
				// post processing
				iRec2[1] = iRec2[0];
			}
		}
	};


	FAUSTFLOAT 	fslider0;
	float 	fRec0_perm[4];
	FAUSTFLOAT 	fslider1;
	float 	fConst0;
	float 	fRec1_perm[4];
	static float 	ftbl0[65536];
  public:
	static void metadata(Meta* m) 	{ 
		m->declare("name", "osc");
		m->declare("version", "1.0");
		m->declare("author", "Grame");
		m->declare("license", "BSD");
		m->declare("copyright", "(c)GRAME 2009");
		m->declare("music.lib/name", "Music Library");
		m->declare("music.lib/author", "GRAME");
		m->declare("music.lib/copyright", "GRAME");
		m->declare("music.lib/version", "1.0");
		m->declare("music.lib/license", "LGPL");
		m->declare("math.lib/name", "Math Library");
		m->declare("math.lib/author", "GRAME");
		m->declare("math.lib/copyright", "GRAME");
		m->declare("math.lib/version", "1.0");
		m->declare("math.lib/license", "LGPL");
	}

	virtual int getNumInputs() 	{ return 0; }
	virtual int getNumOutputs() 	{ return 1; }
	static void classInit(int samplingFreq) {
		SIG0 sig0;
		sig0.init(samplingFreq);
		sig0.fill(65536,ftbl0);
	}
	virtual void instanceInit(int samplingFreq) {
		fSamplingFreq = samplingFreq;
		fslider0 = 0.0f;
		for (int i=0; i<4; i++) fRec0_perm[i]=0;
		fslider1 = 1e+03f;
		fConst0 = (1.0f / float(min(192000, max(1, fSamplingFreq))));
		for (int i=0; i<4; i++) fRec1_perm[i]=0;
	}
	virtual void init(int samplingFreq) {
		classInit(samplingFreq);
		instanceInit(samplingFreq);
	}
	virtual void buildUserInterface(UI* interface) {
		interface->openVerticalBox("Oscillator");
		interface->declare(&fslider1, "unit", "Hz");
		interface->addHorizontalSlider("freq", &fslider1, 1e+03f, 2e+01f, 2.4e+04f, 1.0f);
		interface->declare(&fslider0, "unit", "dB");
		interface->addHorizontalSlider("volume", &fslider0, 0.0f, -96.0f, 0.0f, 0.1f);
		interface->closeBox();
	}
	virtual void compute (int fullcount, FAUSTFLOAT** input, FAUSTFLOAT** output) {
		float 	fRec0_tmp[32+4];
		float 	fZec0[32];
		float 	fRec1_tmp[32+4];
		float 	fSlow0 = (0.0010000000000000009f * powf(10,(0.05f * fslider0)));
		float* 	fRec0 = &fRec0_tmp[4];
		float 	fSlow1 = (fConst0 * fslider1);
		float* 	fRec1 = &fRec1_tmp[4];
		int index;
		for (index = 0; index <= fullcount - 32; index += 32) {
			// compute by blocks of 32 samples
			const int count = 32;
			FAUSTFLOAT* output0 = &output[0][index];
			// SECTION : 1
			// LOOP 0x101194c80
			// pre processing
			for (int i=0; i<4; i++) fRec0_tmp[i]=fRec0_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec0[i] = (fSlow0 + (0.999f * fRec0[i-1]));
			}
			// post processing
			for (int i=0; i<4; i++) fRec0_perm[i]=fRec0_tmp[count+i];
			
			// LOOP 0x101196930
			// pre processing
			for (int i=0; i<4; i++) fRec1_tmp[i]=fRec1_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fZec0[i] = (fSlow1 + fRec1[i-1]);
				fRec1[i] = (fZec0[i] - floorf(fZec0[i]));
			}
			// post processing
			for (int i=0; i<4; i++) fRec1_perm[i]=fRec1_tmp[count+i];
			
			// SECTION : 2
			// LOOP 0x101194aa0
			// exec code
			for (int i=0; i<count; i++) {
				output0[i] = (FAUSTFLOAT)(fRec0[i] * ftbl0[int((65536.0f * fRec1[i]))]);
			}
			
		}
		if (index < fullcount) {
			// compute the remaining samples if any
			int count = fullcount-index;
			FAUSTFLOAT* output0 = &output[0][index];
			// SECTION : 1
			// LOOP 0x101194c80
			// pre processing
			for (int i=0; i<4; i++) fRec0_tmp[i]=fRec0_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fRec0[i] = (fSlow0 + (0.999f * fRec0[i-1]));
			}
			// post processing
			for (int i=0; i<4; i++) fRec0_perm[i]=fRec0_tmp[count+i];
			
			// LOOP 0x101196930
			// pre processing
			for (int i=0; i<4; i++) fRec1_tmp[i]=fRec1_perm[i];
			// exec code
			for (int i=0; i<count; i++) {
				fZec0[i] = (fSlow1 + fRec1[i-1]);
				fRec1[i] = (fZec0[i] - floorf(fZec0[i]));
			}
			// post processing
			for (int i=0; i<4; i++) fRec1_perm[i]=fRec1_tmp[count+i];
			
			// SECTION : 2
			// LOOP 0x101194aa0
			// exec code
			for (int i=0; i<count; i++) {
				output0[i] = (FAUSTFLOAT)(fRec0[i] * ftbl0[int((65536.0f * fRec1[i]))]);
			}
			
		}
	}
};


float 	osc::ftbl0[65536];

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

class ControlSpecAllocator : public UI
{
public:
    void finish()
    {
        for (MetaDataMap::iterator metaData = m_metaData.begin(); metaData != m_metaData.end(); metaData++) {
            ControlSpecMap::iterator spec = m_controlSpecs.find(metaData->first);
            if (spec == m_controlSpecs.end()) {
                // Free metadata
                delete metaData->second;
            } else {
                spec->second->setMetaData(metaData->second);
            }
        }
    }

    Plugin::ControlSpec* controlInputSpec(size_t index) const { return m_controlInputs[index]; }
    Plugin::ControlSpec* controlOutputSpec(size_t index) const { return m_controlOutputs[index]; }

    size_t numControlInputs() const { return m_controlInputs.size(); }
    size_t numControlOutputs() const { return m_controlOutputs.size(); }

    // -- widget's layouts

    virtual void openFrameBox(const char* label) { }
    virtual void openTabBox(const char* label) { }
    virtual void openHorizontalBox(const char* label) { }
    virtual void openVerticalBox(const char* label) { }
    virtual void closeBox() { };

    // -- active widgets

    virtual void addButton(const char* label, float* zone)
        { addControlInput(label, zone, 0, 1, 1, 0, kMescalineTrigger); }
    virtual void addToggleButton(const char* label, float* zone)
        { addControlInput(label, zone, 0, 1, 1, 0); }
    virtual void addCheckButton(const char* label, float* zone)
        { addControlInput(label, zone, 0, 1, 1, 0); }
    virtual void addVerticalSlider(const char* label, float* zone, float init, float min, float max, float step)
        { addControlInput(label, zone, min, max, step, init); }
    virtual void addHorizontalSlider(const char* label, float* zone, float init, float min, float max, float step)
        { addControlInput(label, zone, min, max, step, init); }
    virtual void addNumEntry(const char* label, float* zone, float init, float min, float max, float step)
        { addControlInput(label, zone, min, max, step, init); }

    // -- passive widgets

    virtual void addNumDisplay(const char* label, float* zone, int precision)
        { addControlOutput(label, zone, 0.f, 0.f, 0.f, 0.f); }
    virtual void addTextDisplay(const char* label, float* zone, const char* names[], float min, float max)
        { addControlOutput(label, zone, min, max, 1.f, min); }
    virtual void addHorizontalBargraph(const char* label, float* zone, float min, float max)
        { addControlOutput(label, zone, min, max, 0.f, min); }
    virtual void addVerticalBargraph(const char* label, float* zone, float min, float max)
        { addControlOutput(label, zone, min, max, 0.f, min); }

	// -- metadata declarations

    virtual void declare(float* zone, const char* key, const char* value)
    {
        MetaDataMap::iterator spec = m_metaData.find(zone);
        if (spec == m_metaData.end()) {
            m_metaData[zone] = new Plugin::MetaData(key, value);
        } else {
            m_metaData[zone] = new Plugin::MetaData(*spec->second, key, value);
        }
    }

protected:
    void addControlInput(const char*, float* zone, float minValue, float maxValue, float stepSize, float defaultValue, MescalineControlFlags flags=kMescalineNoFlags)
    {
        BOOST_ASSERT_MSG( m_controlSpecs.find(zone) == m_controlSpecs.end(), "duplicate zone/control spec" );
        Plugin::ControlSpec* spec = new Plugin::ControlSpec(flags, minValue, maxValue, stepSize, defaultValue);
        m_controlSpecs[zone] = spec;
        m_controlInputs.push_back(spec);
    }
    void addControlOutput(const char*, float* zone, float minValue, float maxValue, float stepSize, float defaultValue, MescalineControlFlags flags=kMescalineNoFlags)
    {
        BOOST_ASSERT_MSG( m_controlSpecs.find(zone) == m_controlSpecs.end(), "duplicate zone/control spec" );
        Plugin::ControlSpec* spec = new Plugin::ControlSpec(flags, minValue, maxValue, stepSize, defaultValue);
        m_controlSpecs[zone] = spec;
        m_controlOutputs.push_back(spec);
    }

private:
    vector<Plugin::ControlSpec*> m_controlInputs;
    vector<Plugin::ControlSpec*> m_controlOutputs;
    typedef boost::unordered_map<const float*, const Plugin::MetaData*> MetaDataMap;
    typedef boost::unordered_map<const float*, Plugin::ControlSpec*> ControlSpecMap;
    MetaDataMap m_metaData;
    ControlSpecMap m_controlSpecs;
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
    SynthDef(const char* name, dsp& dsp, const ControlSpecAllocator& ui)
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
        // Add control specs
        for (size_t i=0; i < numControlInputs; i++) {
            addControlInputSpec(ui.controlInputSpec(i));
        }
        for (size_t i=0; i < numControlOutputs; i++) {
            addControlOutputSpec(ui.controlOutputSpec(i));
        }
    }

private:
    string      m_name;
    MetaData    m_metaData;
};

MESCALINE_EXPORT void MESCALINE_INIT_FUNC(FAUSTCLASS)(MescalineHost* host)
{
    FAUSTCLASS* dsp = new FAUSTCLASS;
    ControlSpecAllocator ui;
    dsp->init(MescalineHostGetSampleRate(host));
    dsp->buildUserInterface(&ui);
    MescalineSynthDef* def = new SynthDef(MESCALINE_STRINGIFY(FAUSTCLASS), *dsp, ui);
    MescalineHostRegisterSynthDef(host, def);
    delete dsp;
}
