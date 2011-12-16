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
        Plugin::MetaData* md = addInputZone(zone, kMescalineControlTrigger);
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

    Plugin::MetaData* addInputZone(float* zone, MescalineControlFlags flags=kMescalineControlFlags)
    {
        BOOST_ASSERT_MSG( m_controlSpecs.find(zone) == m_controlSpecs.end(), "duplicate input zone" );
        Plugin::ControlSpec* spec = new Plugin::ControlSpec(flags);
        m_controlInputSpecs.push_back(spec);
        m_controlSpecs[zone] = spec;
        return getMetaData(zone);
    }

    Plugin::MetaData* addOutputZone(float* zone, MescalineControlFlags flags=kMescalineControlFlags)
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
