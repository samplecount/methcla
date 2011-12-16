#ifndef MESCALINE_AUDIO_SYNTHDEF_HPP_INCLUDED
#define MESCALINE_AUDIO_SYNTHDEF_HPP_INCLUDED

#include <Mescaline/Audio/Plugin/API.h>
#include <Mescaline/Utility/Hash.hpp>

#include <boost/unordered_map.hpp>
#include <boost/utility.hpp>

#include <cstring>
#include <string>

namespace Mescaline { namespace Audio {

using namespace std;

class SynthDef : boost::noncopyable
{
public:
    SynthDef(MescalineHost* host, MescalineSynthDef* def)
        : m_host(host)
        , m_def(def)
    { }
    
    const char* name() const { return m_def->name; }

    size_t instanceSize      () const { return m_def->instanceSize;       }
    size_t instanceAlignment () const { return m_def->instanceAlignment;  }
    
    size_t numAudioInputs    () const { return m_def->numAudioInputs;    }
    size_t numAudioOutputs   () const { return m_def->numAudioOutputs;   }
    size_t numControlInputs  () const { return m_def->numControlInputs;  }
    size_t numControlOutputs () const { return m_def->numControlOutputs; }

    void initialize() { (*m_def->fInitialize)(m_host, m_def); }
    void cleanup() { (*m_def->fCleanup)(m_host, m_def); }

    void construct(MescalineSynth* instance) const
    {
        memset(instance, 0, sizeof(MescalineSynth));
        MescalineSynthDefConstruct(m_host, m_def, instance);
    }

    void destroy(MescalineSynth* instance) const
    {
        MescalineSynthDefDestroy(m_host, m_def, instance);
    }

private:
    MescalineHost*     m_host;
    MescalineSynthDef* m_def;
};

class SynthDefMap
{
public:
    void insert(const SynthDef* synthDef)
    {
        m_map[synthDef->name()] = synthDef;
    }

    const SynthDef& lookup(const char* name) const
    {
		// TODO: error handling
        return *m_map.at(name);
    }

	// TODO: Use reference count for SynthDefs and implement delete/overwrite
	// TODO: Use 4-byte aligned symbols for faster lookups

private:
    typedef boost::unordered_map<
				const char*
			  , const SynthDef*
			  , Mescaline::Utility::Hash::string_hash
			  , Mescaline::Utility::Hash::string_equal_to >
			Map;
    Map m_map;
};

}; };

#endif // MESCALINE_AUDIO_SYNTHDEF_HPP_INCLUDED