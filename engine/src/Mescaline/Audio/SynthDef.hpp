#ifndef MESCALINE_AUDIO_SYNTHDEF_HPP_INCLUDED
#define MESCALINE_AUDIO_SYNTHDEF_HPP_INCLUDED

#include <Mescaline/API.h>
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
    
    size_t numAudioInputs    () const { return m_def-> numAudioInputs;    }
    size_t numAudioOutputs   () const { return m_def-> numAudioOutputs;   }
    size_t numControlInputs  () const { return m_def-> numControlInputs;  }
    size_t numControlOutputs () const { return m_def-> numControlOutputs; }
    
    void initialize() { (*m_def->fInitialize)(m_host, m_def); }
    void cleanup() { (*m_def->fCleanup)(m_host, m_def); }

    void construct(MescalineSynth* instance, sample_t** inputControls, sample_t** outputControls) const
    {
        memset(instance, 0, sizeof(MescalineSynth));
        (*m_def->fConstruct)(m_host, m_def, instance, inputControls, outputControls);
    }
    void destroy(MescalineSynth* instance) const
        { (*m_def->fDestroy)(m_host, m_def, instance); }
    
private:
    MescalineHost*     m_host;
    MescalineSynthDef* m_def;
};

}; };

#endif // MESCALINE_AUDIO_SYNTHDEF_HPP_INCLUDED