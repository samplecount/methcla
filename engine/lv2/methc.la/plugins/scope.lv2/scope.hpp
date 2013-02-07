#ifndef METHCLA_AUDIO_SCOPE_HPP_INCLUDED
#define METHCLA_AUDIO_SCOPE_HPP_INCLUDED

#include <boost/lockfree/ringbuffer.hpp>

namespace Methcla { namespace LV2
{
    struct Scope
    {
    public:
        typedef boost::lockfree::ringbuffer<sample_t,1024> Buffer;

        Buffer& buffer() { return m_buffer; }

    private:
        Buffer m_buffer;
    };
}; };

#endif // MESCALNE_AUDIO_SCOPE_HPP_INCLUDED