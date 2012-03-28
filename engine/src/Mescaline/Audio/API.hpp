#ifndef Mescaline_Audio_API_hpp_included
#define Mescaline_Audio_API_hpp_included

#include <boost/function.hpp>
#include "lv2/lv2plug.in/ns/ext/atom/atom.h"

namespace Mescaline { namespace Audio { namespace API {

    // typedef boost::function2<void, LV2_Atom*, const LV2_Atom*> HandleResponse;
    typedef void (*HandleResponse)(void* data, LV2_Atom* request, const LV2_Atom* response);

	class Request
	{
	public:
		Request(LV2_Atom* msg, const HandleResponse& handler, void* handlerData)
			: m_request(msg)
			, m_handler(handler)
			, m_handlerData(handlerData)
		{ }
        virtual ~Request() { }

		const LV2_Atom* request()
		{
		    return m_request;
		}
		virtual void respond(const LV2_Atom* msg)
		{
		    m_handler(m_handlerData, m_request, msg);
		}

	private:
		LV2_Atom*		m_request;
        HandleResponse  m_handler;
        void*           m_handlerData;
	};

}; }; };

#endif // Mescaline_Audio_API_hpp_included
