#ifndef Mescaline_Audio_API_hpp_included
#define Mescaline_Audio_API_hpp_included

#include <boost/function.hpp>
#include "lv2/lv2plug.in/ns/ext/atom/atom.h"

namespace Mescaline { namespace Audio { namespace API {

    typedef boost::function1<void, const LV2_Atom*> ResponseHandler;

	class Request
	{
	public:
		Request(LV2_Atom* atom, const ResponseHandler& handler)
			: m_atom(atom)
			, m_responseHandler(handler)
		{ }

		const LV2_Atom* request() { return m_atom; }
		void respond(const LV2_Atom* atom) { m_responseHandler(atom); }

	private:
		LV2_Atom*		m_atom;
	    ResponseHandler m_responseHandler;
	};

}; }; };

#endif // Mescaline_Audio_API_hpp_included
