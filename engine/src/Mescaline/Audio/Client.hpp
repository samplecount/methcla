#ifndef Mescaline_Audio_Client_hpp_included
#define Mescaline_Audio_Client_hpp_included

#include "Mescaline/Audio/Engine.hpp"

#include <boost/unordered_map.hpp>
#include <string>

namespace Mescaline { namespace Audio { namespace API { namespace Client {

class Bus
{
public:
	Bus(const AudioBusId& id)
		: m_id(id)
	{ }
	
	const AudioBusId& id() const { return m_id; }

private:
	AudioBusId m_id;
};

typedef boost::unordered_map<std::string,Bus> BusMap;

class Engine
{
public:
	Engine(Mescaline::Audio::Engine& engine);

	Environment& env() { return m_engine.env(); }
	LV2_Atom* request(const LV2_Atom* request);

	BusMap hwAudioBuses();

private:
	Mescaline::Audio::Engine& m_engine;
};
	
}; }; }; };

#endif // Mescaline_Audio_Client_hpp_included