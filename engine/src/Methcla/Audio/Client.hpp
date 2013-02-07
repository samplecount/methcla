#ifndef Methcla_Audio_Client_hpp_included
#define Methcla_Audio_Client_hpp_included

#include "Methcla/Audio/Engine.hpp"

#include <string>
#include <unordered_map>

namespace Methcla { namespace Audio { namespace API { namespace Client {

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

typedef std::unordered_map<std::string,Bus> BusMap;

class Engine
{
public:
	Engine(Methcla::Audio::Engine& engine);

	Environment& env() { return m_engine.env(); }
	LV2_Atom* request(const LV2_Atom* request);

	BusMap hwAudioBuses();

private:
	Methcla::Audio::Engine& m_engine;
};
	
}; }; }; };

#endif // Methcla_Audio_Client_hpp_included