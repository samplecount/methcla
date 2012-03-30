#ifndef Mescaline_Audio_Client_hpp_included
#define Mescaline_Audio_Client_hpp_included

#include <Mescaline/Audio/Engine.hpp>
#include <boost/unordered_map.hpp>

namespace Mescaline { namespace Audio { namespace API { namespace Client {

class Port
{
public:
	Port(const ResourceId& id)
		: m_id(id)
	{ }
	
	const ResourceId& id() const { return m_id; }

private:
	ResourceId m_id;
};

class Engine
{
public:
	typedef boost::unordered_map<std::string,Port> PortMap;

	Engine(Mescaline::Audio::Engine& engine);

	const PortMap& hwAudioPorts() const { return m_hwAudioPorts; }

private:
	Mescaline::Audio::Engine&	m_engine;
	PortMap                     m_hwAudioPorts;
};
	
}; }; }; };

#endif // Mescaline_Audio_Client_hpp_included