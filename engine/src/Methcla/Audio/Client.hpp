// Copyright 2012-2013 Samplecount S.L.
// 
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
// 
//     http://www.apache.org/licenses/LICENSE-2.0
// 
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

#ifndef METHCLA_AUDIO_CLIENT_HPP_INCLUDED
#define METHCLA_AUDIO_CLIENT_HPP_INCLUDED

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

#endif // METHCLA_AUDIO_CLIENT_HPP_INCLUDED