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

#include "Methcla/Audio/Client.hpp"
#include "Methcla/Utility/Semaphore.hpp"

using namespace Methcla::Audio::API::Client;

Engine::Engine(Methcla::Audio::Engine& engine)
	: m_engine(engine)
{ }

void handleResponse(LV2_Atom*& dst, Methcla::Utility::Semaphore* sem, const LV2_Atom* atom)
{
	const size_t atomSize = lv2_atom_total_size(atom);
	LV2_Atom* result = static_cast<LV2_Atom*>(Methcla::Memory::alloc(atomSize));
	memcpy(result, atom, atomSize);
	dst = result;
	sem->post();
}

LV2_Atom* Engine::request(const LV2_Atom* request)
{
	// LV2_Atom* result;
	// Methcla::Utility::Semaphore sem;
	// env().sendRequest(request, boost::bind(handleResponse, result, &sem, _1));
	// sem.wait();
	// return result;
    return 0;
}

BusMap Engine::hwAudioBuses()
{
	// LV2_Atom_Forge forge(env().atomForge());
	// LV2_Atom_Forge_Frame msg_frame;
	// LV2_Atom* msg = (LV2_Atom*)lv2_atom_forge_blank(
	// 	&forge, &msg_frame, 1, env().mapUri("fuck you"));

	// lv2_atom_forge_property_head(forge, uris->msg_body, 0);
	// LV2_Atom_Forge_Frame body_frame;
	// lv2_atom_forge_blank(forge, &body_frame, 2, 0);
	// 
	// lv2_atom_forge_property_head(forge, uris->eg_file, 0);
	// lv2_atom_forge_path(forge, (const uint8_t*)filename, filename_len);
	// 
	// lv2_atom_forge_pop(forge, &body_frame);
	// lv2_atom_forge_pop(forge, &set_frame);
	// 
	// return set;
	return BusMap();
}

