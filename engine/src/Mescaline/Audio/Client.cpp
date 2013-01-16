#include <Mescaline/Audio/Client.hpp>
#include "Mescaline/Utility/Semaphore.hpp"

using namespace Mescaline::Audio::API::Client;

Engine::Engine(Mescaline::Audio::Engine& engine)
	: m_engine(engine)
{ }

void handleResponse(LV2_Atom*& dst, Mescaline::Utility::Semaphore* sem, const LV2_Atom* atom)
{
	const size_t atomSize = lv2_atom_total_size(atom);
	LV2_Atom* result = static_cast<LV2_Atom*>(Mescaline::Memory::alloc(atomSize));
	memcpy(result, atom, atomSize);
	dst = result;
	sem->post();
}

LV2_Atom* Engine::request(const LV2_Atom* request)
{
	// LV2_Atom* result;
	// Mescaline::Utility::Semaphore sem;
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

