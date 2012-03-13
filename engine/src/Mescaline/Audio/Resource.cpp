#include <Mescaline/Audio/Resource.hpp>

using namespace Mescaline::Audio;

void Mescaline::Audio::intrusive_ptr_add_ref(Resource* x)
{
    x->retain();
}

void Mescaline::Audio::intrusive_ptr_release(Resource* x)
{
    x->release();
}

void Resource::handleRequest(const API::Request&)
{
}

void Resource::free()
{
    delete this;
}

