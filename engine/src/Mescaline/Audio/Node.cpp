#include <Mescaline/Audio/Engine.hpp>
#include <Mescaline/Audio/Group.hpp>
#include <Mescaline/Audio/Node.hpp>
#include <Mescaline/Audio/Synth.hpp>

using namespace Mescaline::Audio;

void* Node::operator new(size_t, void* where)
{
    return where;
}

void Node::operator delete(void*)
{
}

void* Node::operator new(size_t numBytes, Environment& env)
{
    return env.rtMem().malloc(numBytes);
}

void Node::operator delete(void* ptr, Environment& env)
{
    env.rtMem().free(ptr);
}

template <class T> void Node::free(T* node)
{
    Environment& env = node->environment();
    node->~T();
    env.rtMem().free(node);
}
        
template void Node::free<Group>(Group* node);
template void Node::free<Synth>(Synth* node);
