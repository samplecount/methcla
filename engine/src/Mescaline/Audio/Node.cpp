#include <Mescaline/Audio/Engine.hpp>
#include <Mescaline/Audio/Group.hpp>
#include <Mescaline/Audio/Node.hpp>
#include <Mescaline/Audio/Synth.hpp>

using namespace Mescaline::Audio;

Node::~Node()
{
    // TODO: This needs to be different for different resources because removeResource is not realtime safe!
//    environment().removeResource(*this);
}

void Node::free()
{
    delete this;
}

void Node::operator delete(void* ptr)
{
    Memory::AllocatedBase<Node, Memory::RTMemoryManager>::destroy(ptr);
}
