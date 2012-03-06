#include <Mescaline/Audio/Engine.hpp>
#include <Mescaline/Audio/Group.hpp>
#include <Mescaline/Audio/Node.hpp>
#include <Mescaline/Audio/Synth.hpp>

using namespace Mescaline::Audio;

Node::~Node()
{
    environment().releaseNodeId(this->id());
}

void Node::free()
{
    delete this;
}

void Node::operator delete(void* ptr)
{
    Memory::AllocatedBase<Node, Memory::RTMemoryManager>::destroy(ptr);
}
