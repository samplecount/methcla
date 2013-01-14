#include <Mescaline/Audio/Engine.hpp>
#include <Mescaline/Audio/Group.hpp>
#include <Mescaline/Audio/Node.hpp>
#include <Mescaline/Audio/Synth.hpp>

using namespace Mescaline::Audio;

Node::Node(Environment& env, Group* target, AddAction addAction)
    : Resource(env, env.nodes().nextId())
{
	env.nodes().insert(id(), this);
	if (target == nullptr) {
		m_parent = nullptr;
	} else {
		switch (addAction) {
			case kAddToHead:
				m_parent = target;
				target->addToHead(*this);
				break;
			case kAddToTail:
				m_parent = target;
				target->addToTail(*this);
				break;
		}
	}
}

Node::~Node()
{
	env().nodes().remove(id());
}

void Node::free()
{
	delete this;
}
