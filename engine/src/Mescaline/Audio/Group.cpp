#include <Mescaline/Audio/Engine.hpp>
#include <Mescaline/Audio/Group.hpp>

using namespace Mescaline::Audio;

Group* Group::construct(Environment& env, Group* target, Node::AddAction addAction)
{
    return new (env.rtMem()) Group(env, target, addAction);
}

void Group::free()
{
    if (isRootNode()) {
        BOOST_THROW_EXCEPTION(
            InvalidNodeId()
         << ErrorInfoNodeId(id())
         << ErrorInfoString("cannot free root node")
         );
    } else {
        Node::free();
    }
}

void Group::process(size_t numFrames)
{
    for (Node& node : m_children) { node.process(numFrames); }
}
