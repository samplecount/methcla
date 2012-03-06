#include <Mescaline/Audio/Engine.hpp>
#include <Mescaline/Audio/Group.hpp>
#include <boost/foreach.hpp>

using namespace Mescaline::Audio;

Group* Group::construct(Environment& env, const NodeId& id, Group* parent)
{
    return new (env.rtMem()) Group(env, id, parent);
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
    BOOST_FOREACH(Node& node, m_children) { node.process(numFrames); }
}
