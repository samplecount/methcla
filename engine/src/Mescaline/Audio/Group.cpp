#include <Mescaline/Audio/Engine.hpp>
#include <Mescaline/Audio/Group.hpp>
#include <boost/foreach.hpp>

using namespace Mescaline::Audio;

Group* Group::construct(Environment& env, const NodeId& id, Group* parent)
{
    return new (env) Group(env, id, parent);
}

void Group::free()
{
    Node::free<Group>(this);
}

void Group::process(size_t numFrames)
{
    // for (NodeList::iterator it = m_children.begin(); it != m_children.end(); it++)
    //     it->process(numFrames);
    BOOST_FOREACH(Node& node, m_children) { node.process(numFrames); }
}

