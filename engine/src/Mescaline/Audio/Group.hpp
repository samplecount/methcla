#ifndef MESCALINE_AUDIO_GROUP_HPP_INCLUDED
#define MESCALINE_AUDIO_GROUP_HPP_INCLUDED

#include <Mescaline/Audio/Node.hpp>

namespace Mescaline { namespace Audio {

class Group : public Node
{
protected:
    Group(Environment& env, const NodeId& id, Group* parent)
        : Node(env, id, parent)
    { }

public:
    enum AddAction
    {
        kAddToHead
      , kAddToTail
      , kAddBefore
      , kAddAfter
      , kReplace
    };

    static Group* construct(Environment& env, const NodeId& id, Group* target /*, AddAction addAction*/);
    virtual void free();

    const NodeList& children() const { return m_children; }
    void addToHead(Node& node) { m_children.push_front(node); }
    void addToTail(Node& node) { m_children.push_back(node); }

    virtual void process(size_t numFrames);

private:
    NodeList m_children;
};

}; };

#endif // MESCALINE_AUDIO_GROUP_HPP_INCLUDED
