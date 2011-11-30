#include <Mescaline/Audio/Engine.h>
#include <cstdlib>

using namespace Mescaline::Audio;

void NodeMap::insert(Node* node)
{
    NodeId id = node->id();
    if (m_nodes[id] != 0)
        m_nodes[id]->free();
    m_nodes[id] = node;
}

void* NRTMemoryManager::malloc(size_t numBytes) throw(MemoryAllocationFailure)
{
    void* ptr = ::malloc(numBytes);
    if (ptr == 0)
        BOOST_THROW_EXCEPTION(MemoryAllocationFailure());
    return ptr;
}

void* NRTMemoryManager::memalign(const Alignment& align, size_t numBytes) throw(MemoryAllocationFailure)
{
    void* ptr;
    int err = posix_memalign(&ptr, align.alignment(), numBytes);
    if (err != 0)
        BOOST_THROW_EXCEPTION(MemoryAllocationFailure());
    return ptr;
}

void NRTMemoryManager::free(void* ptr) throw(MemoryAllocationFailure)
{
    if (ptr != 0)
        ::free(ptr);
}

AudioBus::AudioBus(MemoryManager& mem, size_t numFrames, uint32_t writeCount)
    : m_writeCount(writeCount)
{
    m_data = mem.allocAligned<sample_t>(MemoryManager::Alignment(16), numFrames);
}

Environment::Environment(const Options& options)
    : m_rootNode(0)
    , m_nodes(options.maxNumNodes)
    , m_blockSize(options.blockSize)
    , m_audioBuses(options.maxNumAudioBuses, 0)
{
    m_rootNode = Group::construct(*this, 0);
    m_nodes.insert(m_rootNode);
    for (size_t i=0; i < options.maxNumAudioBuses; i++) {
        m_audioBuses[i] = new AudioBus(nrtMem(), blockSize(), 0);
    }
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

Group* Group::construct(Environment& env, const NodeId& id)
{
    return new (env) Group(env, id);
}

void Group::free()
{
    Node::free<Group>(this);
}

void Group::process(size_t numFrames)
{
    for (NodeList::iterator it = m_children.begin(); it != m_children.end(); it++)
        it->process(numFrames);
}

void Synth::process(size_t numFrames)
{
    // Parallel scheduler:
    // Lock input buses in a deterministic order
    // Copy data from buses to internal buffers
    // Unlock input buses in reverse order
    
    // Sequential scheduler
    // Cache bus data pointers in internal buffer containers
    // BUT: need to take writeCount into account! Copy in any case?

    compute(numFrames, m_inputBuffers, m_outputBuffers);

    // Parallel scheduler:
    // Lock output buses in a deterministic order
    // Copy data from internal buffers to buses
    // Unlock output buses in reverse order
}

void Synth::compute(size_t numFrames, sample_t** inputs, sample_t** outputs)
{
}
