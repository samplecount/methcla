#ifndef MESCALINE_MEMORY_HPP_INCLUDED
#define MESCALINE_MEMORY_HPP_INCLUDED

#include <Mescaline/Exception.hpp>

namespace Mescaline { namespace Memory {

class Alignment
{
public:
    explicit Alignment(size_t alignment)
        : m_alignment(alignment)
        , m_mask(~(alignment - 1))
    {
        BOOST_ASSERT_MSG((alignment & (alignment - 1)) == 0, "alignment must be a power of two");
        BOOST_ASSERT_MSG(alignment >= sizeof(void*), "alignment must be >= sizeof(void*)");
    }

    size_t alignment() const            { return m_alignment; }
    size_t align(size_t size) const     { return (size + m_alignment) & m_mask; }
    size_t isAligned(size_t size) const { return (size & m_mask) == size; }

    /// Return the alignment needed for data accessed by SIMD instructions.
    static Alignment SIMDAlignment() { return Alignment(16); }

private:
    size_t m_alignment;
    size_t m_mask;
};

// Primitives
void* malloc(size_t numBytes) throw(MemoryAllocationFailure);
void* memalign(const Alignment& alignment, size_t numBytes) throw(MemoryAllocationFailure);
void free(void* ptr) throw(MemoryAllocationFailure);

// Wrappers
template <typename T> T* alloc(size_t numElems=1) throw(MemoryAllocationFailure)
{
    return static_cast<T*>(malloc(numElems * sizeof(T)));
}

template <typename T> T* allocAligned(const Alignment& alignment, size_t numElems=1) throw(MemoryAllocationFailure)
{
    return static_cast<T*>(memalign(alignment, numElems * sizeof(T)));
}

}; };

#endif // MESCALINE_MEMORY_HPP_INCLUDED
