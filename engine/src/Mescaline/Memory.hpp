#ifndef MESCALINE_MEMORY_HPP_INCLUDED
#define MESCALINE_MEMORY_HPP_INCLUDED

#include <Mescaline/Exception.hpp>

namespace Mescaline { namespace Memory {

template <size_t kAlignment> class Alignment
{
public:
    Alignment()
        : m_mask(~(alignment() - 1))
    {
        BOOST_STATIC_ASSERT_MSG((kAlignment & (kAlignment - 1)) == 0, "alignment must be a power of two");
        BOOST_STATIC_ASSERT_MSG(kAlignment >= sizeof(void*), "alignment must be >= sizeof(void*)");
    }

    size_t alignment() const            { return kAlignment; }
    size_t align(size_t size) const     { return (size + alignment()) & m_mask; }
    size_t isAligned(size_t size) const { return (size & m_mask) == size; }

private:
    size_t m_mask;
};

static const size_t kDefaultAlignment = sizeof(void*);
/// Alignment needed for data accessed by SIMD instructions.
static const size_t kSIMDAlignment = 16;

// Primitives
inline static void* alloc(size_t numBytes) throw(MemoryAllocationFailure)
{
    void* ptr = ::malloc(numBytes);
    if (ptr == 0)
        BOOST_THROW_EXCEPTION(Mescaline::MemoryAllocationFailure());
    return ptr;
}

inline static void free(void* ptr) throw(MemoryAllocationFailure)
{
    if (ptr != 0)
        ::free(ptr);
}

template <size_t align> void* allocAligned(size_t numBytes)
    throw(MemoryAllocationFailure)
{
    void* ptr;
    int err = posix_memalign(&ptr, Alignment<align>().alignment(), numBytes);
    if (err != 0)
        BOOST_THROW_EXCEPTION(Mescaline::MemoryAllocationFailure());
    return ptr;
}

// Wrappers
template <typename T> T* allocOf(size_t numElems=1)
    throw(MemoryAllocationFailure)
{
    return static_cast<T*>(alloc(numElems * sizeof(T)));
}

template <typename T, size_t align> T* allocAlignedOf(size_t numElems=1)
    throw(MemoryAllocationFailure)
{
    return static_cast<T*>(allocAligned<align>(numElems * sizeof(T)));
}

}; };

#endif // MESCALINE_MEMORY_HPP_INCLUDED
