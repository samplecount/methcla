#ifndef MESCALINE_MEMORY_HPP_INCLUDED
#define MESCALINE_MEMORY_HPP_INCLUDED

#include <Mescaline/Exception.hpp>

namespace Mescaline { namespace Memory {

template <size_t alignment> class Alignment
{
public:
    static const size_t kAlignment = alignment;
    static const size_t kMask = ~(kAlignment - 1);

    BOOST_STATIC_ASSERT_MSG( (kAlignment & (kAlignment - 1)) == 0, "Alignment must be a power of two" );
    BOOST_STATIC_ASSERT_MSG( kAlignment >= sizeof(void*), "Alignment must be >= sizeof(void*)" );

#   define MESCALINE_ISALIGNED(alignment, size) \
        (((size) & Mescaline::Memory::Alignment< alignment >::kMask) == (size))
#   define MESCALINE_ALIGN(alignment, size) \
        (((size) + (alignment)) & Mescaline::Memory::Alignment< alignment >::kMask)
#   define MESCALINE_PADDING(alignment, size) \
        (MESCALINE_ALIGN(alignment, size) - (size))

    // C++0x generalized constant expressions (supported from gcc 4.6)
    /* constexpr */ inline static size_t isAligned(size_t size) { return MESCALINE_ISALIGNED(kAlignment, size); }
    /* constexpr */ inline static size_t align(size_t size)     { return MESCALINE_ALIGN(kAlignment, size); }
    /* constexpr */ inline static size_t padding(size_t size)   { return MESCALINE_PADDING(kAlignment, size); }
};

/// Default alignment, corresponding to the size of a pointer.
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
    int err = posix_memalign(&ptr, align, numBytes);
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
