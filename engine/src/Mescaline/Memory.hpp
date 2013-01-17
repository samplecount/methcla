#ifndef MESCALINE_MEMORY_HPP_INCLUDED
#define MESCALINE_MEMORY_HPP_INCLUDED

#include <Mescaline/Exception.hpp>
#include <memory>
#include <stdexcept>

namespace Mescaline { namespace Memory {

template <size_t alignment> class Alignment
{
public:
    static const size_t kAlignment = alignment;
    static const size_t kMask = ~(kAlignment - 1);

    static_assert( (kAlignment & (kAlignment - 1)) == 0, "Alignment must be a power of two" );
    static_assert( kAlignment >= sizeof(nullptr), "Alignment must be >= sizeof(nullptr)" );

    constexpr inline static size_t isAligned(size_t size)
    {
        return (size & kMask) == size;
    }
    constexpr inline static size_t align(size_t size)
    {
        return (size + alignment) & kMask;
    }
    constexpr inline static size_t padding(size_t size)
    {
        return align(size) - size;
    }
};

/// Default alignment, corresponding to the size of a pointer.
static const size_t kDefaultAlignment = sizeof(nullptr);
/// Alignment needed for data accessed by SIMD instructions.
static const size_t kSIMDAlignment = 16;

// Primitives
inline static void* alloc(size_t size)
    throw(std::invalid_argument, std::bad_alloc)
{
    if (size == 0)
        BOOST_THROW_EXCEPTION(std::invalid_argument("size must be greater than zero"));
    void* ptr = ::malloc(size);
    if (ptr == nullptr)
        BOOST_THROW_EXCEPTION(std::bad_alloc());
    return ptr;
}

inline static void free(void* ptr)
{
    if (ptr != nullptr)
        ::free(ptr);
}

template <size_t align> void* allocAligned(size_t size)
    throw(std::invalid_argument, std::bad_alloc)
{
    if (size == 0)
        BOOST_THROW_EXCEPTION(std::invalid_argument("size must be greater than zero"));
    void* ptr;
    int err = posix_memalign(&ptr, align, size);
    if (err != 0)
        BOOST_THROW_EXCEPTION(std::bad_alloc());
    return ptr;
}

// Wrappers
template <typename T> T* allocOf(size_t n=1)
    throw(std::invalid_argument, std::bad_alloc)
{
    return static_cast<T*>(alloc(n * sizeof(T)));
}

template <typename T, size_t align=alignof(T)> T* allocAlignedOf(size_t n=1)
    throw(std::invalid_argument, std::bad_alloc)
{
    return static_cast<T*>(allocAligned<align>(n * sizeof(T)));
}

}; };

#endif // MESCALINE_MEMORY_HPP_INCLUDED
