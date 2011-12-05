#ifndef MESCALINE_MEMORY_MANAGER_HPP_INCLUDED
#define MESCALINE_MEMORY_MANAGER_HPP_INCLUDED

#include <Mescaline/Exception.hpp>
#include <Mescaline/Memory.hpp>
#include <boost/cstdint.hpp>

namespace Mescaline { namespace Memory {

class RTMemoryManager
{
public:
    // Primitives
    void* malloc(size_t numBytes) throw(MemoryAllocationFailure)
        { return Memory::malloc(numBytes); }
    void* memalign(const Alignment& alignment, size_t numBytes) throw(MemoryAllocationFailure)
        { return Memory::memalign(alignment, numBytes); }
    void free(void* ptr) throw(MemoryAllocationFailure)
        { Memory::free(ptr); }
    
    // Wrappers
    template <typename T> T* alloc(size_t numElems=1) throw(MemoryAllocationFailure)
        { return Memory::alloc<T>(numElems); }
    template <typename T> T* allocAligned(const Alignment& alignment, size_t numElems=1) throw(MemoryAllocationFailure)
        { return Memory::allocAligned<T>(alignment, numElems); }
};

}; };

#endif // MESCALINE_MEMORY_MANAGER_HPP_INCLUDED
