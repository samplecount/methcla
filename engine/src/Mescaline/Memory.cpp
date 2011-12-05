#include <Mescaline/Memory.hpp>

void* Mescaline::Memory::malloc(size_t numBytes) throw(Mescaline::MemoryAllocationFailure)
{
    void* ptr = ::malloc(numBytes);
    if (ptr == 0)
        BOOST_THROW_EXCEPTION(Mescaline::MemoryAllocationFailure());
    return ptr;
}

void* Mescaline::Memory::memalign(const Alignment& align, size_t numBytes) throw(Mescaline::MemoryAllocationFailure)
{
    void* ptr;
    int err = posix_memalign(&ptr, align.alignment(), numBytes);
    if (err != 0)
        BOOST_THROW_EXCEPTION(Mescaline::MemoryAllocationFailure());
    return ptr;
}

void Mescaline::Memory::free(void* ptr) throw(Mescaline::MemoryAllocationFailure)
{
    if (ptr != 0)
        ::free(ptr);
}
