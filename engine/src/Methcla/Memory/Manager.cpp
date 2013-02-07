#include <Methcla/Memory/Manager.hpp>
#include <cstdlib>

//using namespace Methcla::Memory;
//
//void* RTMemoryManager::malloc(size_t numBytes) throw(MemoryAllocationFailure)
//{
//    void* ptr = ::malloc(numBytes);
//    if (ptr == 0)
//        BOOST_THROW_EXCEPTION(MemoryAllocationFailure());
//    return ptr;
//}
//
//void* RTMemoryManager::memalign(const Alignment& align, size_t numBytes) throw(MemoryAllocationFailure)
//{
//    void* ptr;
//    int err = posix_memalign(&ptr, align.alignment(), numBytes);
//    if (err != 0)
//        BOOST_THROW_EXCEPTION(MemoryAllocationFailure());
//    return ptr;
//}
//
//void RTMemoryManager::free(void* ptr) throw(MemoryAllocationFailure)
//{
//    if (ptr != 0)
//        ::free(ptr);
//}
