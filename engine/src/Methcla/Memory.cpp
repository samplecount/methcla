// #include <Methcla/Memory.hpp>
// 
// void* Methcla::Memory::alloc(size_t numBytes)
//     throw(Methcla::MemoryAllocationFailure)
// {
//     void* ptr = ::malloc(numBytes);
//     if (ptr == 0)
//         BOOST_THROW_EXCEPTION(Methcla::MemoryAllocationFailure());
//     return ptr;
// }
// 
// void Methcla::Memory::free(void* ptr) throw(Methcla::MemoryAllocationFailure)
// {
//     if (ptr != 0)
//         ::free(ptr);
// }
