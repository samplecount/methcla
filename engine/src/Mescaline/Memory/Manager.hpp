#ifndef MESCALINE_MEMORY_MANAGER_HPP_INCLUDED
#define MESCALINE_MEMORY_MANAGER_HPP_INCLUDED

#include <Mescaline/Exception.hpp>
#include <Mescaline/Memory.hpp>

#include <boost/cstdint.hpp>
//#include <boost/lockfree/fifo.hpp>

namespace Mescaline { namespace Memory {

class RTMemoryManager
{
public:
    // Primitives
    void* alloc(size_t numBytes)
        throw(MemoryAllocationFailure)
        { return Memory::alloc(numBytes); }
    template <size_t align> void* allocAligned(size_t numBytes)
        throw(MemoryAllocationFailure)
        { return Memory::allocAligned<align>(numBytes); }
    void free(void* ptr)
        throw(MemoryAllocationFailure)
        { Memory::free(ptr); }
    
    // Wrappers
    template <typename T> T* allocOf(size_t numElems=1)
        throw(MemoryAllocationFailure)
        { return Memory::allocOf<T>(numElems); }
    template <typename T, size_t align> T* allocAlignedOf(size_t numElems=1)
        throw(MemoryAllocationFailure)
        { return Memory::allocAligned<T,align>(numElems); }
};

template <class T, class Allocator, size_t align=kDefaultAlignment> class AllocatedBase
{
    struct Chunk
    {
        Allocator*  alloc;
        char        padding[Alignment<align>::padding(sizeof(alloc))];
        char        data[];
    };

protected:
    static const size_t kAlignment = align;

    static void* alloc(Allocator& allocator, size_t size)
    {
        Chunk* chunk = static_cast<Chunk*>(allocator.template allocAligned<align>(sizeof(Chunk) + size));
        chunk->alloc = &allocator;
        BOOST_ASSERT( Alignment<align>::isAligned(reinterpret_cast<size_t>(chunk->data)) );
        return chunk->data;
    }
    static void destroy(void* ptr)
    {
        Chunk* chunk = static_cast<Chunk*>(ptr) - 1;
        BOOST_ASSERT( chunk->data == ptr );
        static_cast<T*>(ptr)->~T();
        chunk->alloc->free(chunk);
    }

    void* operator new(size_t);
};

template <class T, class Allocator, size_t align=kDefaultAlignment>class Allocated
    : public AllocatedBase<T, Allocator, align>
{
    typedef AllocatedBase<T, Allocator, align> super;

public:
    void* operator new(size_t size, Allocator& alloc)
    {
        return super::alloc(alloc, size);
    }
    void operator delete(void* ptr, Allocator& alloc)
    {
        super::destroy(ptr);
    }
    void* operator new(size_t size, Allocator& alloc, size_t additional)
    {
        return super::alloc(alloc, size+additional);
    }
    void operator delete(void* ptr, Allocator& alloc, size_t realSize)
    {
        super::destroy(ptr);
    }
    void operator delete(void* ptr)
    {
        super::destroy(ptr);
    }
};

// template <class Manager> class DeferredMemoryManager
// {
// public:
//     typedef void (*Destructor)(void* ptr);
// 
//     DeferredMemoryManager(Manager& mem)
//         : m_mem(mem)
//     { }
// 
//     void* malloc(size_t size)
//     {
//         Chunk* chunk;
//         while (m_fifo.dequeue(chunk)) {
//             if (chunk->destroy != 0) {
//                 chunk->destroy(chunk->data);
//             }
//             m_mem.free(chunk);
//         }
//         cout << "DeferredMemoryManager::malloc" << endl;
//         chunk = static_cast<Chunk*>(m_mem.malloc(sizeof(Chunk) + size));
//         chunk->destroy = 0;
//         return chunk->data;
//     }
//     void free(Destructor destroy, void* ptr)
//     {
//         Chunk* chunk = static_cast<Chunk*>(ptr) - 1;
//         chunk->destroy = destroy;
//         do {
//             /* SPIN */
//         } while (!m_fifo.enqueue(chunk));
//     }
// 
// private:
//     struct Chunk
//     {
//         Destructor  destroy;
//         char        data[];
//     };
// 
//     Manager& m_mem;
//     boost::lockfree::fifo<Chunk*> m_fifo;
// };
// 
// template <class T, class Manager> class AllocatedDeferredBase
// {
// public:
//     typedef DeferredMemoryManager<Manager> Allocator;
// 
//     void* operator new(size_t size, Allocator& alloc)
//     {
//         Chunk* chunk = static_cast<Chunk*>(alloc.malloc(sizeof(Chunk) + size));
//         chunk->alloc = &alloc;
//         return chunk->data;
//     }
//     void operator delete(void* ptr, Allocator& alloc)
//     {
//         AllocatedDeferredBase<T, Manager>::free(ptr);
//     }
// 
//     void free()
//     {
//         AllocatedDeferredBase<T, Manager>::free(this);
//     }
// 
// protected:
//     void operator delete(void*)
//     {
//         BOOST_ASSERT( false );
//     }
// 
// private:
//     struct Chunk
//     {
//         Allocator* alloc;
//         char       data[];
//     };
// 
//     static void destroy(void* ptr)
//     {
//         cout << "AllocatedDeferredBase::destroy" << endl;
//         Chunk* chunk = static_cast<Chunk*>(ptr);
//         reinterpret_cast<T*>(chunk->data)->~T();
//     }
//     static void free(void* ptr)
//     {
//         cout << "AllocatedDeferredBase::free" << endl;
//         Chunk* chunk = static_cast<Chunk*>(ptr) - 1;
//         BOOST_ASSERT( chunk->data == ptr );
//         chunk->alloc->free(&destroy, chunk);
//     }
// 
//     void* operator new(size_t);
// };

}; };

#endif // MESCALINE_MEMORY_MANAGER_HPP_INCLUDED
