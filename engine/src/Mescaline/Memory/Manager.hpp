#ifndef MESCALINE_MEMORY_MANAGER_HPP_INCLUDED
#define MESCALINE_MEMORY_MANAGER_HPP_INCLUDED

#include <Mescaline/Exception.hpp>
#include <Mescaline/Memory.hpp>

#include <boost/cstdint.hpp>
#include <boost/lockfree/fifo.hpp>

namespace Mescaline { namespace Memory {

using namespace std;

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

template <class T, class Allocator> class AllocatedBase
{
    struct Chunk
    {
        Allocator*  alloc;
        char        data[];
    };

public:
    void* operator new(size_t size, Allocator& alloc)
    {
        Chunk* chunk = static_cast<Chunk*>(alloc.malloc(sizeof(Chunk) + size));
        chunk->alloc = &alloc;
        return chunk->data;
    }
    void operator delete(void* ptr, Allocator& alloc)
    {
        AllocatedBase<T, Allocator>::free(ptr);
    }
    void operator delete(void* ptr)
    {
        AllocatedBase<T, Allocator>::free(ptr);
    }

private:
    static void free(void* ptr)
    {
        cout << "AllocatedBase::free" << endl;
        Chunk* chunk = static_cast<Chunk*>(ptr) - 1;
        BOOST_ASSERT( chunk->data == ptr );
        static_cast<T*>(ptr)->~T();
        chunk->alloc->free(chunk);
    }

    void* operator new(size_t);
};

template <class Manager> class DeferredMemoryManager
{
public:
    typedef void (*Destructor)(void* ptr);

    DeferredMemoryManager(Manager& mem)
        : m_mem(mem)
    { }

    void* malloc(size_t size)
    {
        Chunk* chunk;
        while (m_fifo.dequeue(chunk)) {
            if (chunk->destroy != 0) {
                chunk->destroy(chunk->data);
            }
            m_mem.free(chunk);
        }
        cout << "DeferredMemoryManager::malloc" << endl;
        chunk = static_cast<Chunk*>(m_mem.malloc(sizeof(Chunk) + size));
        chunk->destroy = 0;
        return chunk->data;
    }
    void free(Destructor destroy, void* ptr)
    {
        Chunk* chunk = static_cast<Chunk*>(ptr) - 1;
        chunk->destroy = destroy;
        do {
            /* SPIN */
        } while (!m_fifo.enqueue(chunk));
    }

private:
    struct Chunk
    {
        Destructor  destroy;
        char        data[];
    };

    Manager& m_mem;
    boost::lockfree::fifo<Chunk*> m_fifo;
};

template <class T, class Manager> class AllocatedDeferredBase
{
public:
    typedef DeferredMemoryManager<Manager> Allocator;

    void* operator new(size_t size, Allocator& alloc)
    {
        Chunk* chunk = static_cast<Chunk*>(alloc.malloc(sizeof(Chunk) + size));
        chunk->alloc = &alloc;
        return chunk->data;
    }
    void operator delete(void* ptr, Allocator& alloc)
    {
        AllocatedDeferredBase<T, Manager>::free(ptr);
    }

    void free()
    {
        AllocatedDeferredBase<T, Manager>::free(this);
    }

protected:
    void operator delete(void*)
    {
        BOOST_ASSERT( false );
    }

private:
    struct Chunk
    {
        Allocator* alloc;
        char       data[];
    };

    static void destroy(void* ptr)
    {
        cout << "AllocatedDeferredBase::destroy" << endl;
        Chunk* chunk = static_cast<Chunk*>(ptr);
        reinterpret_cast<T*>(chunk->data)->~T();
    }
    static void free(void* ptr)
    {
        cout << "AllocatedDeferredBase::free" << endl;
        Chunk* chunk = static_cast<Chunk*>(ptr) - 1;
        BOOST_ASSERT( chunk->data == ptr );
        chunk->alloc->free(&destroy, chunk);
    }

    void* operator new(size_t);
};

}; };

#endif // MESCALINE_MEMORY_MANAGER_HPP_INCLUDED
