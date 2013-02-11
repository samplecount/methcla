// Copyright 2012-2013 Samplecount S.L.
// 
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
// 
//     http://www.apache.org/licenses/LICENSE-2.0
// 
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

#include "Methcla/Memory/Manager.hpp"
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
