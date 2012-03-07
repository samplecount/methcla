//
//  Semaphore.hpp
//  MescalineMobile
//
//  Created by Stefan Kersten on 27.02.12.
//  Copyright (c) 2012 __MyCompanyName__. All rights reserved.
//

#ifndef MESCALINE_UTILITY_SEMAPHORE_HPP_INCLUDED
#define MESCALINE_UTILITY_SEMAPHORE_HPP_INCLUDED

#include "zix/sem.h"

namespace Mescaline { namespace Utility {

class Semaphore : boost::noncopyable
{
public:
    Semaphore(unsigned initial=0)
    {
        zix_sem_init(&m_sem, initial);
    }
    ~Semaphore()
    {
        zix_sem_destroy(&m_sem);
    }

    void post() { zix_sem_post(&m_sem); }
    void wait() { zix_sem_wait(&m_sem); }
    void tryWait() { zix_sem_try_wait(&m_sem); }

private:
    ZixSem m_sem;
};

}; };

#endif // MESCALINE_UTILITY_SEMAPHORE_HPP_INCLUDED
