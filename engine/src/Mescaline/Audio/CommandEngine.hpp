//
//  CommandEngine.hpp
//  MescalineMobile
//
//  Created by Stefan Kersten on 06.03.12.
//  Copyright (c) 2012 __MyCompanyName__. All rights reserved.
//

#ifndef Mescaline_Audio_CommandEngine_hpp_included
#define Mescaline_Audio_CommandEngine_hpp_included

#include <Mescaline/Utility/Semaphore.hpp>

#include <boost/lockfree/fifo.hpp>
#include <boost/lockfree/ringbuffer.hpp>
#include <boost/thread/mutex.hpp>
#include <boost/thread/thread.hpp>

namespace Mescaline { namespace Audio {

enum Context
{
    kRealtime
  , kNonRealtime
};

template <class Command> class CommandChannel
{
public:
    CommandChannel(size_t size);

    void enqueue(Command* cmd);
    void perform();
//        size_t perform(size_t maxNumCommands);

private:
    typedef boost::lockfree::ringbuffer<Command*,0> CommandFifo;
    CommandFifo     m_fifo;
    boost::mutex    m_mutex;
};

template <class Command>
CommandChannel<Command>::CommandChannel(size_t size)
    : m_fifo(size)
{ }

template <class Command>
void CommandChannel<Command>::enqueue(Command* cmd)
{
    boost::mutex::scoped_lock(m_mutex);
    do {
        /* SPIN */
    } while (!m_fifo.enqueue(cmd));
}

//size_t CommandChannel::perform(size_t maxNumCommands)
//{
//    Command* cmd;
//    size_t n = 0;
//    while (n < maxNumCommands && m_fifo.dequeue(cmd)) {
//        cmd->perform(kRealtime);
//        n++;
//    }
//    return n;
//}

template <class Command>
void CommandChannel<Command>::perform()
{
    Command* cmd;
    while (m_fifo.dequeue(cmd)) {
        cmd->perform(kRealtime);
    }
}

template <class Command> class CommandEngine
{
public:
    CommandEngine(size_t size);
    ~CommandEngine();

    void enqueue(Context context, Command* cmd);
    void free(Context context, Command* cmd);
    void perform(Context context);

private:
    void process();
    
private:
    typedef boost::lockfree::ringbuffer<Command*,0> CommandFifo;

    bool                m_continue;
    boost::thread       m_thread;
    Utility::Semaphore  m_semaphore;
    CommandFifo         m_toNrt;
    CommandFifo         m_freeNrt;
    CommandFifo         m_toRt;
    CommandFifo         m_freeRt;
};

template <class Command>
CommandEngine<Command>::CommandEngine(size_t size)
    : m_continue(true)
    , m_toNrt(size)
    , m_freeNrt(size)
    , m_toRt(size)
    , m_freeRt(size)
{
    m_thread = boost::thread(&CommandEngine::process, this);
}

template <class Command>
CommandEngine<Command>::~CommandEngine()
{
    m_continue = false;
    m_semaphore.post();
    m_thread.join();
}

template <class Command>
void CommandEngine<Command>::enqueue(Context context, Command* cmd)
{
    switch (context) {
        case kRealtime:
            do { } while (!m_toNrt.enqueue(cmd));
            m_semaphore.post();
            break;
        case kNonRealtime:
            do { } while (!m_toRt.enqueue(cmd));
            break;
        default:
            BOOST_ASSERT_MSG( false, "Invalid execution context" );
    }
}

template <class Command>
void CommandEngine<Command>::free(Context context, Command* cmd)
{
    switch (context) {
        case kRealtime:
            if (cmd->context() == kRealtime) {
                delete cmd;
            } else {
                m_freeNrt.enqueue(cmd);
                m_semaphore.post();
            }
            break;
        case kNonRealtime:
            if (cmd->context() == kNonRealtime) {
                delete cmd;
            } else {
                m_freeRt.enqueue(cmd);
            }
            break;
        default:
            BOOST_ASSERT_MSG( false, "Invalid execution context" );
    }
}

template <class Command>
void CommandEngine<Command>::perform(Context context)
{
    Command* cmd;
    switch (context) {
        case kRealtime:
            while (m_toRt.dequeue(cmd)) {
                cmd->perform(kRealtime);
            }
            while (m_freeRt.dequeue(cmd)) {
                delete cmd;
            }
            break;
        case kNonRealtime:
            while (m_toNrt.dequeue(cmd)) {
                cmd->perform(kNonRealtime);
            }
            while (m_freeNrt.dequeue(cmd)) {
                delete cmd;
            }
            break;
        default:
            BOOST_ASSERT_MSG( false, "Invalid execution context" );
    }
}

template <class Command>
void CommandEngine<Command>::process()
{
    while (m_continue) {
        m_semaphore.wait();
        perform(kNonRealtime);
    }
}

}; };

#endif // Mescaline_Audio_CommandEngine_hpp_included
