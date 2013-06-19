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

#include "Methcla/Audio/IO/IcecastDriver.hpp"
#include "Methcla/Exception.hpp"
#include "Methcla/Utility/Semaphore.hpp"

#include <boost/assert.hpp>
#include <chrono>
#include <lame/lame.h>
#include <memory>
#include <shout/shout.h>
#include <stdexcept>
#include <thread>

using namespace Methcla;
using namespace Methcla::Audio::IO;
using Methcla::Audio::sample_t;

static const size_t kNumOutputs = 2;
static const size_t kMinLameBufferSize = 7200;

class detail::IcecastDriverImpl
{
public:
    typedef std::unique_ptr<shout_t,std::function<void(shout_t*)>> ShoutPtr;
    typedef std::unique_ptr<lame_global_flags,std::function<int(lame_global_flags*)>> LamePtr;

    IcecastDriverImpl(const IcecastDriver::Options& options, IcecastDriver* driver)
        : m_driver(driver)
        , m_sampleRate(options.sampleRate)
        , m_bufferSize(options.bufferSize)
        , m_outputBuffers(Driver::makeBuffers(kNumOutputs, m_bufferSize))
        , m_shout(ShoutPtr(shout_new(), shout_free))
        , m_lame(LamePtr(lame_init(), lame_close))
    {
        initEncoder(m_lame.get(), options);
        initConnection(m_shout.get(), options);
        connect();
    }

    ~IcecastDriverImpl()
    {
        stop();
        disconnect();
        Driver::freeBuffers(kNumOutputs, m_outputBuffers);
    }

    double sampleRate() const
    {
        return m_sampleRate;
    }

    size_t bufferSize() const
    {
        return m_bufferSize;
    }

    void connect()
    {
        check(m_shout.get(), shout_open(m_shout.get()));
    }

    void disconnect()
    {
        check(m_shout.get(), shout_close(m_shout.get()));
    }

    void start()
    {
        if (!m_thread.joinable()) {
            m_continue = true;
            m_thread = std::thread(&IcecastDriverImpl::run, this);
        }
    }

    void stop()
    {
        if (m_thread.joinable()) {
            m_continue = false;
            m_thread.join();
        }
    }

private:
    void run()
    {
        const size_t lameBufferSize = 1.25 * bufferSize() + kMinLameBufferSize + 0.5;
        std::unique_ptr<unsigned char[]> lameBuffer(new unsigned char[lameBufferSize]);

        BOOST_ASSERT( sizeof(sample_t) == sizeof(float) );
        BOOST_ASSERT( lameBufferSize >= kMinLameBufferSize );
  
        while (m_continue) {
            std::chrono::milliseconds delay(shout_delay(m_shout.get()));
            auto start = std::chrono::steady_clock::now();

            m_driver->process(bufferSize(), nullptr, m_outputBuffers);

            int n = lame_encode_buffer_ieee_float(
                    m_lame.get(),
                    m_outputBuffers[0],
                    m_outputBuffers[1],
                    bufferSize(),
                    lameBuffer.get(),
                    static_cast<int>(lameBufferSize)
                );
            std::cout << n << '\n';
            if (n < 0) check(m_lame.get(), n);
            BOOST_ASSERT_MSG( n > 0, "lame_encode_buffer_ieee_float returned 0" );            

            auto end = std::chrono::steady_clock::now();
            auto elapsed = end - start;

            std::this_thread::sleep_for(delay - elapsed);
            // shout_sync(m_shout.get());
            shout_send(m_shout.get(), lameBuffer.get(), n);
        }
    }

private:
    void initConnection(shout_t* self, const IcecastDriver::Options& options)
    {
        check(self, shout_set_host(self, options.host.c_str()));
        check(self, shout_set_port(self, options.port));
        check(self, shout_set_user(self, options.user.c_str()));
        check(self, shout_set_password(self, options.password.c_str()));
        check(self, shout_set_protocol(self, SHOUT_PROTOCOL_HTTP));
        check(self, shout_set_format(self, SHOUT_FORMAT_MP3));
        if (!options.mount.empty())
            check(self, shout_set_mount(self, options.mount.c_str()));
        if (!options.name.empty()) {
            check(self, shout_set_name(self, options.name.c_str()));
        }
    }

    void initEncoder(lame_global_flags* self, const IcecastDriver::Options& options)
    {
        // TODO: Verify sample rate validity
        check(self, lame_set_in_samplerate(self, (int)options.sampleRate));
        check(self, lame_set_out_samplerate(self, (int)options.sampleRate));
        check(self, lame_set_num_channels(self, kNumOutputs));
        // check(self, lame_set_VBR(self, vbr_default));
        check(self, lame_set_brate(self, 128));
        check(self, lame_set_mode(self, JOINT_STEREO));
        check(self, lame_set_force_short_blocks(self, 1));
        check(self, lame_set_asm_optimizations(self, SSE, 1));
        check(self, lame_init_params(self));
    }

    void check(shout_t* self, int code)
    {
        if (code != SHOUTERR_SUCCESS) {
            std::cerr << "libshout: " << shout_get_error(self) << "\n";
            if (code == SHOUTERR_INSANE)
                throw std::logic_error(shout_get_error(self));
            else if (code == SHOUTERR_MALLOC)
                throw std::bad_alloc();
            else
                throw std::runtime_error(shout_get_error(self));
        }
    }

    void check(lame_global_flags*, int code)
    {
        if (code < 0) {
            switch (code) {
                case LAME_NOMEM:
                    throw Error::memory();
                case LAME_GENERICERROR:
                case LAME_BADBITRATE:
                case LAME_BADSAMPFREQ:
                case LAME_INTERNALERROR:
                default:
                    throw Error::unspecified();
            }
        }
    }

    IcecastDriver*      m_driver;
    double              m_sampleRate;
    size_t              m_bufferSize;
    sample_t**          m_outputBuffers;
    ShoutPtr            m_shout;
    LamePtr             m_lame;
    std::thread         m_thread;
    std::atomic<bool>   m_continue;
};

class ShoutLibrary
{
public:
    ShoutLibrary()
    {
        shout_init();
    }
    ~ShoutLibrary()
    {
        shout_shutdown();
    }
};

IcecastDriver::IcecastDriver(const Options& options)
{
    static ShoutLibrary libshout;
    m_impl = new detail::IcecastDriverImpl(options, this);
}

IcecastDriver::~IcecastDriver()
{
    delete m_impl;
}

double IcecastDriver::sampleRate() const
{
    return m_impl->sampleRate();
}

size_t IcecastDriver::numInputs() const
{
    return 0;
}

size_t IcecastDriver::numOutputs() const
{
    return kNumOutputs;
}

size_t IcecastDriver::bufferSize() const
{
    return m_impl->bufferSize();
}


void IcecastDriver::start()
{
    m_impl->start();
}

void IcecastDriver::stop()
{
    m_impl->stop();
}

Driver* Methcla::Audio::IO::defaultPlatformDriver()
{
    IcecastDriver::Options options;
    options.password = "h3aRh3aR";
    options.mount = "wuppy";
    options.name = options.mount;
    return new IcecastDriver(options);
}
