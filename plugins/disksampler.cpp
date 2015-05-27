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

#include <methcla/plugins/disksampler.h>
#include <methcla/file.hpp>
#include <methcla/plugin.hpp>
#include <oscpp/server.hpp>

#include <cassert>
#include <atomic>

#define METHCLA_PLUGINS_DISKSAMPLER_USE_RESAMPLING 1

static const size_t kCacheLineSize = 64;
// Multiple of disk block size, more or less.
static const size_t kDiskBlockSize = 8192;
// How many frames are read at once?
// NOTE: Previously kDiskBlockSize * 4, now kDiskBlockSize * 8 in order to avoid buffer underruns while app is in background on iOS.
// TODO: Make disk buffer size configurable at runtime.
static const size_t kDiskTransferSize = kDiskBlockSize * 8;
// How many transfer blocks are in a buffer?
static const size_t kNumTransfersPerBuffer = 4;

static const size_t kMinInterpFrames = 4;

static inline size_t bytesToFrames(size_t channels, size_t bytes)
{
    return bytes / (channels * sizeof(float));
}

static inline size_t framesToBytes(size_t channels, size_t frames)
{
    return frames * channels * sizeof(float);
}

typedef enum {
    kPort_amp,
    kPort_rate,
    kPort_output_0,
    kPort_output_1,
    kNumPorts
} PortIndex;

static const size_t kNumOutputChannels = 2;

enum StateVar
{
    kInitializing,
    kIdle,
    kFilling,
    kFinishing,
    kMemoryPlayback,
    kFinished
};

static inline
size_t readAll(
    Methcla::SoundFile& file,
    float* buffer,
    size_t channels,
    size_t inNumFrames,
    bool loop,
    int64_t startFrame)
{
    size_t numFramesToRead = inNumFrames;
    size_t numFramesRead = 0;

    for (;;) {
        const size_t numFrames = file.read(buffer + channels * numFramesRead, numFramesToRead);
        numFramesToRead -= numFrames;
        numFramesRead += numFrames;
        if (!loop || numFramesToRead == 0)
            break;
        // If looping, seek back to the beginning
        file.seek(startFrame);
    }

    return numFramesRead;
}

class State
{
    std::atomic<int> m_state;

    int m_refCount;

    char m_path[FILENAME_MAX];
    bool m_loop;

    Methcla::SoundFile m_file;

    size_t m_channels;
    int64_t m_startFrame;
    int64_t m_fileFrames;

    size_t m_transferFrames;
    size_t m_bufferFrames;
    float* m_buffer;

    double m_phase;

    std::atomic<size_t> m_readPos;
    // Force read and write pointers to different cache lines.
    char m_padding[kCacheLineSize-sizeof(m_readPos)];
    std::atomic<size_t> m_writePos;

public:
    State(const char* path, bool loop, int64_t startFrame, int64_t fileFrames, size_t bufferFrames)
       : m_state(kInitializing)
       , m_refCount(1)
       , m_loop(loop)
       , m_channels(0)
       , m_startFrame(startFrame)
       , m_fileFrames(fileFrames)
       , m_transferFrames(0)
       , m_bufferFrames(bufferFrames)
       , m_buffer(nullptr)
       , m_phase(0.)
       , m_readPos(0)
       , m_writePos(0)
    {
       assert( m_state.is_lock_free() );
       assert( m_writePos.is_lock_free() );
       assert( m_readPos.is_lock_free() );

       strncpy(m_path, path, FILENAME_MAX-1);
    }

    bool isValid() const
    {
        return m_buffer != nullptr && m_file;
    }

    StateVar state() const
    {
        return static_cast<StateVar>(m_state.load(std::memory_order_consume));
    }

    bool loop() const
    {
        return m_loop;
    }

    size_t bufferChannels() const
    {
        return m_channels;
    }

    size_t bufferFrames() const
    {
        return m_bufferFrames;
    }

    size_t transferFrames() const
    {
        return m_transferFrames;
    }

    const float* buffer() const
    {
        return m_buffer;
    }

    void initBuffer(const Methcla_World* world)
    {
        performCommand(world, initBufferCallback);
    }

    inline void release(const Methcla_World* world)
    {
        assert(m_refCount > 0);
        m_refCount--;
        if (m_refCount == 0)
        {
            methcla_world_perform_command(world, destroyCallback, this);
        }
    }

    inline void finish()
    {
        m_state.store(kFinished, std::memory_order_relaxed);
    }

    void fillBuffer(const Methcla_World* world)
    {
        m_state.store(kFilling, std::memory_order_relaxed);
        performCommand(world, fillBufferCallback);
    }

    size_t readPos() const
    {
        return m_readPos.load(std::memory_order_relaxed);
    }

    void setReadPos(size_t readPos)
    {
        m_readPos.store(readPos, std::memory_order_release);
    }

    size_t writePos() const
    {
        return m_writePos.load(std::memory_order_acquire);
    }

    double phase() const
    {
        return m_phase;
    }

    void setPhase(double phase)
    {
        m_phase = phase;
    }

    inline static size_t readable(size_t w, size_t r, size_t n)
    {
        return w >= r ? w - r : w + n - r;
    }

    inline static size_t writable(size_t w, size_t r, size_t n)
    {
        return w >= r ? r - w + n - 1 : r - w - 1;
    }

private:
    ~State()
    {
        delete [] m_buffer;
    }

    static void freeCallback(const Methcla_World* world, void* data)
    {
        methcla_world_free(world, data);
    }

    static void destroyCallback(const Methcla_Host* host, void* data)
    {
        // Call destructor
        static_cast<State*>(data)->~State();
        // Free memory allocated from RT heap
        methcla_host_perform_command(host, freeCallback, data);
    }

    static void releaseCallback(const Methcla_World* world, void* data)
    {
        static_cast<State*>(data)->release(world);
    }

    // Release from host context
    void release(const Methcla_Host* host)
    {
        methcla_host_perform_command(host, releaseCallback, this);
    }

    void performCommand(const Methcla_World* world, Methcla_HostPerformFunction func)
    {
        m_refCount++;
        methcla_world_perform_command(world, func, this);
    }

    void initBuffer(const Methcla_Host* host)
    {
        try
        {
            m_file = std::move(Methcla::SoundFile(host, m_path));

            m_channels = m_file.info().channels;
            m_startFrame = std::min(std::max<int64_t>(0, m_startFrame), m_file.info().frames);
            m_fileFrames = m_fileFrames < 0
                            ? m_file.info().frames - m_startFrame
                            : std::min(m_fileFrames, m_file.info().frames - m_startFrame);

            if (m_fileFrames <= 0)
                // Force cleanup
                throw std::exception();

            // If kDiskTransferSize is smaller than audio block size, use audio block size.
            m_transferFrames = std::max(
                bytesToFrames(m_channels, kDiskTransferSize),
                // bufferFrames is audio block size initially
                m_bufferFrames);

            // Seek to start frame
            m_file.seek(m_startFrame);

            if (m_fileFrames <= (int64_t)m_transferFrames)
            {
                // If the file's number of frames is less than transferFrames,
                // read the entire contents and play back directly from memory.
                m_transferFrames = 0;
                m_bufferFrames = m_fileFrames;
                m_buffer = new float[framesToBytes(m_channels, m_bufferFrames)];

                const size_t numFrames = m_file.read(m_buffer, m_bufferFrames);
                if (numFrames != m_bufferFrames)
                    throw std::exception();

                // After having read the whole file close it right away.
                // FIXME: In order to keep latency low, maybe better do it later.
                m_file.close();
                m_state.store(kMemoryPlayback, std::memory_order_release);
            }
            else
            {
                // Load the first transferFrames into memory for streaming.
                m_bufferFrames = m_transferFrames * kNumTransfersPerBuffer;
                m_buffer = new float[framesToBytes(m_channels, m_bufferFrames)];

                const size_t numFrames = m_file.read(m_buffer, m_transferFrames);
                if (numFrames != m_transferFrames)
                    throw std::exception();

                m_writePos.store(numFrames == m_bufferFrames ? 0 : numFrames, std::memory_order_release);
                m_state.store(kIdle, std::memory_order_release);
            }
        }
        catch (std::exception)
        {
            finish();
            if (m_file)
                m_file.close();
            if (m_buffer != nullptr)
            {
                delete [] m_buffer;
                m_buffer = nullptr;
            }
        }

        release(host);
    }

    static void initBufferCallback(const Methcla_Host* host, void* data)
    {
        static_cast<State*>(data)->initBuffer(host);
    }

    inline void fillBuffer(const Methcla_Host* host)
    {
        const size_t writePos = m_writePos.load(std::memory_order_relaxed);

        assert( (m_bufferFrames % m_transferFrames) == 0 );

        try
        {
            const size_t writeSpace =
                writable(
                    writePos,
                    m_readPos.load(std::memory_order_acquire),
                    m_bufferFrames
                );

            if (writeSpace < m_transferFrames)
            {
                Methcla::Plugin::HostContext(host).log(kMethcla_LogError)
                    << METHCLA_PLUGINS_DISKSAMPLER_URI
                    << ": buffer overflow"
                    << ", need " << m_transferFrames
                    << ", got " << writeSpace
                    << ", missing " << m_transferFrames - writeSpace;
                m_state.store(kIdle, std::memory_order_release);
            }
            else
            {
                const size_t numFrames = readAll(
                    m_file,
                    m_buffer + m_channels * writePos,
                    m_channels,
                    m_transferFrames,
                    m_loop,
                    m_startFrame);

                assert( !m_loop || (numFrames == m_transferFrames) );

                const size_t nextWritePos = writePos + numFrames;
                m_writePos.store(
                    nextWritePos == m_bufferFrames ? 0 : nextWritePos,
                    std::memory_order_release
                );

                if (!m_loop && (numFrames < m_transferFrames)) {
                    m_state.store(kFinishing, std::memory_order_release);
                } else {
                    m_state.store(kIdle, std::memory_order_release);
                }
            }
        }
        catch (std::exception)
        {
            finish();
        }

        release(host);
    }

    static void fillBufferCallback(const Methcla_Host* host, void* data)
    {
        static_cast<State*>(data)->fillBuffer(host);
    }
};

struct DiskSampler
{
    float* ports[kNumPorts];
    State* state;
};

extern "C"
{
    static bool disksampler_port_descriptor(const Methcla_SynthOptions*, Methcla_PortCount, Methcla_PortDescriptor*);
    static void disksampler_configure(const void*, size_t, const void*, size_t, Methcla_SynthOptions*);
    static void disksampler_construct( const Methcla_World*, const Methcla_SynthDef*, const Methcla_SynthOptions*, Methcla_Synth*);
    static void disksampler_destroy(const Methcla_World*, Methcla_Synth*);
    static void disksampler_connect(Methcla_Synth*, Methcla_PortCount, void* data);
    static void disksampler_process(const Methcla_World*, Methcla_Synth*, size_t);
}

bool
disksampler_port_descriptor(
    const Methcla_SynthOptions* /* options */,
    Methcla_PortCount index,
    Methcla_PortDescriptor* port )
{
    switch ((PortIndex)index) {
        case kPort_amp:
        case kPort_rate:
            port->type = kMethcla_ControlPort;
            port->direction = kMethcla_Input;
            port->flags = kMethcla_PortFlags;
            return true;
        case kPort_output_0:
        case kPort_output_1:
            port->type = kMethcla_AudioPort;
            port->direction = kMethcla_Output;
            port->flags = kMethcla_PortFlags;
            return true;
        default:
            return false;
    }
}

struct DiskSamplerOptions
{
    const char* path;
    bool loop;
    size_t startFrame;
    int32_t frames;
};

void
disksampler_configure(
    const void* tags,
    size_t tags_size,
    const void* args,
    size_t args_size,
    Methcla_SynthOptions* outOptions )
{
    OSCPP::Server::ArgStream argStream(OSCPP::ReadStream(tags, tags_size), OSCPP::ReadStream(args, args_size));
    DiskSamplerOptions* options =
        static_cast<DiskSamplerOptions*>(outOptions);
    options->path = argStream.string();
    options->loop = argStream.atEnd() ? false : argStream.int32();
    options->startFrame = argStream.atEnd() ? 0 : std::max(0, argStream.int32());
    options->frames = argStream.atEnd() ? -1 : argStream.int32();
    // std::cout << "DiskSampler: "
    //           << options->path << " "
    //           << options->loop << " "
    //           << options->frames << "\n";
}

void
disksampler_construct(
    const Methcla_World* world,
    const Methcla_SynthDef* /* synthDef */,
    const Methcla_SynthOptions* inOptions,
    Methcla_Synth* synth )
{
    const DiskSamplerOptions* options =
        static_cast<const DiskSamplerOptions*>(inOptions);

    DiskSampler* self = (DiskSampler*)synth;

    self->state = static_cast<State*>(methcla_world_alloc(world, sizeof(State)));

    if (self->state != nullptr)
    {
        new (self->state) State(
            options->path,
            options->loop,
            options->startFrame,
            options->frames,
            methcla_world_block_size(world)
        );

        self->state->initBuffer(world);
    }
}

void
disksampler_destroy(const Methcla_World* world, Methcla_Synth* synth)
{
    State* state = static_cast<DiskSampler*>(synth)->state;
    if (state) state->release(world);
}

void
disksampler_connect(
    Methcla_Synth* synth,
    Methcla_PortCount index,
    void* data )
{
    ((DiskSampler*)synth)->ports[index] = (float*)data;
}

static void reportUnderrun(
    const Methcla_World* world,
    size_t numFramesNeeded,
    size_t numFramesProvided )
{
    Methcla::Plugin::World<DiskSampler>(world).log(kMethcla_LogWarn)
        << METHCLA_PLUGINS_DISKSAMPLER_URI
        << ": buffer underrun"
        << ", need " << numFramesNeeded
        << ", got " << numFramesProvided
        << ", missing " << numFramesNeeded - numFramesProvided;
}

static inline void
process_disk(
    const Methcla_World* world,
    DiskSampler* self,
    size_t numFrames,
    float amp,
    const float* buffer,
    float* out0,
    float* out1,
    StateVar state )
{
    assert( self->state->isValid() );

    const size_t bufferFrames = self->state->bufferFrames();
    const size_t writePos = self->state->writePos();
    const size_t readPos = self->state->readPos();

    if (   state == kIdle
        && State::writable(writePos, readPos, bufferFrames) >= self->state->transferFrames())
    {
        // Trigger refill
        self->state->fillBuffer(world);
    }

    const size_t readable = std::min(numFrames, State::readable(writePos, readPos, bufferFrames));
    const size_t readable1 = std::min(readable, bufferFrames - readPos);
    const size_t readable2 = readable - readable1;
    const size_t bufferChannels = self->state->bufferChannels();

    if (bufferChannels == 1)
    {
        for (size_t k = 0; k < readable1; k++)
        {
            out0[k] = out1[k] = amp * buffer[(readPos+k)*bufferChannels];
        }
        for (size_t k = 0; k < readable2; k++)
        {
            out0[readable1+k] = out1[readable1+k] = amp * buffer[k*bufferChannels];
        }
        for (size_t k = readable; k < numFrames; k++)
        {
            out0[k] = out1[k] = 0.f;
        }
    }
    else
    {
        for (size_t k = 0; k < readable1; k++)
        {
            const size_t j = (readPos+k)*bufferChannels;
            out0[k] = amp * buffer[j];
            out1[k] = amp * buffer[j+1];
        }
        for (size_t k = 0; k < readable2; k++)
        {
            const size_t m = readable1+k;
            const size_t j = k*bufferChannels;
            out0[m] = amp * buffer[j];
            out1[m] = amp * buffer[j+1];
        }
        for (size_t k = readable; k < numFrames; k++)
        {
            out0[k] = out1[k] = 0.f;
        }
    }

    if (readable < numFrames)
    {
        if (state == kFinishing)
        {
            self->state->finish();
        }
        else
        {
            reportUnderrun(world, numFrames, readable);
        }
    }

    const size_t nextReadPos = readable2 > 0 ? readable2 : readPos + readable1;
    self->state->setReadPos(nextReadPos == bufferFrames ? 0 : nextReadPos);
}

static inline void
process_memory(
    DiskSampler* self,
    size_t numFrames,
    float amp,
    const float* buffer,
    float* out0,
    float* out1 )
{
    size_t pos = self->state->readPos();
    const size_t left = self->state->bufferFrames() - pos;
    const size_t channels = self->state->bufferChannels();

    if (left >= numFrames) {
        if (channels == 1) {
            for (size_t k = 0; k < numFrames; k++) {
                out0[k] = out1[k] = amp * buffer[(pos+k)*channels];
            }
        } else {
            for (size_t k = 0; k < numFrames; k++) {
                const size_t j = (pos+k)*channels;
                out0[k] = amp * buffer[j];
                out1[k] = amp * buffer[j+1];
            }
        }
        if (left == numFrames) {
            if (self->state->loop()) self->state->setReadPos(0);
            else self->state->finish();
        } else {
            self->state->setReadPos(pos + numFrames);
        }
    } else if (self->state->loop()) {
        size_t played = 0;
        size_t toPlay = left;
        while (played < numFrames) {
            if (channels == 1) {
                for (size_t k = 0; k < toPlay; k++) {
                    const size_t m = played+k;
                    out0[m] = out1[m] = amp * buffer[(pos+k)*channels];
                }
            } else {
                for (size_t k = 0; k < toPlay; k++) {
                    const size_t m = played+k;
                    const size_t j = (pos+k)*channels;
                    out0[m] = amp * buffer[j];
                    out1[m] = amp * buffer[j+1];
                }
            }
            played += toPlay;
            pos += toPlay;
            if (pos >= self->state->bufferFrames())
                pos = 0;
            toPlay = std::min<size_t>(numFrames - played, self->state->bufferFrames());
        }
        self->state->setReadPos(pos);
    } else {
        if (channels == 1) {
            for (size_t k = 0; k < left; k++) {
                out0[k] = out1[k] = amp * buffer[(pos+k)*channels];
            }
        } else {
            for (size_t k = 0; k < left; k++) {
                const size_t j = (pos+k)*channels;
                out0[k] = amp * buffer[j];
                out1[k] = amp * buffer[j+1];
            }
        }
        for (size_t k = left; k < numFrames; k++) {
            out0[k] = out1[k] = 0.f;
        }
        self->state->finish();
    }
}

static inline float hermite1(float x, float y0, float y1, float y2, float y3)
{
    // 4-point, 3rd-order Hermite (x-form)
    const float c0 = y1;
    const float c1 = 0.5f * (y2 - y0);
    const float c2 = y0 - 2.5f * y1 + 2.f * y2 - 0.5f * y3;
    const float c3 = 1.5f * (y1 - y2) + 0.5f * (y3 - y0);

    return ((c3 * x + c2) * x + c1) * x + c0;
}

template <bool wrapInterp, bool wrapPhase>
inline size_t
resample(
    float* out0,
    float* out1,
    size_t numFrames,
    const float* buffer,
    size_t bufferChannels,
    size_t bufferFrames,
    size_t bufferEnd,
    float amp,
    float rate,
    double& phase )
{
    const size_t bufferChannel1 = 0;
    const size_t bufferChannel2 = bufferChannels > 1 ? 1 : 0;
    const double maxPhase = (double)bufferFrames;

    size_t k;

    for (k=0; k < numFrames; k++)
    {
        const double findex = std::floor(phase);
        const size_t index = (size_t)findex;

        const float* xm;
        const float* x0;
        const float* x1;
        const float* x2;

        if (index == 0)
        {
            if (wrapInterp)
                xm = buffer + (bufferFrames - 1) * bufferChannels;
            else
                xm = buffer;
        }
        else
        {
            xm = buffer + (index - 1) * bufferChannels;
        }

        if (index < bufferEnd - 2)
        {
            x0 = buffer + index * bufferChannels;
            x1 = x0 + bufferChannels;
            x2 = x1 + bufferChannels;
        }
        else if (index < bufferEnd - 1)
        {
            x0 = buffer + index * bufferChannels;
            x1 = x0 + bufferChannels;
            if (wrapInterp)
                x2 = buffer;
            else
                x2 = x1;
        }
        else if (index < bufferEnd)
        {
            x0 = buffer + index * bufferChannels;
            if (wrapInterp)
            {
                x1 = buffer;
                x2 = buffer + bufferChannels;
            }
            else
            {
                x1 = x0;
                x2 = x0;
            }
        }
        else
        {
            break;
        }

        const double x = phase - findex;

        out0[k] = amp * hermite1(x, xm[bufferChannel1], x0[bufferChannel1], x1[bufferChannel1], x2[bufferChannel1]);
        out1[k] = amp * hermite1(x, xm[bufferChannel2], x0[bufferChannel2], x1[bufferChannel2], x2[bufferChannel2]);

        phase += rate;

        if (wrapPhase && phase >= maxPhase)
            phase -= maxPhase;
    }

    return k;
}

static inline void
process_disk_interp(
    const Methcla_World* world,
    DiskSampler* self,
    size_t numFrames,
    float amp,
    float rate,
    const float* buffer,
    float* out0,
    float* out1,
    StateVar state )
{
    assert( self->state->isValid() );

    const size_t bufferFrames = self->state->bufferFrames();
    const size_t bufferChannels = self->state->bufferChannels();

    const size_t writePos = self->state->writePos();
    const size_t readPos = self->state->readPos();

    if (state == kIdle)
    {
        if (State::writable(writePos, readPos, bufferFrames)
            >= (self->state->transferFrames() + kMinInterpFrames))
        {
            // Trigger refill
            self->state->fillBuffer(world);
        }
    }

    const size_t readable  = State::readable(writePos, readPos, bufferFrames);
    const size_t readable1 = std::min(readable, bufferFrames - readPos);
    const size_t readable2 = readable - readable1;

    double phase = self->state->phase();
    using namespace std;
    assert((size_t)trunc(phase) == readPos);

    size_t numFramesProduced = 0;

    if (readable > 0)
    {
        if (readable2 > 0)
        {
            numFramesProduced = resample<true,true>(
                out0,
                out1,
                numFrames,
                buffer,
                bufferChannels,
                bufferFrames,
                bufferFrames,
                amp,
                rate,
                phase
            );
            if (numFramesProduced < numFrames)
            {
                numFramesProduced += resample<true,true>(
                    out0 + numFramesProduced,
                    out1 + numFramesProduced,
                    numFrames - numFramesProduced,
                    buffer,
                    bufferChannels,
                    bufferFrames,
                    readable2,
                    amp,
                    rate,
                    phase
                );
            }
        }
        else
        {
            numFramesProduced = resample<false,true>(
                out0,
                out1,
                numFrames,
                buffer,
                bufferChannels,
                bufferFrames,
                readPos + readable1,
                amp,
                rate,
                phase
            );
        }
    }

    if (numFramesProduced < numFrames)
    {
        for (size_t k=numFramesProduced; k < numFrames; k++)
        {
            out0[k] = out1[k] = 0.f;
        }

        if (state == kFinishing)
        {
            self->state->finish();
        }
        else
        {
            reportUnderrun(world, numFrames, numFramesProduced);
        }
    }

    assert(phase >= 0 && phase < (double)bufferFrames);
    const size_t nextReadPos = std::floor(phase);
    assert(nextReadPos < bufferFrames);
    if (nextReadPos != readPos)
        self->state->setReadPos(nextReadPos);
    self->state->setPhase(phase);
}

static inline void
process_memory_interp(
    DiskSampler* self,
    size_t numFrames,
    float amp,
    float rate,
    const float* buffer,
    float* out0,
    float* out1 )
{
    const size_t bufferFrames = self->state->bufferFrames();
    const size_t bufferChannels = self->state->bufferChannels();
    double phase = self->state->phase();

    if (self->state->loop())
    {
        size_t numFramesProduced = 0;

        while (numFramesProduced < numFrames)
        {
            numFramesProduced += resample<true,true>(
                out0 + numFramesProduced,
                out1 + numFramesProduced,
                numFrames - numFramesProduced,
                buffer,
                bufferChannels,
                bufferFrames,
                bufferFrames,
                amp,
                rate,
                phase
            );
        }

        assert(numFramesProduced == numFrames);
    }
    else
    {
        const size_t numFramesProduced = resample<false,false>(
            out0,
            out1,
            numFrames,
            buffer,
            bufferChannels,
            bufferFrames,
            bufferFrames,
            amp,
            rate,
            phase
        );

        if (numFramesProduced < numFrames)
        {
            for (size_t k = numFramesProduced; k < numFrames; k++) {
                out0[k] = out1[k] = 0.f;
            }
            self->state->finish();
        }
    }

    self->state->setPhase(phase);
}

static void
disksampler_process(
    const Methcla_World* world,
    Methcla_Synth* synth,
    size_t numFrames )
{
    DiskSampler* self = static_cast<DiskSampler*>(synth);

    const float amp = *self->ports[kPort_amp];
    const float rate = *self->ports[kPort_rate];
    float* out0 = self->ports[kPort_output_0];
    float* out1 = self->ports[kPort_output_1];
    const float* buffer = self->state->buffer();

    const StateVar state = self->state->state();

    switch (state)
    {
        case kIdle:
        case kFilling:
        case kFinishing:
#if METHCLA_PLUGINS_DISKSAMPLER_USE_RESAMPLING
        process_disk_interp(world, self, numFrames, amp, rate, buffer, out0, out1, state);
#else
        process_disk(world, self, numFrames, amp, buffer, out0, out1, state);
#endif
        break;
        case kMemoryPlayback:
#if METHCLA_PLUGINS_DISKSAMPLER_USE_RESAMPLING
        process_memory_interp(self, numFrames, amp, rate, buffer, out0, out1);
#else
        process_memory(self, numFrames, amp, buffer, out0, out1);
#endif
        break;
        case kInitializing:
        case kFinished:
        for (size_t k = 0; k < numFrames; k++) {
            out0[k] = out1[k] = 0.f;
        }
        break;
    }
}

static const Methcla_SynthDef kDiskSamplerDef =
{
    METHCLA_PLUGINS_DISKSAMPLER_URI,
    sizeof(DiskSampler),
    sizeof(DiskSamplerOptions),
    disksampler_configure,
    disksampler_port_descriptor,
    disksampler_construct,
    disksampler_connect,
    nullptr,
    disksampler_process,
    disksampler_destroy
};

static const Methcla_Library kDiskSamplerLibrary = { nullptr, nullptr };

METHCLA_EXPORT const Methcla_Library*
methcla_plugins_disksampler(
    const Methcla_Host* host,
    const char* /* bundlePath */)
{
    methcla_host_register_synthdef(host, &kDiskSamplerDef);
    return &kDiskSamplerLibrary;
}
