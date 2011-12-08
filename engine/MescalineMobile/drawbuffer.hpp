#ifndef DRAWBUFFER_HPP_INCLUDED
#define DRAWBUFFER_HPP_INCLUDED

#define kNumDrawBuffers 12
#define kDefaultDrawSamples 1024
#define kMinDrawSamples 64
#define kMaxDrawSamples 4096

extern int drawBufferIdx;
extern int drawBufferLen;
extern int drawBufferLen_alloced;
extern float *drawBuffers[];

#endif // DRAWBUFFER_HPP_INCLUDED

