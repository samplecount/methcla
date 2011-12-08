#include "drawbuffer.hpp"

// This determines how slowly the oscilloscope lines fade away from the display. 
// Larger numbers = slower fade (and more strain on the graphics processing)
float* drawBuffers[kNumDrawBuffers];

int drawBufferIdx = 0;
int drawBufferLen = kDefaultDrawSamples;
int drawBufferLen_alloced = 0;
