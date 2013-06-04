/*
 * Copyright 2012 Peter Brinkmann (peter.brinkmann@gmail.com)
 *
 * Based on sample code by Victor Lazzarini, available at
 * http://audioprograming.wordpress.com/2012/03/03/android-audio-streaming-with-opensl-es-and-the-ndk/
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#include "opensl_io.h"

#include <android/log.h>
#include <semaphore.h>
#include <stdlib.h>
#include <string.h>
#include <sys/resource.h>
#include <time.h>
#include <unistd.h>

#include <SLES/OpenSLES.h>
#include <SLES/OpenSLES_Android.h>

#define LOGI(...) \
  __android_log_print(ANDROID_LOG_INFO, "opensl_io", __VA_ARGS__)
#define LOGW(...) \
  __android_log_print(ANDROID_LOG_WARN, "opensl_io", __VA_ARGS__)

#define INITIAL_DELAY 4 

struct _opensl_stream {
  SLObjectItf engineObject;
  SLEngineItf engineEngine;

  SLObjectItf outputMixObject;

  SLObjectItf playerObject;
  SLPlayItf playerPlay;
  SLAndroidSimpleBufferQueueItf playerBufferQueue;

  SLObjectItf recorderObject;
  SLRecordItf recorderRecord;
  SLAndroidSimpleBufferQueueItf recorderBufferQueue;

  int sampleRate;
  int inputChannels;
  int outputChannels;

  int callbackBufferFrames;
  int totalBufferFrames;

  short *inputBuffer;
  short *outputBuffer;
  short *dummyBuffer;

  int inputIndex;
  int outputIndex;
  int readIndex;
  int initialReadIndex;

  int isRunning;

  void *context;
  opensl_process_t callback;

  struct timespec inputTime;
  int intervals;
  double thresholdMillis;

  sem_t semReady;
  int callbacks;
};

static double time_diff_millis(struct timespec *a, struct timespec *b)
{
  return (a->tv_sec - b->tv_sec) * 1e3 + (a->tv_nsec - b->tv_nsec) * 1e-6;
}

static void recorderCallback(SLAndroidSimpleBufferQueueItf bq, void *context)
{
  OPENSL_STREAM *p = (OPENSL_STREAM *) context;
  if (p->intervals < INITIAL_DELAY) {
    struct timespec t;
    clock_gettime(CLOCK_MONOTONIC, &t);
    if (p->inputTime.tv_sec + p->inputTime.tv_nsec > 0) {
      double dt = time_diff_millis(&t, &p->inputTime);
      if (dt > p->thresholdMillis) {
        __sync_bool_compare_and_swap(
            &p->intervals, p->intervals, p->intervals + 1);
        if (p->intervals < INITIAL_DELAY) {
          p->initialReadIndex = p->inputIndex;
        }
      }
    }
    p->inputTime.tv_sec = t.tv_sec;
    p->inputTime.tv_nsec = t.tv_nsec;
  }
  p->inputIndex = (p->inputIndex + p->callbackBufferFrames) %
      p->totalBufferFrames;
  (*bq)->Enqueue(bq, p->inputBuffer + p->inputIndex * p->inputChannels,
      p->callbackBufferFrames * p->inputChannels * sizeof(short));
}

static void playerCallback(SLAndroidSimpleBufferQueueItf bq, void *context)
{
  OPENSL_STREAM *p = (OPENSL_STREAM *) context;
  if (p->callbacks < 2) {
    if (++p->callbacks == 2) {
      sem_post(&p->semReady);  // Start reading input on the second invocation.
    }
  }
  if (p->readIndex < 0 &&
      __sync_fetch_and_or(&p->intervals, 0) == INITIAL_DELAY) {
    p->readIndex = p->initialReadIndex;  // Start reading input when ready.
  }
  if (p->readIndex >= 0) {  // Render with input if available.
    p->callback(p->context, p->sampleRate, p->callbackBufferFrames,
        p->inputChannels,
        p->inputBuffer + p->readIndex * p->inputChannels,
        p->outputChannels,
        p->outputBuffer + p->outputIndex * p->outputChannels);
    p->readIndex = (p->readIndex + p->callbackBufferFrames) %
        p->totalBufferFrames;
  } else {  // Render with empty input when input is not yet availabe.
    p->callback(p->context, p->sampleRate, p->callbackBufferFrames,
        p->inputChannels,
        p->dummyBuffer,
        p->outputChannels,
        p->outputBuffer + p->outputIndex * p->outputChannels);
  }
  (*bq)->Enqueue(bq, p->outputBuffer + p->outputIndex * p->outputChannels,
      p->callbackBufferFrames * p->outputChannels * sizeof(short));
  p->outputIndex = (p->outputIndex + p->callbackBufferFrames) %
      p->totalBufferFrames;
}

static SLuint32 convertSampleRate(SLuint32 sr)
{
  const SLuint32 maxSRmHz = 0xFFFFFFFFul;
  const SLuint32 maxSRHz = maxSRmHz / 1000ul;
  return sr >= maxSRHz ? maxSRmHz : sr * 10ul;
}

static SLresult openSLCreateEngine(OPENSL_STREAM *p)
{
  const SLuint32 engineIIDCount = 1;
  const SLInterfaceID engineIIDs[] = {SL_IID_ENGINE};
  const SLboolean engineReqs[] = {SL_BOOLEAN_TRUE};

  SLresult result = slCreateEngine(&p->engineObject, 0, NULL,
    engineIIDCount, engineIIDs, engineReqs);
  if (result != SL_RESULT_SUCCESS) return result;

  result = (*p->engineObject)->Realize(p->engineObject, SL_BOOLEAN_FALSE);
  if (result != SL_RESULT_SUCCESS) return result;

  return (*p->engineObject)->GetInterface(
      p->engineObject, SL_IID_ENGINE, &p->engineEngine);
}

static SLresult openSLRecOpen(OPENSL_STREAM *p, SLuint32 sr)
{
  // enforce (temporary?) bounds on channel numbers)
  if (p->inputChannels > 2) {
    return SL_RESULT_PARAMETER_INVALID;
  }

  SLDataLocator_IODevice loc_dev =
      {SL_DATALOCATOR_IODEVICE, SL_IODEVICE_AUDIOINPUT,
       SL_DEFAULTDEVICEID_AUDIOINPUT, NULL};
  SLDataSource audioSrc = {&loc_dev, NULL};  // source: microphone

  int mics;
  if (p->inputChannels > 1) {
    // Yes, we're using speaker macros for mic config.  It's okay, really.
    mics = SL_SPEAKER_FRONT_LEFT | SL_SPEAKER_FRONT_RIGHT;
  } else {
    mics = SL_SPEAKER_FRONT_CENTER;
  }
  SLDataLocator_AndroidSimpleBufferQueue loc_bq =
      {SL_DATALOCATOR_ANDROIDSIMPLEBUFFERQUEUE, 1};
  SLDataFormat_PCM format_pcm =
      {SL_DATAFORMAT_PCM, p->inputChannels, sr, SL_PCMSAMPLEFORMAT_FIXED_16,
       SL_PCMSAMPLEFORMAT_FIXED_16, mics, SL_BYTEORDER_LITTLEENDIAN};
  SLDataSink audioSnk = {&loc_bq, &format_pcm};  // sink: buffer queue

  // create audio recorder (requires the RECORD_AUDIO permission)
  const SLInterfaceID id[1] = {SL_IID_ANDROIDSIMPLEBUFFERQUEUE};
  const SLboolean req[1] = {SL_BOOLEAN_TRUE};
  SLresult result = (*p->engineEngine)->CreateAudioRecorder(
      p->engineEngine, &p->recorderObject, &audioSrc, &audioSnk, 1, id, req);
  if (SL_RESULT_SUCCESS != result) return result;

  result = (*p->recorderObject)->Realize(p->recorderObject, SL_BOOLEAN_FALSE);
  if (SL_RESULT_SUCCESS != result) return result;

  result = (*p->recorderObject)->GetInterface(p->recorderObject,
      SL_IID_RECORD, &p->recorderRecord);
  if (SL_RESULT_SUCCESS != result) return result;

  result = (*p->recorderObject)->GetInterface(
      p->recorderObject, SL_IID_ANDROIDSIMPLEBUFFERQUEUE,
      &p->recorderBufferQueue);
  if (SL_RESULT_SUCCESS != result) return result;

  result = (*p->recorderBufferQueue)->RegisterCallback(
      p->recorderBufferQueue, recorderCallback, p);
  return result;
}

static SLresult openSLPlayOpen(OPENSL_STREAM *p, SLuint32 sr)
{
  // enforce (temporary?) bounds on channel numbers)
  if (p->outputChannels > 2) {
    return SL_RESULT_PARAMETER_INVALID;
  }

  // const SLInterfaceID mixIds[] = {SL_IID_VOLUME};
  // const SLboolean mixReq[] = {SL_BOOLEAN_FALSE};
  const SLuint32 mixIIDCount = 0;
  const SLInterfaceID mixIIDs[] = {};
  const SLboolean mixReqs[] = {};
  SLresult result = (*p->engineEngine)->CreateOutputMix(
      p->engineEngine, &p->outputMixObject, mixIIDCount, mixIIDs, mixReqs);
  if (result != SL_RESULT_SUCCESS) return result;

  result = (*p->outputMixObject)->Realize(
      p->outputMixObject, SL_BOOLEAN_FALSE);
  if (result != SL_RESULT_SUCCESS) return result;

  SLDataFormat_PCM format_pcm;
  memset(&format_pcm, 0, sizeof(format_pcm));
  format_pcm.formatType = SL_DATAFORMAT_PCM;
  format_pcm.numChannels = p->outputChannels;
  format_pcm.samplesPerSec = sr;
  format_pcm.bitsPerSample = SL_PCMSAMPLEFORMAT_FIXED_16;
  format_pcm.containerSize = SL_PCMSAMPLEFORMAT_FIXED_16;
  format_pcm.channelMask = p->outputChannels > 1
                            ? SL_SPEAKER_FRONT_LEFT | SL_SPEAKER_FRONT_RIGHT
                            : SL_SPEAKER_FRONT_CENTER;
  format_pcm.endianness = SL_BYTEORDER_LITTLEENDIAN;

  SLDataLocator_AndroidSimpleBufferQueue loc_bufq;
  memset(&loc_bufq, 0, sizeof(loc_bufq));
  loc_bufq.locatorType = SL_DATALOCATOR_ANDROIDSIMPLEBUFFERQUEUE;
  loc_bufq.numBuffers = 1;

  SLDataSource audioSrc;
  memset(&audioSrc, 0, sizeof(audioSrc));
  audioSrc.pLocator = &loc_bufq;
  audioSrc.pFormat = &format_pcm;

  SLDataLocator_OutputMix loc_outmix;
  memset(&loc_outmix, 0, sizeof(loc_outmix));
  loc_outmix.locatorType = SL_DATALOCATOR_OUTPUTMIX;
  loc_outmix.outputMix = p->outputMixObject;

  SLDataSink audioSnk;
  memset(&audioSnk, 0, sizeof(audioSnk));
  audioSnk.pLocator = &loc_outmix;
  audioSnk.pFormat = NULL;

  // create audio player
  const SLuint32 playIIDCount = 1;
  const SLInterfaceID playIIDs[] = {SL_IID_PLAY,SL_IID_ANDROIDSIMPLEBUFFERQUEUE};
  const SLboolean playReqs[] = {SL_BOOLEAN_TRUE,SL_BOOLEAN_TRUE};
  result = (*p->engineEngine)->CreateAudioPlayer(
      p->engineEngine, &p->playerObject, &audioSrc, &audioSnk,
      playIIDCount, playIIDs, playReqs);
  if (result != SL_RESULT_SUCCESS) return result;

  result = (*p->playerObject)->Realize(p->playerObject, SL_BOOLEAN_FALSE);
  if (result != SL_RESULT_SUCCESS) return result;

  result = (*p->playerObject)->GetInterface(
      p->playerObject, SL_IID_PLAY, &p->playerPlay);
  if (result != SL_RESULT_SUCCESS) return result;

  result = (*p->playerObject)->GetInterface(
      p->playerObject, SL_IID_ANDROIDSIMPLEBUFFERQUEUE,
      &p->playerBufferQueue);
  if (result != SL_RESULT_SUCCESS) return result;

  result = (*p->playerBufferQueue)->RegisterCallback(
      p->playerBufferQueue, playerCallback, p);
  return result;
}

static void openSLDestroyEngine(OPENSL_STREAM *p)
{
  if (p->playerObject) {
    (*p->playerObject)->Destroy(p->playerObject);
  }
  if (p->recorderObject) {
    (*p->recorderObject)->Destroy(p->recorderObject);
  }
  if (p->outputMixObject) {
    (*p->outputMixObject)->Destroy(p->outputMixObject);
  }
  if (p->engineObject) {
    (*p->engineObject)->Destroy(p->engineObject);
  }
}

OPENSL_STREAM *opensl_open(
    int sampleRate, int inChans, int outChans, int callbackBufferFrames,
    opensl_process_t proc, void *context)
{
  if (!proc || !outChans) {
    return NULL;
  }

  SLuint32 srmillihz = convertSampleRate(sampleRate);

  OPENSL_STREAM *p = (OPENSL_STREAM *) calloc(1, sizeof(OPENSL_STREAM));
  if (!p) {
    return NULL;
  }

  sem_init(&p->semReady, 0, 0);

  p->callback = proc;
  p->context = context;
  p->isRunning = 0;

  p->inputBuffer = NULL;
  p->outputBuffer = NULL;
  p->dummyBuffer = NULL;
  p->callbackBufferFrames = callbackBufferFrames;

  // Half the buffer duration in milliseconds.
  p->thresholdMillis = 500.0 * callbackBufferFrames / sampleRate;

  p->totalBufferFrames =
      (sampleRate / callbackBufferFrames) * callbackBufferFrames;

  p->inputChannels = inChans;
  p->outputChannels = outChans;
  p->sampleRate = sampleRate;

  if (openSLCreateEngine(p) != SL_RESULT_SUCCESS) {
    opensl_close(p);
    return NULL;
  }

  if (p->inputChannels > 0) {
    int inBufSize = p->totalBufferFrames * p->inputChannels;
    if (!(openSLRecOpen(p, srmillihz) == SL_RESULT_SUCCESS &&
        (p->inputBuffer = (short *) calloc(inBufSize, sizeof(short))) &&
        (p->dummyBuffer = (short *) calloc(callbackBufferFrames * p->inputChannels,
             sizeof(short))))) {
      opensl_close(p);
      return NULL;
    }
    memset(p->dummyBuffer, 0, sizeof(p->dummyBuffer));
  }

  if (p->outputChannels > 0) {
    int outBufSize = p->totalBufferFrames * p->outputChannels;
    if (!(openSLPlayOpen(p, srmillihz) == SL_RESULT_SUCCESS &&
        (p->outputBuffer = (short *) calloc(outBufSize, sizeof(short))))) {
      opensl_close(p);
      return NULL;
    }
  }

  return p;
}

void opensl_close(OPENSL_STREAM *p)
{
  opensl_pause(p);
  openSLDestroyEngine(p);
  sem_destroy(&p->semReady);
  free(p->inputBuffer);
  free(p->outputBuffer);
  free(p->dummyBuffer);
  free(p);
}

int opensl_is_running(OPENSL_STREAM *p)
{
  return p->isRunning;
}

int opensl_start(OPENSL_STREAM *p)
{
  if (p->isRunning) {
    return 0;  // Already running.
  }

  while (!sem_trywait(&p->semReady));  // Clear semaphore, just in case.

  p->inputIndex = 0;
  p->outputIndex = 0;
  p->readIndex = -1;
  p->initialReadIndex = 0;

  p->inputTime.tv_sec = 0;
  p->inputTime.tv_nsec = 0;
  p->intervals = 0;
  p->callbacks = 0;

  memset(p->outputBuffer, 0, sizeof(p->outputBuffer));
  if ((*p->playerPlay)->SetPlayState(p->playerPlay,
         SL_PLAYSTATE_PLAYING) != SL_RESULT_SUCCESS) {
    opensl_pause(p);
    return -1;
  }
  LOGI("Starting player queue.");
  playerCallback(p->playerBufferQueue, p);

  if (p->recorderRecord) {
    memset(p->inputBuffer, 0, sizeof(p->inputBuffer));
    sem_wait(&p->semReady);
    if ((*p->recorderRecord)->SetRecordState(p->recorderRecord,
            SL_RECORDSTATE_RECORDING) != SL_RESULT_SUCCESS) {
      opensl_pause(p);
      return -1;
    }
    LOGI("Starting recorder queue.");
    recorderCallback(p->recorderBufferQueue, p);
  }

  p->isRunning = 1;
  return 0;
}

void opensl_pause(OPENSL_STREAM *p)
{
  if (p->isRunning) {
    if (p->recorderRecord) {
      (*p->recorderBufferQueue)->Clear(p->recorderBufferQueue);
      (*p->recorderRecord)->SetRecordState(p->recorderRecord,
          SL_RECORDSTATE_STOPPED);
    }
    (*p->playerBufferQueue)->Clear(p->playerBufferQueue);
    (*p->playerPlay)->SetPlayState(p->playerPlay,
        SL_PLAYSTATE_STOPPED);
    p->isRunning = 0;
  }
}
