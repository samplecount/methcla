#import  "MescalineMobileAppDelegate.h"
#include "drawbuffer.hpp"
#include "scope.hpp"

#include <Mescaline/Audio/IO/Driver.hpp>
#include <Mescaline/Audio/Group.hpp>
#include <Mescaline/Audio/Plugin/API.h>
#include <Mescaline/Audio/Synth.hpp>
#include <Mescaline/Audio/SynthDef.hpp>

#include <AudioToolbox/AudioServices.h>
#include <boost/thread.hpp>

#include <iostream>

#include <lilv/lilv.h>
#include <lv2/lv2plug.in/ns/ext/atom/atom.h>
#include <lv2/lv2plug.in/ns/ext/atom/forge.h>
#include <lv2/lv2plug.in/ns/ext/atom/util.h>

MESCALINE_EXPORT void MESCALINE_INIT_FUNC(osc)(MescalineHost*);
MESCALINE_EXPORT void MESCALINE_INIT_FUNC(Scope)(MescalineHost*);

class MyLoader : public Mescaline::Audio::Plugin::StaticLoader
{
public:
    MyLoader()
        : Mescaline::Audio::Plugin::StaticLoader(descriptorFunctions())
    {
        setenv("LV2_PATH", "/Users/sk/Library/Audio/Plug-Ins/LV2", 1);
    }

    static Mescaline::Audio::Plugin::StaticLoader::DescriptorFunctionMap descriptorFunctions()
    {
        Mescaline::Audio::Plugin::StaticLoader::DescriptorFunctionMap dfs;
        extern const LV2_Descriptor* puesnada_sine_lv2_descriptor(uint32_t index);
        dfs["http://mescaline.puesnada.es/lv2/plugins/sine"] = puesnada_sine_lv2_descriptor;
        return dfs;
    }
};

class MyEngine : public Mescaline::Audio::Engine
{
public:
    MyEngine(MyLoader* loader)
        : Mescaline::Audio::Engine(loader)
        , m_osc(0)
        , m_scope(0)
    { }

    virtual void configure(const Mescaline::Audio::IO::Driver& driver)
    {
        Mescaline::Audio::Engine::configure(driver);

        // Create sine instance
        const Mescaline::Audio::Plugin::Manager::PluginHandle& def = env().lookupSynthDef(
            "http://mescaline.puesnada.es/lv2/plugins/sine" );
        Mescaline::Audio::Synth* synth = m_osc = Mescaline::Audio::Synth::construct(env(), env().nextResourceId(), env().rootNode(), *def);
        env().rootNode()->addToTail(*synth);
        synth->mapOutput(0, Mescaline::Audio::ResourceId(3), Mescaline::Audio::kOut);

//        const Mescaline::Audio::SynthDef& scopeDef = environment()->lookupSynthDef("scope");
//        Mescaline::Audio::Synth* scope = Mescaline::Audio::Synth::construct(*environment(), 2, environment()->rootNode(), scopeDef);
//        environment()->rootNode()->addToTail(*scope);
//        scope->mapInput(0, Mescaline::Audio::AudioBusId(Mescaline::Audio::AudioBusId::kOutput, 0), Mescaline::Audio::kIn);
//        m_scope = scope->synth<Mescaline::Audio::ScopeSynth>();
    }

    Mescaline::Audio::Synth* osc() { return m_osc; }
    Mescaline::Audio::ScopeSynth* scope() { return m_scope; }

private:
    Mescaline::Audio::Synth* m_osc;
    Mescaline::Audio::ScopeSynth* m_scope;
};

@implementation aurioTouchAppDelegate

// value, a, r, g, b
GLfloat colorLevels[] = {
0., 1., 0., 0., 0.,
.333, 1., .7, 0., 0.,
.667, 1., 0., 0., 1.,
1., 1., 0., 1., 1.,
};

@synthesize window;
@synthesize view;

void cycleOscilloscopeLines()
{
    // Cycle the lines in our draw buffer so that they age and fade. The oldest line is discarded.
    int drawBuffer_i;
    for (drawBuffer_i=(kNumDrawBuffers - 2); drawBuffer_i>=0; drawBuffer_i--)
        memmove(drawBuffers[drawBuffer_i + 1], drawBuffers[drawBuffer_i], drawBufferLen);
}

// #pragma mark -Audio Session Property Listener

//void propListener(  void *                  inClientData,
//                    AudioSessionPropertyID  inID,
//                    UInt32                  inDataSize,
//                    const void *            inData)
//{
//    aurioTouchAppDelegate *THIS = (aurioTouchAppDelegate*)inClientData;
//    if (inID == kAudioSessionProperty_AudioRouteChange)
//    {
//        try {
//             UInt32 isAudioInputAvailable;
//             UInt32 size = sizeof(isAudioInputAvailable);
//             XThrowIfError(AudioSessionGetProperty(kAudioSessionProperty_AudioInputAvailable, &size, &isAudioInputAvailable), "couldn't get AudioSession AudioInputAvailable property value");
//
//             if(THIS->unitIsRunning && !isAudioInputAvailable)
//             {
//                 XThrowIfError(AudioOutputUnitStop(THIS->rioUnit), "couldn't stop unit");
//                 THIS->unitIsRunning = false;
//             }
//
//             else if(!THIS->unitIsRunning && isAudioInputAvailable)
//             {
//                 XThrowIfError(AudioSessionSetActive(true), "couldn't set audio session active\n");
//
//                 if (!THIS->unitHasBeenCreated) // the rio unit is being created for the first time
//                 {
//                     XThrowIfError(SetupRemoteIO(THIS->rioUnit, THIS->inputProc, THIS->thruFormat), "couldn't setup remote i/o unit");
//                     THIS->unitHasBeenCreated = true;
//
//                     UInt32 maxFPS;
//                     size = sizeof(maxFPS);
//                     XThrowIfError(AudioUnitGetProperty(THIS->rioUnit, kAudioUnitProperty_MaximumFramesPerSlice, kAudioUnitScope_Global, 0, &maxFPS, &size), "couldn't get the remote I/O unit's max frames per slice");
//
//                     THIS->fftBufferManager = new FFTBufferManager(maxFPS);
//                     THIS->l_fftData = new int32_t[maxFPS/2];
//
//                     THIS->oscilLine = (GLfloat*)malloc(drawBufferLen * 2 * sizeof(GLfloat));
//                 }
//
//                 XThrowIfError(AudioOutputUnitStart(THIS->rioUnit), "couldn't start unit");
//                 THIS->unitIsRunning = true;
//             }
//
//            // we need to rescale the sonogram view's color thresholds for different input
//            CFStringRef newRoute;
//            size = sizeof(CFStringRef);
//            XThrowIfError(AudioSessionGetProperty(kAudioSessionProperty_AudioRoute, &size, &newRoute), "couldn't get new audio route");
//            if (newRoute)
//            {
//                CFShow(newRoute);
//                if (CFStringCompare(newRoute, CFSTR("Headset"), NULL) == kCFCompareEqualTo) // headset plugged in
//                {
//                    colorLevels[0] = .3;
//                    colorLevels[5] = .5;
//                }
//                else if (CFStringCompare(newRoute, CFSTR("Receiver"), NULL) == kCFCompareEqualTo) // headset plugged in
//                {
//                    colorLevels[0] = 0;
//                    colorLevels[5] = .333;
//                    colorLevels[10] = .667;
//                    colorLevels[15] = 1.0;
//
//                }
//                else
//                {
//                    colorLevels[0] = 0;
//                    colorLevels[5] = .333;
//                    colorLevels[10] = .667;
//                    colorLevels[15] = 1.0;
//
//                }
//            }
//        } catch (CAXException e) {
//            char buf[256];
//            fprintf(stderr, "Error: %s (%s)\n", e.mOperation, e.FormatError(buf));
//        }
//
//    }
//}

//#pragma mark -RIO Render Callback
//
//static OSStatus PerformThru(
//                            void                        *inRefCon,
//                            AudioUnitRenderActionFlags  *ioActionFlags,
//                            const AudioTimeStamp        *inTimeStamp,
//                            UInt32                      inBusNumber,
//                            UInt32                      inNumberFrames,
//                            AudioBufferList             *ioData)
//{
//    aurioTouchAppDelegate *THIS = (aurioTouchAppDelegate *)inRefCon;
//    OSStatus err = AudioUnitRender(THIS->rioUnit, ioActionFlags, inTimeStamp, 1, inNumberFrames, ioData);
//    if (err) { printf("PerformThru: error %d\n", (int)err); return err; }
//
//    // Remove DC component
////  for(UInt32 i = 0; i < ioData->mNumberBuffers; ++i)
////      THIS->dcFilter[i].InplaceFilter((SInt32*)(ioData->mBuffers[i].mData), inNumberFrames, 1);
//        float* inputs[ioData->mNumberBuffers];
//        float* outputs[ioData->mNumberBuffers];
//        for (size_t i = 0; i < ioData->mNumberBuffers; ++i) {
//            inputs[i] = (float*)ioData->mBuffers[i].mData;
//            outputs[i] = (float*)ioData->mBuffers[i].mData;
//        }
//
//    if (THIS->displayMode == aurioTouchDisplayModeOscilloscopeWaveform)
//    {
//        // The draw buffer is used to hold a copy of the most recent PCM data to be drawn on the oscilloscope
//        if (drawBufferLen != drawBufferLen_alloced)
//        {
//            int drawBuffer_i;
//
//            // Allocate our draw buffer if needed
//            if (drawBufferLen_alloced == 0)
//                for (drawBuffer_i=0; drawBuffer_i<kNumDrawBuffers; drawBuffer_i++)
//                    drawBuffers[drawBuffer_i] = NULL;
//
//            // Fill the first element in the draw buffer with PCM data
//            for (drawBuffer_i=0; drawBuffer_i<kNumDrawBuffers; drawBuffer_i++)
//            {
//                drawBuffers[drawBuffer_i] = (SInt8 *)realloc(drawBuffers[drawBuffer_i], drawBufferLen);
//                bzero(drawBuffers[drawBuffer_i], drawBufferLen);
//            }
//
//            drawBufferLen_alloced = drawBufferLen;
//        }
//
//        int i;
//
//        SInt8 *data_ptr = (SInt8 *)(ioData->mBuffers[0].mData);
//        for (i=0; i<inNumberFrames; i++)
//        {
//            if ((i+drawBufferIdx) >= drawBufferLen)
//            {
//                cycleOscilloscopeLines();
//                drawBufferIdx = -i;
//            }
//            drawBuffers[0][i + drawBufferIdx] = data_ptr[2];
//            data_ptr += 4;
//        }
//        drawBufferIdx += inNumberFrames;
//    }
//
//    else if ((THIS->displayMode == aurioTouchDisplayModeSpectrum) || (THIS->displayMode == aurioTouchDisplayModeOscilloscopeFFT))
//    {
//        if (THIS->fftBufferManager == NULL) return noErr;
//
//        if (THIS->fftBufferManager->NeedsNewAudioData())
//        {
//            THIS->fftBufferManager->GrabAudioData(ioData);
//        }
//
//    }
//    if (THIS->mute == YES) { SilenceData(ioData); }
//
//    return err;
//}

#pragma mark-

- (void)applicationDidFinishLaunching:(UIApplication *)application
{
    // Turn off the idle timer, since this app doesn't rely on constant touch input
    application.idleTimerDisabled = YES;

    memset(drawBuffers, 0, kNumDrawBuffers * sizeof(float*));
    drawBuffers[0] = (float*)malloc(drawBufferLen * sizeof(float));

    try {
        // Initialize and configure the audio session
        m_engine = new MyEngine(new MyLoader());
        m_audioDriver = new Mescaline::Audio::IO::RemoteIODriver(m_engine);

        oscilLine = (GLfloat*)malloc(drawBufferLen * 2 * sizeof(GLfloat));

        m_audioDriver->start();
    }
    catch (...) {
        fprintf(stderr, "An unknown error occurred\n");
    }

    // Set ourself as the delegate for the EAGLView so that we get drawing and touch events
    view.delegate = self;

    // Enable multi touch so we can handle pinch and zoom in the oscilloscope
    view.multipleTouchEnabled = YES;

    // Set up the view to refresh at 20 hz
    [view setAnimationInterval: 1./20.];
    [view startAnimation];

    // boost::thread(boost::ref(m_oscServer));
}

- (void)applicationDidBecomeActive:(UIApplication *)application {
    // Start animation now that we're in the foreground
    view.applicationResignedActive = NO;
    [view startAnimation];
    AudioSessionSetActive(true);
}

- (void)applicationWillResignActive:(UIApplication *)application {
    // Stop animation before going into background
    view.applicationResignedActive = YES;
    [view stopAnimation];
}

- (void)applicationDidEnterBackground:(UIApplication *)application {
}

- (void)applicationWillEnterForeground:(UIApplication *)application {
}


- (void)dealloc
{
    [view release];
    [window release];

    free(oscilLine);

    [super dealloc];
}


- (void)drawOscilloscope
{
    // Clear the view
    glClear(GL_COLOR_BUFFER_BIT);

    glBlendFunc(GL_SRC_ALPHA, GL_ONE);

    glColor4f(1., 1., 1., 1.);

    glPushMatrix();

    glTranslatef(0., 1024., 0.);
    glRotatef(-90., 0., 0., 1.);


    glEnable(GL_TEXTURE_2D);
    glEnableClientState(GL_VERTEX_ARRAY);
    glEnableClientState(GL_TEXTURE_COORD_ARRAY);

//    {
//        // Draw our background oscilloscope screen
//        const GLfloat vertices[] = {
//            0., 0.,
//            512., 0.,
//            0.,  512.,
//            512.,  512.,
//        };
//        const GLshort texCoords[] = {
//            0, 0,
//            1, 0,
//            0, 1,
//            1, 1,
//        };
//
//
//        glBindTexture(GL_TEXTURE_2D, bgTexture);
//
//        glVertexPointer(2, GL_FLOAT, 0, vertices);
//        glTexCoordPointer(2, GL_SHORT, 0, texCoords);
//
//        glDrawArrays(GL_TRIANGLE_STRIP, 0, 4);
//    }

    GLfloat *oscilLine_ptr;
    GLfloat max = drawBufferLen;
    float *drawBuffer_ptr;

    // Alloc an array for our oscilloscope line vertices
    if (resetOscilLine) {
        oscilLine = (GLfloat*)realloc(oscilLine, drawBufferLen * 2 * sizeof(GLfloat));
        resetOscilLine = NO;
    }

    glPushMatrix();

    // Translate to the left side and vertical center of the screen, and scale so that the screen coordinates
    // go from 0 to 1 along the X, and -1 to 1 along the Y
    glTranslatef(0., 768./2., 0.);
    glScalef(1024., 768./2., 1.);

    // Set up some GL state for our oscilloscope lines
    glDisable(GL_TEXTURE_2D);
    glDisableClientState(GL_TEXTURE_COORD_ARRAY);
    glDisableClientState(GL_COLOR_ARRAY);
    glDisable(GL_LINE_SMOOTH);
    glLineWidth(2.);

//    reinterpret_cast<MyEngine*>(m_engine)->scope()->buffer().dequeue(drawBuffers[0], drawBufferLen);

    int drawBuffer_i;
    // Draw a line for each stored line in our buffer (the lines are stored and fade over time)
    for (drawBuffer_i=0; drawBuffer_i<kNumDrawBuffers; drawBuffer_i++)
    {
        if (!drawBuffers[drawBuffer_i]) continue;

        oscilLine_ptr = oscilLine;
        drawBuffer_ptr = drawBuffers[drawBuffer_i];

        GLfloat i;
        // Fill our vertex array with points
        for (i=0.; i<max; i=i+1.)
        {
            *oscilLine_ptr++ = i/max;
//            *oscilLine_ptr++ = 2. * (GLfloat)rand() / (GLfloat)RAND_MAX - 1.;
            *oscilLine_ptr++ = *drawBuffer_ptr++;
        }

        // If we're drawing the newest line, draw it in solid green. Otherwise, draw it in a faded green.
        if (drawBuffer_i == 0)
            glColor4f(0., 1., 0., 1.);
        else
            glColor4f(0., 1., 0., (.24 * (1. - ((GLfloat)drawBuffer_i / (GLfloat)kNumDrawBuffers))));

        // Set up vertex pointer,
        glVertexPointer(2, GL_FLOAT, 0, oscilLine);

        // and draw the line.
        glDrawArrays(GL_LINE_STRIP, 0, drawBufferLen);

    }

    glPopMatrix();

    glPopMatrix();
}


- (void)drawView:(id)sender forTime:(NSTimeInterval)time
{
    [self drawOscilloscope];
}

- (void)touchesBegan:(NSSet *)touches withEvent:(UIEvent *)event
{
//    // If we're if waveform mode and not currently in a pinch event, and we've got two touches, start a pinch event
//    if ((!pinchEvent) && ([[event allTouches] count] == 2) && (self.displayMode == aurioTouchDisplayModeOscilloscopeWaveform))
//    {
//        pinchEvent = event;
//        NSArray *t = [[event allTouches] allObjects];
//        lastPinchDist = fabs([[t objectAtIndex:0] locationInView:view].x - [[t objectAtIndex:1] locationInView:view].x);
//
//        sampleSizeText.text = [NSString stringWithFormat:@"%i ms", drawBufferLen / (int)(hwSampleRate / 1000.)];
//        [view addSubview:sampleSizeOverlay];
//    }
}

- (void)touchesMoved:(NSSet *)touches withEvent:(UIEvent *)event
{
    NSArray *t = [[event allTouches] allObjects];
    float freq = [[t objectAtIndex:0] locationInView:view].y;

    reinterpret_cast<MyEngine*>(m_engine)->osc()->controlInput(0) = freq;

    const char* str[] = { "Hello!", "Mother Mary wasn't merry", "Blah albajsdhakjsh balsdj", 0 };
    for (const char** it = str; *it != 0; it++) {
        const size_t atomSize = sizeof(LV2_Atom_String) + strlen(*it) + 1;
        LV2_Atom* atom = (LV2_Atom*)malloc(lv2_atom_pad_size(atomSize));
        LV2_Atom_Forge forge;
        lv2_atom_forge_init(&forge, m_engine->env().pluginManager().lv2UridMap());
        lv2_atom_forge_set_buffer(&forge, (uint8_t*)atom, atomSize);
        BOOST_ASSERT( lv2_atom_forge_string(&forge, (uint8_t*)(*it), strlen(*it)) != 0 );
        m_engine->env().sendMessage(atom);
    }

//    // If we are in a pinch event...
//    if ((event == pinchEvent) && ([[event allTouches] count] == 2))
//    {
//        CGFloat thisPinchDist, pinchDiff;
//        NSArray *t = [[event allTouches] allObjects];
//        thisPinchDist = fabs([[t objectAtIndex:0] locationInView:view].x - [[t objectAtIndex:1] locationInView:view].x);
//
//        // Find out how far we traveled since the last event
//        pinchDiff = thisPinchDist - lastPinchDist;
//        // Adjust our draw buffer length accordingly,
//        drawBufferLen -= 12 * (int)pinchDiff;
//        drawBufferLen = CLAMP(kMinDrawSamples, drawBufferLen, kMaxDrawSamples);
//        resetOscilLine = YES;
//
//        // and display the size of our oscilloscope window in our overlay view
//        sampleSizeText.text = [NSString stringWithFormat:@"%i ms", drawBufferLen / (int)(hwSampleRate / 1000.)];
//
//        lastPinchDist = thisPinchDist;
//    }
}

- (void)touchesEnded:(NSSet *)touches withEvent:(UIEvent *)event
{
//    if (event == pinchEvent)
//    {
//        // If our pinch/zoom has ended, nil out the pinchEvent and remove the overlay view
//        [sampleSizeOverlay removeFromSuperview];
//        pinchEvent = nil;
//        return;
//    }
//
//    // any tap in sonogram view will exit back to the waveform
//    if (self.displayMode == aurioTouchDisplayModeSpectrum)
//    {
//        AudioServicesPlaySystemSound(buttonPressSound);
//        self.displayMode = aurioTouchDisplayModeOscilloscopeWaveform;
//        return;
//    }
//
//    UITouch *touch = [touches anyObject];
//    if (unitIsRunning)
//    {
//        if (CGRectContainsPoint(CGRectMake(0., 5., 52., 99.), [touch locationInView:view])) // The Sonogram button was touched
//        {
//            AudioServicesPlaySystemSound(buttonPressSound);
//            if ((self.displayMode == aurioTouchDisplayModeOscilloscopeWaveform) || (self.displayMode == aurioTouchDisplayModeOscilloscopeFFT))
//            {
//                if (!initted_spectrum) [self setupViewForSpectrum];
//                [self clearTextures];
//                self.displayMode = aurioTouchDisplayModeSpectrum;
//            }
//        }
//        else if (CGRectContainsPoint(CGRectMake(0., 104., 52., 99.), [touch locationInView:view])) // The Mute button was touched
//        {
//            AudioServicesPlaySystemSound(buttonPressSound);
//            self.mute = !(self.mute);
//            return;
//        }
//        else if (CGRectContainsPoint(CGRectMake(0., 203, 52., 99.), [touch locationInView:view])) // The FFT button was touched
//        {
//            AudioServicesPlaySystemSound(buttonPressSound);
//            self.displayMode = (self.displayMode == aurioTouchDisplayModeOscilloscopeWaveform) ?  aurioTouchDisplayModeOscilloscopeFFT :
//                                                                                                aurioTouchDisplayModeOscilloscopeWaveform;
//            return;
//        }
//    }
}

@end
