#import <UIKit/UIKit.h>
#import <OpenGLES/EAGL.h>
#import <OpenGLES/ES1/gl.h>
#import <OpenGLES/ES1/glext.h>
#include <libkern/OSAtomic.h>
#include <CoreFoundation/CFURL.h>

#include <Mescaline/Audio/Engine.hpp>
#include <Mescaline/Audio/IO/Client.hpp>
#include <Mescaline/Audio/IO/RemoteIODriver.hpp>

#import "EAGLView.h"

#define SPECTRUM_BAR_WIDTH 4

#ifndef CLAMP
#define CLAMP(min,x,max) (x < min ? min : (x > max ? max : x))
#endif

inline double linearInterp(double valA, double valB, double fract)
{
	return valA + ((valB - valA) * fract);
}

@interface aurioTouchAppDelegate : NSObject <UIApplicationDelegate, EAGLViewDelegate> {
    IBOutlet UIWindow*			window;
    IBOutlet EAGLView*			view;

    GLuint				bgTexture;
    GLfloat*				oscilLine;
    BOOL				resetOscilLine;
    
    Mescaline::Audio::IO::RemoteIODriver* m_audioDriver;
    Mescaline::Audio::Engine* m_engine;
}

@property (nonatomic, retain)	UIWindow*				window;
@property (nonatomic, retain)	EAGLView*				view;

@end

