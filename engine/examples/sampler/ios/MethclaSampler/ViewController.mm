//
//  ViewController.m
//  MethclaSampler
//
//  Created by Stefan Kersten on 11/07/2013.
//  Copyright (c) 2013 Samplecount. All rights reserved.
//

#include <methcla/plugins/pro/soundfile_api_extaudiofile.h>

#import "ViewController.h"
#import "Engine.hpp"

inline NSString* resourcePath(NSString* component)
{
    return [[[NSBundle mainBundle] resourcePath] stringByAppendingPathComponent:component];
}

@interface ViewController ()
{
    Methcla::Examples::Sampler::Engine* engine;
}
@end

@implementation ViewController

- (void)viewDidLoad
{
    [super viewDidLoad];

    // Enable multitouch
    [self.view setMultipleTouchEnabled:YES];

    // Set up the sound engine
    try {
        Methcla::Examples::Sampler::Engine::Options options;
        options.soundFileAPI = methcla_soundfile_api_extaudiofile;
        options.soundDir = [resourcePath(@"sounds") UTF8String];
        engine = new Methcla::Examples::Sampler::Engine(options);
    } catch (std::exception& e) {
        std::cerr << e.what() << std::endl;
    }
}

- (void)didReceiveMemoryWarning
{
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}

- (CGPoint)relativeLocation:(UITouch*)touch inView:(UIView*)view
{
    const CGPoint pt = [touch locationInView:view];
    const CGSize sz = view.bounds.size;
    return { .x = pt.x / sz.width, .y = pt.y / sz.height };
}

- (void) touchesBegan:(NSSet*)touches withEvent:(UIEvent*)event
{
    for (UITouch* touch in touches) {
	    const CGPoint pt = [self relativeLocation:touch inView:self.view];
	    engine->startVoice(reinterpret_cast<intptr_t>(touch), 0, pt.x);
    }
}

- (void) touchesMoved:(NSSet*)touches withEvent:(UIEvent*)event
{
    for (UITouch* touch in touches) {
	    const CGPoint pt = [self relativeLocation:touch inView:self.view];
        engine->updateVoice(reinterpret_cast<intptr_t>(touch), pt.x);
    }
}

- (void) touchesEnded:(NSSet *)touches withEvent:(UIEvent *)event
{
    for (UITouch* touch in touches) {
    	engine->stopVoice(reinterpret_cast<intptr_t>(touch));
    }
}

- (void) touchesCancelled:(NSSet *)touches withEvent:(UIEvent *)event
{
    [self touchesEnded:touches withEvent:event];
}

@end
