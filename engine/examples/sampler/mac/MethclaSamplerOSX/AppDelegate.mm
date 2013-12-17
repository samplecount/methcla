//
//  AppDelegate.m
//  MethclaSamplerOSX
//
//  Created by Stefan Kersten on 27/08/2013.
//  Copyright (c) 2013 Samplecount. All rights reserved.
//

#import "AppDelegate.h"
#import "Engine.hpp"

@interface KeyboardView : NSView
{
    Engine* engine;
}
- (void)setEngine:(Engine*)theEngine;
@end

int keyToIndex(unichar key)
{
    switch (key) {
    case 122: return 0;
    case 120: return 1;
    case  99: return 2;
    case 118: return 3;
    case  98: return 4;
    case  97: return 5;
    case 115: return 6;
    case 100: return 7;
    case 102: return 8;
    case 103: return 9;
    case 113: return 10;
    case 119: return 11;
    case 101: return 12;
    case 114: return 13;
    case 116: return 14;
    }
    return -1;
}

@implementation KeyboardView
- (void)setEngine:(Engine*)theEngine
{
    engine = theEngine;
}
- (BOOL)acceptsFirstResponder
{
    return YES;
}
- (void)keyDown:(NSEvent *)theEvent
{
    if (![theEvent isARepeat]) {
        NSString* chars = [theEvent charactersIgnoringModifiers];
        if ([chars length] == 1) {
            unichar key = [chars characterAtIndex:0];
//            NSLog(@"keyDown: %u", key);
            int index = keyToIndex(key);
            if (index >= 0) {
                engine->startVoice(static_cast<intptr_t>(index), engine->nextSound(), 0.5);
            }
        }
    }
}
- (void)keyUp:(NSEvent *)theEvent
{
    if (![theEvent isARepeat]) {
        NSString* chars = [theEvent charactersIgnoringModifiers];
        if ([chars length] == 1) {
            unichar key = [chars characterAtIndex:0];
//            NSLog(@"keyUp: %u", key);
            int index = keyToIndex(key);
            if (index >= 0) {
                engine->stopVoice(static_cast<intptr_t>(index));
            }
        }
    }
}
@end

inline NSString* resourcePath(NSString* component)
{
    return [[[NSBundle mainBundle] resourcePath] stringByAppendingPathComponent:component];
}

@interface AppDelegate ()
{
    Engine* engine;
}
@end

@implementation AppDelegate
- (void)applicationDidFinishLaunching:(NSNotification *)aNotification
{
    // Set up the sound engine
    try {
        // Initialize and configure the audio engine
        engine = new Engine([resourcePath(@"sounds") UTF8String]);
    } catch (std::exception& e) {
        std::cerr << e.what() << std::endl;
    }

    // Set up a KeyboardView as first responder
    KeyboardView* view = [[KeyboardView alloc] initWithFrame:NSMakeRect(100, 100, 100, 100)];
    [view setEngine:engine];
    [view setWantsLayer:YES];
    view.layer.backgroundColor = [[NSColor yellowColor] CGColor];
    [self.window.contentView addSubview:view];
    [self.window makeFirstResponder:view];
}

@end
