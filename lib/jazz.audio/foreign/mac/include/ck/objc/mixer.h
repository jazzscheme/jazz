// Copyright 2011 Cricket Technology
// www.crickettechnology.com

#import <Foundation/Foundation.h>


/** Objective-C interface for CkMixer.
  See ck/mixer.h for method documentation. */
@interface CkoMixer : NSObject
{
    struct CkMixer* m_impl;
}

@property(nonatomic, copy) NSString* name;
@property(nonatomic, assign) float volume;
@property(nonatomic, readonly) float mixedVolume;
@property(nonatomic, assign) BOOL paused;
@property(nonatomic, readonly) BOOL mixedPauseState;
@property(nonatomic, assign) CkoMixer* parent;

- (void) setName:(NSString*)name;
- (NSString*) name;

- (void) setVolume:(float)volume;
- (float) volume;

- (float) mixedVolume;

- (void) setPaused:(BOOL)paused;
- (BOOL) paused;

- (BOOL) mixedPauseState;

- (void) setParent:(CkoMixer*)parent;
- (CkoMixer*) parent;

+ (CkoMixer*) master;

+ (CkoMixer*) find:(NSString*)name;

+ (CkoMixer*) newMixer:(NSString*)name;
+ (CkoMixer*) newMixer:(NSString*)name parent:(CkoMixer*)parent;

@end



