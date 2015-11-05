// Copyright 2011 Cricket Technology
// www.crickettechnology.com

#import "ck/objc/pathtype.h"
#import "ck/objc/attenuationmode.h"

@class CkoBank;
@class CkoMixer;
@class CkoEffectBus;

/** Objective-C interface for CkSound.
  See ck/sound.h for method documentation. */
@interface CkoSound : NSObject
{
    struct CkSound* m_impl;
}

@property(nonatomic, assign) CkoMixer* mixer;
@property(nonatomic, readonly) BOOL ready;
@property(nonatomic, readonly) BOOL failed;
@property(nonatomic, readonly) BOOL playing;
@property(nonatomic, assign) BOOL paused;
@property(nonatomic, assign) int loopCount;
@property(nonatomic, readonly) int loopStart;
@property(nonatomic, readonly) int loopEnd;
@property(nonatomic, readonly) int currentLoop;
@property(nonatomic, readonly) BOOL loopReleased;
@property(nonatomic, assign) float volume;
@property(nonatomic, assign) int playPosition;
@property(nonatomic, assign) float playPositionMs;
@property(nonatomic, readonly) float mixedVolume;
@property(nonatomic, assign) float pan;
@property(nonatomic, assign) float pitchShift;
@property(nonatomic, assign) float speed;
@property(nonatomic, assign) CkoSound* nextSound;
@property(nonatomic, readonly) int length;
@property(nonatomic, readonly) float lengthMs;
@property(nonatomic, readonly) int sampleRate;
@property(nonatomic, readonly) int channels;
@property(nonatomic, assign) CkoEffectBus* effectBus;
@property(nonatomic, assign, setter=set3dEnabled:) BOOL threeDEnabled;
@property(nonatomic, readonly) BOOL isVirtual;

- (void) setMixer:(CkoMixer*)mixer;
- (CkoMixer*) mixer;

- (BOOL) ready;
- (BOOL) failed;

- (void) play;
- (void) stop;
- (BOOL) playing; 

- (void) setPaused:(BOOL)paused;
- (BOOL) paused;

- (void) setLoop:(int)startFrame endFrame:(int)endFrame;
- (int) loopStart;
- (int) loopEnd;
- (void) setLoopCount:(int)loopCount;
- (int) loopCount;
- (int) currentLoop;
- (void) releaseLoop;
- (BOOL) loopReleased;

- (void) setPlayPosition:(int)frame;
- (int) playPosition;

- (void) setPlayPositionMs:(float)ms;
- (float) playPositionMs;

- (void) setVolume:(float)volume;
- (float) volume;

- (float) mixedVolume;

- (void) setPan:(float)pan;
- (float) pan;

- (void) setPanMatrixLL:(float)ll LR:(float)lr RL:(float)rl RR:(float)rr;
- (void) getPanMatrixLL:(float*)ll LR:(float*)lr RL:(float*)rl RR:(float*)rr;

- (void) setPitchShift:(float)halfSteps;
- (float) pitchShift;

- (void) setSpeed:(float)speed;
- (float) speed;

- (void) setNextSound:(CkoSound*)next;
- (CkoSound*) nextSound;

- (int) length;
- (float) lengthMs;
- (int) sampleRate;
- (int) channels;

- (void) setEffectBus:(CkoEffectBus*)effectBus;
- (CkoEffectBus*) effectBus;

- (void) set3dEnabled:(BOOL)enabled;
- (BOOL) is3dEnabled;

- (BOOL) isVirtual;

- (void) set3dPositionX:(float)x y:(float)y z:(float)z;
- (void) get3dPositionX:(float*)x y:(float*)y z:(float*)z;
- (void) set3dVelocityX:(float)x y:(float)y z:(float)z;
- (void) get3dVelocityX:(float*)x y:(float*)y z:(float*)z;

+ (void) set3dListenerPositionEyeX:(float)eyeX eyeY:(float)eyeY eyeZ:(float)eyeZ
           lookAtX:(float)lookAtX lookAtY:(float)lookAtY lookAtZ:(float)lookAtZ
           upX:(float)upX upY:(float)upY upZ:(float)upZ;
+ (void) get3dListenerPositionEyeX:(float*)eyeX eyeY:(float*)eyeY eyeZ:(float*)eyeZ
           lookAtX:(float*)lookAtX lookAtY:(float*)lookAtY lookAtZ:(float*)lookAtZ
           upX:(float*)upX upY:(float*)upY upZ:(float*)upZ;
+ (void) set3dListenerVelocityX:(float)x y:(float)y z:(float)z;
+ (void) get3dListenerVelocityX:(float*)x y:(float*)y z:(float*)z;

+ (void) set3dAttenuationMode:(CkAttenuationMode)mode nearDist:(float)nearDist farDist:(float)farDist farVol:(float)farVol;
+ (void) get3dAttenuationMode:(CkAttenuationMode*)mode nearDist:(float*)nearDist farDist:(float*)farDist farVol:(float*)farVol;

+ (void) set3dSoundSpeed:(float)speed;
+ (float) get3dSoundSpeed;

+ (float) getSoundSpeedCentimetersPerSecond;
+ (float) getSoundSpeedMetersPerSecond;
+ (float) getSoundSpeedInchesPerSecond;
+ (float) getSoundSpeedFeetPerSecond;

+ newBankSound:(CkoBank*)bank index:(int)index;
+ newBankSound:(CkoBank*)bank name:(NSString*)name;
+ newStreamSound:(NSString*)path; 
+ newStreamSound:(NSString*)path pathType:(CkPathType)pathType;
+ newStreamSound:(NSString*)path pathType:(CkPathType)pathType offset:(int)offset length:(int)length extension:(NSString*)extension;
#if CK_PLATFORM_IOS
+ newAssetStreamSound:(NSURL*)url; 
#endif

@end


