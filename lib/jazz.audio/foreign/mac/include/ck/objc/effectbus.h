// Copyright 2013 Cricket Technology
// www.crickettechnology.com

#import <Foundation/Foundation.h>

@class CkoEffect;

/** Objective-C interface for CkEffectBus.
  See ck/effectbus.h for method documentation. */
@interface CkoEffectBus : NSObject
{
    struct CkEffectBus* m_impl;
}

@property(nonatomic, assign) BOOL bypassed;
@property(nonatomic, assign) float wetDryRatio;
@property(nonatomic, assign) CkoEffectBus* outputBus;

- (void) addEffect:(CkoEffect*)effect;
- (void) removeEffect:(CkoEffect*)effect;
- (void) removeAllEffects;

- (void) setOutputBus:(CkoEffectBus*)bus;
- (CkoEffectBus*) outputBus;

- (void) reset;

- (void) setBypassed:(BOOL)bypass;
- (BOOL) bypassed;

- (void) setWetDryRatio:(float)wetDry;
- (float) wetDryRatio;

+ (CkoEffectBus*) newEffectBus;
+ (CkoEffectBus*) globalEffectBus;

@end





