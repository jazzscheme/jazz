// Copyright 2013 Cricket Technology
// www.crickettechnology.com

#import <Foundation/Foundation.h>
#import "ck/effecttype.h"


/** Objective-C interface for CkEffect.
  See ck/effect.h for method documentation. */
@interface CkoEffect : NSObject
{
    struct CkEffect* m_impl;
}

@property(nonatomic, assign) BOOL bypassed;
@property(nonatomic, assign) float wetDryRatio;

- (void) setParam:(int)paramId value:(float)value;

- (void) reset;

- (void) setBypassed:(BOOL)bypass;
- (BOOL) bypassed;

- (void) setWetDryRatio:(float)wetDry;
- (float) wetDryRatio;

+ (CkoEffect*) newEffect:(CkEffectType)type;
+ (CkoEffect*) newCustomEffect:(int)id;

@end




