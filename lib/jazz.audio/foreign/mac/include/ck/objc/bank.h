// Copyright 2011 Cricket Technology
// www.crickettechnology.com

#import <Foundation/Foundation.h>
#import "ck/objc/pathtype.h"


/** Objective-C interface for CkBank.
  See ck/bank.h for method documentation. */
@interface CkoBank : NSObject
{
    struct CkBank* m_impl;
}

@property(nonatomic, readonly) BOOL loaded;
@property(nonatomic, readonly) BOOL failed;
@property(nonatomic, readonly) NSString* name;
@property(nonatomic, readonly) int numSounds;

- (BOOL) loaded;
- (BOOL) failed;
- (NSString*) name;
- (int) numSounds;
- (NSString*) soundName:(int)index;

+ (CkoBank*) newBank:(NSString*)path;
+ (CkoBank*) newBank:(NSString*)path pathType:(CkPathType)pathType;
+ (CkoBank*) newBank:(NSString*)path pathType:(CkPathType)pathType offset:(int)offset length:(int)length;
+ (CkoBank*) newBankAsync:(NSString*)path;
+ (CkoBank*) newBankAsync:(NSString*)path pathType:(CkPathType)pathType;
+ (CkoBank*) newBankAsync:(NSString*)path pathType:(CkPathType)pathType offset:(int)offset length:(int)length;
+ (CkoBank*) newBankFromMemory:(void*)buf bytes:(int)bytes;

+ (CkoBank*) find:(NSString*)name;

@end




