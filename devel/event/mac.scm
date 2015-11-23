;;;
;;;; Minimal event
;;;

(include "common.scm")

;;;
;;;; Platform
;;;

(c-declare #<<c-end

#import <Cocoa/Cocoa.h>

void mouse_down();

@interface MyException : NSException
@end

@implementation MyException
@end

@interface MyApplication : NSApplication
{
}
@end

@implementation MyApplication
@end

@interface MyWindowDelegate : NSObject <NSDraggingDestination>
{
}
@end

@implementation MyWindowDelegate
@end

@interface MyView: NSView
{
}
@end

@implementation MyView
- (void)mouseDown:(NSEvent *)event
{
    mouse_down();
}
@end

void createMenuBar()
{
    NSMenu* bar = [[NSMenu alloc] init];
    [NSApp setMainMenu:bar];
}

c-end
)

(define setup-app
  (c-lambda () void
    #<<c-end
    [MyApplication sharedApplication];
  
    [NSApp setActivationPolicy:NSApplicationActivationPolicyRegular];
  
    createMenuBar();
c-end
))

(define create-window
  (c-lambda () void
    #<<c-end
    int style;
    NSWindow *window;
    id delegate;
    NSView *view;
  
    style = NSTitledWindowMask | NSClosableWindowMask | NSMiniaturizableWindowMask | NSResizableWindowMask;
    window = [[NSWindow alloc] initWithContentRect:NSMakeRect(100, 100, 300, 300)
                                         styleMask:style
                                           backing:NSBackingStoreBuffered
                                             defer:NO];

    delegate = [[MyWindowDelegate alloc] init];
    [window setDelegate:delegate];
  
    view = [[MyView alloc] init];
    
    [window setContentView: view];
    [window makeKeyAndOrderFront:window];
c-end
))

(c-processing (poll-events) void
  #<<c-end
    NSEvent* event;
 
    do
    {
        event = [NSApp nextEventMatchingMask:NSAnyEventMask
                                   untilDate:nil
                                      inMode:NSDefaultRunLoopMode
                                     dequeue:YES];

        if (event)
            [NSApp sendEvent:event];
    }
    while (event && !exit_processing);
c-end
)

(c-callback (mouse-down) () void "mouse_down" ""
  #;
  (append 1 2)
  (exit-main))

;;;
;;;; Events
;;;

(define event-thread
  (current-thread))

(define (execute-event thread thunk)
  (let ((mutex (make-mutex)))
    (mutex-lock! mutex)
    (thread-send thread
      (lambda ()
        (thunk)
        (mutex-unlock! mutex)))
    (mutex-lock! mutex)))

(define (process-events)
  (let loop ()
    (let ((thunk (thread-receive)))
      (thunk))
    (loop)))

;;;
;;;; Messages
;;;

(define (process-messages)
  (thread-start!
    (make-thread
      (lambda ()
        (let loop ()
          (execute-event event-thread
            (lambda ()
              (poll-events)))
          (thread-sleep! .01)
          (loop))))))

;;;
;;;; Main
;;;

(define main-exit
  (make-exception))

(define (main-exception? exc)
  (eq? exc main-exit))

(define (main)
  (catch (main-exception? exc
           #f)
    (setup-app)
    (create-window)
    (process-messages)
    (process-events)))

(define (exit-main)
  (throw main-exit))

(main)
