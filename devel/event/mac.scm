;;;
;;;; Minimal event
;;;

(c-declare #<<c-end

#import <Cocoa/Cocoa.h>

void mouse_callback();

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
    mouse_callback();
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

(define poll-events
  (c-lambda () void
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
    while (event);
c-end
))

(c-define (mouse-callback) () void "mouse_callback" ""
  (continuation-return exit-continuation #f))

(define event-thread
  (current-thread))

(define exit-continuation
  #f)

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

(define (main)
  (continuation-capture
    (lambda (cont)
      (set! exit-continuation cont)
      (setup-app)
      (create-window)
      (process-messages)
      (process-events))))

(main)
