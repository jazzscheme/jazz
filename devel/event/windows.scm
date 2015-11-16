;;;
;;;; Minimal event
;;;

(define (setup-app)
  #f)

(c-define-type HWND    (pointer (struct "HWND__")))
(c-define-type UINT    unsigned-int)
(c-define-type WPARAM  unsigned-int)
(c-define-type LPARAM  unsigned-long)
(c-define-type LRESULT long)

(define DefWindowProc
  (c-lambda (HWND UINT WPARAM LPARAM) LRESULT
    "DefWindowProcW"))

(define WM_LBUTTONDOWN
  #x0201)

(c-define (call-process-hwnd-message hwnd umsg wparam lparam) (HWND UINT WPARAM LPARAM) LRESULT "windowproc" "static"
  (cond ((= umsg WM_LBUTTONDOWN)
         (continuation-return exit-continuation #f)
         0)
        (else
         (DefWindowProc hwnd umsg wparam lparam))))

(define create-window
  (c-lambda () void
    #<<c-end
    HINSTANCE instance;
    WNDCLASSEX wc;
    ATOM classatom;
    
    instance = ___EXT(___get_program_startup_info)()->hInstance;
    wc.cbSize =         sizeof(WNDCLASSEX);
    wc.style =          CS_DBLCLKS;
    wc.lpfnWndProc =    windowproc;
    wc.cbClsExtra =     0;
    wc.cbWndExtra =     0;
    wc.hInstance =      instance;
    wc.hIcon =          NULL;
    wc.hCursor =        NULL;
    wc.hbrBackground =  NULL;
    wc.lpszMenuName =   NULL;
    wc.lpszClassName =  "JWindow";
    wc.hIconSm =        NULL;
    classatom = RegisterClassEx(&wc);
    CreateWindowEx(
      WS_EX_APPWINDOW,
      (LPCTSTR) classatom,
      NULL,
      WS_CLIPSIBLINGS | WS_CLIPCHILDREN | WS_POPUP | WS_OVERLAPPEDWINDOW | WS_MINIMIZEBOX | WS_SYSMENU | WS_VISIBLE,
      0, 0, 500, 500,
      NULL,
      NULL,
      instance,
      NULL);
c-end
))

(c-define-type MSG (type "MSG"))
(c-define-type MSG* (pointer MSG))

(define MSG-message
  (c-lambda (MSG*) unsigned-int
    #<<c-end
    ___result = ___arg1->message;
c-end
))

(define TranslateMessage
  (c-lambda (MSG*) void
    "TranslateMessage"))

(define DispatchMessage
  (c-lambda (MSG*) void
    "DispatchMessageW"))

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
        (define QS_ALLINPUT
          #x04FF)
        
        (define (process-message msg)
          (execute-event event-thread
            (lambda ()
              (TranslateMessage msg)
              (DispatchMessage msg))))
        
        (declare (proper-tail-calls))
        (let ((port (open-event-queue QS_ALLINPUT)))
          (let loop ()
            (let ((msg (read port)))
              (process-message msg)
              (foreign-release! msg))
            (loop)))))))

(define (main)
  (continuation-capture
    (lambda (cont)
      (set! exit-continuation cont)
      (setup-app)
      (create-window)
      (process-messages)
      (process-events))))

(main)
