;;;===========================================================================

;;; File: "loglib-scm.scm"

;;; Copyright (c) 2013 by Marc Feeley, All Rights Reserved.

;;;===========================================================================

(unit protected gambit.log.implementation.loglib-scm

(c-declare #<<end-of-c-declare

#include "loglib.c"

___SCMOBJ release_rc_log_context( void* ptr ) {

  struct log_context* p = ___CAST(struct log_context*,ptr);

  ___EXT(___release_rc)( p);

  return ___FIX(___NO_ERR);
}

end-of-c-declare
)

(c-define-type log-context (struct "log_context"))
(c-define-type log-context* (pointer log-context (log-context*)))
(c-define-type log-context*/release-rc (pointer log-context (log-context*) "release_rc_log_context"))

(define (log-context-alloc)
  ((c-lambda ()
             log-context*/release-rc
             "___result_voidstar = ___EXT(___alloc_rc) (sizeof (struct log_context));")))

;;;---------------------------------------------------------------------------

(define log-RGBCOLOR
  (c-lambda (int int int) scheme-object
            "___result = ___FIX(RGBCOLOR(___arg1,___arg2,___arg3));"))

(define log-BLACK    (log-RGBCOLOR   0   0   0))
(define log-WHITE    (log-RGBCOLOR 255 255 255))
(define log-RED      (log-RGBCOLOR 255   0   0))
(define log-GREEN    (log-RGBCOLOR 0   255   0))
(define log-BLUE     (log-RGBCOLOR 0     0 255))
(define log-YELLOW   (log-RGBCOLOR 255 255   0))
(define log-MAGENTA  (log-RGBCOLOR 255   0 255))
(define log-CYAN     (log-RGBCOLOR   0 255 255))

(define log-LAVENDER (log-RGBCOLOR 230 230 250))
(define log-PINK     (log-RGBCOLOR 255 192 203))
(define log-GOLD     (log-RGBCOLOR 255 215   0))
(define log-TOMATO   (log-RGBCOLOR 255  99  71))
(define log-PURPLE   (log-RGBCOLOR 160  32 240))
(define log-GRAY     (log-RGBCOLOR 190 190 190))
(define log-BROWN    (log-RGBCOLOR 165  42  42))
(define log-CORNSILK (log-RGBCOLOR 255 248 220))
(define log-IVORY    (log-RGBCOLOR 255 255 240))

;;;---------------------------------------------------------------------------

(define (setup-logging-heartbeat! parameter state)
  (%%interrupt-vector-set! 1 (logging-heartbeat! parameter state)))

(define (logging-heartbeat! parameter state)
  (lambda ()
    (declare (not interrupts-enabled))
    (let ((log-context (parameter)))
      (if log-context
          (let ((last-state (log-state log-context)))
            (log-transition log-context state)
            (##thread-heartbeat!)
            (log-transition log-context last-state))
        (##thread-heartbeat!)))))

(define log-setup
  (c-lambda (log-context*         ;; log context
             nonnull-char-string  ;; program name
             nonnull-char-string  ;; context name
             int                  ;; processor number
             int                  ;; number of states
             int)                 ;; maximum number of state transitions
            void
            "___EXT(___addref_string)(___arg2); /* prevent deallocation of string */
             ___EXT(___addref_string)(___arg3); /* prevent deallocation of string */
             log_setup(___arg1, ___arg2, ___arg3, ___arg4, ___arg5, ___arg6);"))

(define log-define-state
  (c-lambda (log-context*         ;; log context
             unsigned-int16       ;; state number
             nonnull-char-string  ;; state name
             int)                 ;; state color
            void
            "___EXT(___addref_string)(___arg3); /* prevent deallocation of string */
             log_define_state(___arg1, ___arg2, ___arg3, ___arg4);"))

(define log-start
  (c-lambda (log-context*         ;; log context
             unsigned-int16)      ;; starting state number
            void
            "log_start"))

(define log-state
  (c-lambda (log-context*)        ;; log context
            unsigned-int16
            "log_state"))

(define log-transition
  (c-lambda (log-context*         ;; log context
             unsigned-int16)      ;; state number
            void
            "log_transition"))

(define log-stop
  (c-lambda (log-context*)        ;; log context
            void
            "log_stop"))

(define log-cleanup
  (c-lambda (log-context*)        ;; log context
            void
            "log_cleanup")))
