;;;===========================================================================

;;; File: "demo-scm.scm"

;;; Copyright (c) 2013 by Marc Feeley, All Rights Reserved.

;;;===========================================================================

(module gambit.log.implementation.demo-scm jazz

(import (gambit.log))

(define global-mutex (make-mutex))

(define working   0)
(define waiting   1)
(define locked    2)
(define nb-states 3)

(define (new-log-context name i)
  (let ((log-context (log-context-alloc)))

    (log-setup log-context
               "demo-scm"
               name
               i
               nb-states
               1000000)

    (log-define-state log-context working "working"  log-GREEN)
    (log-define-state log-context waiting "waiting"  log-RED)
    (log-define-state log-context locked  "locked"   log-BLUE)

    log-context))

(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

(define count 0)

(define (create-thread log-ctx)
  (make-thread
   (lambda ()
     (let loop ((i 0))
       (if (< i 20)
           (begin

             (log-transition log-ctx waiting)

             (mutex-lock! global-mutex)

             (log-transition log-ctx locked)

             (set! count (+ count 1))
             (fib 22) ;; waste some time

             (log-transition log-ctx working)

             (mutex-unlock! global-mutex)

             (loop (+ i 1))))))))

(define log-context0 (new-log-context "thread0" 0)) ;; for thread p0
(define log-context1 (new-log-context "thread1" 1)) ;; for thread p1

(define p0 (create-thread log-context0))
(define p1 (create-thread log-context1))

(log-start log-context0 working)
(log-start log-context1 working)

(thread-start! p0)
(thread-start! p1)

(thread-join! p0)
(thread-join! p1)

(log-stop log-context0) ;; writes file demo-scm.log000
(log-stop log-context1) ;; writes file demo-scm.log001

(log-cleanup log-context0)
(log-cleanup log-context1))
