;;; statprof.scm -- A statistical profiler for Gambit-C
;;;
;;; See the README file for license and usage information
;;;
;;; The Initial Developers are Guillaume Germain and Marc Feeley
;;;
;;; Contributor(s):
;;;   Guillaume Cartier


(unit protected statprof.implementation


(declare (proper-tail-calls))


;;;
;;;; Settings
;;;


(jazz.define-setting profiler-depth
  2)


;;;
;;;; Profile
;;;


(define (make-profile depth)
  (%%vector 'profile depth 0 0 (%%make-table test: equal?) 0))


(define (profile-depth profile)
  (%%vector-ref profile 1))

(define (profile-total profile)
  (%%vector-ref profile 2))

(define (profile-total-set! profile total)
  (%%vector-set! profile 2 total))

(define (profile-unknown profile)
  (%%vector-ref profile 3))

(define (profile-unknown-set! profile unknown)
  (%%vector-set! profile 3 unknown))

(define (profile-calls profile)
  (%%vector-ref profile 4))

(define (profile-last-counter profile)
  (%%vector-ref profile 5))

(define (profile-last-counter-set! profile counter)
  (%%vector-set! profile 5 counter))


;;;
;;;; Active
;;;


(define *profile*
  (make-profile (profiler-depth)))


(define (active-profile)
  *profile*)


(define (new-profile #!key (depth #f))
  (set! *profile* (make-profile (or depth profiler-depth))))


(define (reset-profile)
  (set! *profile* (make-profile (profile-depth *profile*))))


;;;
;;;; Counter
;;;


(define (secs->msecs x)
  (inexact->exact (round (* x 1000))))


(define get-performance-frequency
  (lambda ()
    1000))


(define get-performance-counter
  (lambda ()
    (secs->msecs (real-time))))


;;;
;;;; Interruption
;;;


(define *profile-running?*
  #f)


(define (start-profile)
  (profile-last-counter-set! *profile* (get-performance-counter))
  (%%interrupt-vector-set! 1 profile-heartbeat!)
  (set! *profile-running?* #t))


(define (stop-profile)
  (%%interrupt-vector-set! 1 ##thread-heartbeat!)
  (set! *profile-running?* #f))


(define (profile-running?)
  *profile-running?*)


;;;
;;;; Heartbeat
;;;


(define *in-profile-heartbeat?*
  (make-parameter #f))


(define (profile-heartbeat!)
  (declare (not interrupts-enabled))
  (if (%%not (*in-profile-heartbeat?*))
      (%%continuation-capture
        (lambda (cont)
          (parameterize ((*in-profile-heartbeat?* #t))
            (##thread-heartbeat!)
            (register-continuation cont))))))


(define (register-continuation cont)
  (define (duration)
    (let ((counter (get-performance-counter))
          (last-counter (profile-last-counter *profile*)))
      (profile-last-counter-set! *profile* counter)
      (- counter last-counter)))
  
  (let ((duration (max 1 (duration)))
        (stack (identify-stack cont (profile-depth *profile*))))
    (if (%%not stack)
        (profile-unknown-set! *profile* (+ (profile-unknown *profile*) duration))
      (begin
        (profile-total-set! *profile* (+ (profile-total *profile*) duration))
        (let ((actual (%%table-ref (profile-calls *profile*) stack 0)))
          (%%table-set! (profile-calls *profile*) stack (+ actual duration)))))))


;;;
;;;; Stack
;;;


(define (identify-stack cont depth)
  (define (continuation-next-distinct cont creator)
    (let ((creator-name (%%procedure-name creator)))
      (let loop ((current-cont (%%continuation-next cont)))
           (if current-cont
               (let ((current-creator (%%continuation-creator current-cont)))
                 (if (%%eq? creator-name (%%procedure-name current-creator))
                     (loop (%%continuation-next current-cont))
                   current-cont))
             #f))))
  
  (define (identify-location locat)
    (if locat
        (let ((container (%%locat-container locat)))
          (if container
              (let ((filepos (%%position->filepos (%%locat-position locat))))
                (let ((line (%%filepos-line filepos))
                      (col (%%filepos-col filepos)))
                  (%%list container line col)))
            #f))
      #f))
  
  (define (identify cont stack count)
    (if (or (%%not cont) (and depth (%%fx>= count depth)))
        stack
      (let ((creator (%%continuation-creator cont))
            (location (identify-location (%%continuation-locat cont))))
        (identify (continuation-next-distinct cont creator)
                  (%%cons (%%list creator location) stack)
                  (%%fx+ count 1)))))
  
  (identify cont '() 0)))
