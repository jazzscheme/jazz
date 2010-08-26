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


;; Note that because statprof duration is a fixed quantum, keeping both
;; call count and call duration only makes sense because of FFI calls that
;; can span more than one statprof quantum beeing uninterruptible.


;;;
;;;; Interruption
;;;


(define *statprof-running?*
  #f)


(define (statprof-running?)
  *statprof-running?*)


(define profile-last-counter
  profile-user-data)

(define profile-last-counter-set!
  profile-user-data-set!)


(define (start-statprof profile)
  (profile-last-counter-set! profile (profiler-performance-counter))
  (set! *statprof-running?* #t)
  (if (%%not (%%fx= (profile-depth profile) 0))
      (%%interrupt-vector-set! 1 profile-heartbeat!)))


(define (stop-statprof profile)
  (%%interrupt-vector-set! 1 ##thread-heartbeat!)
  (set! *statprof-running?* #f)
  (if (%%fx= (profile-depth profile) 0)
      (profile-total-duration-set! profile (+ (profile-total-duration profile) (- (profiler-performance-counter) (profile-last-counter profile))))))


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
            (register-continuation (active-profile) cont))))))


(define (register-continuation profile cont)
  (define (duration)
    (let ((counter (profiler-performance-counter))
          (last-counter (profile-last-counter profile)))
      (profile-last-counter-set! profile counter)
      (- counter last-counter)))
  
  (let ((duration (max 1 (duration)))
        (stack (identify-call cont (profile-depth profile) '() '())))
    (profile-register-call profile stack duration)))


;;;
;;;; Statprof
;;;


(define statprof
  (make-profiler 'statprof start-statprof stop-statprof))


(default-profiler statprof)

;; quick fix so it stays compatible with the old approach
(active-profile (new-profile)))
