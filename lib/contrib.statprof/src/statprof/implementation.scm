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


(define heartbeat-interval 0.001)


(define (start-statprof profile)
  (##heartbeat-interval-set! heartbeat-interval)
  (set! *statprof-running?* #t)
  (%%interrupt-vector-set! 1 profile-heartbeat!))


(define (stop-statprof profile)
  (%%interrupt-vector-set! 1 ##thread-heartbeat!)
  (set! *statprof-running?* #f))


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
            (let ((profile (active-profile)))
              (profile-register-call profile
                                     (identify-call cont
                                                    (profile-depth profile)
                                                    (profile-profiler profile))
                                     0)))))))


;;;
;;;; Statprof
;;;


(define statprof
  (make-profiler 'statprof start-statprof stop-statprof 2)))
