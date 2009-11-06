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
  1)


;;;
;;;; Profile
;;;


(define (make-profile depth)
  (%%vector 'profile depth 0 0 (%%make-table test: equal?)))


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


;;;
;;;; Active
;;;


(define *profile*
  (make-profile (profiler-depth)))


(define (active-profile)
  *profile*)


(define (profile-new #!key (depth #f))
  (set! *profile* (make-profile (or depth profiler-depth))))


(define (profile-reset!)
  (set! *profile* (make-profile (profile-depth *profile*))))


;;;
;;;; Interruption
;;;


(define *profile-running?*
  #f)


(define (profile-start!)
  (%%interrupt-vector-set! 1 profile-heartbeat!)
  (set! *profile-running?* #t))

(define (profile-stop!)
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
  (let ((stack (identify-stack cont (profile-depth *profile*))))
    (if (%%not stack)
        (profile-unknown-set! *profile* (%%fx+ (profile-unknown *profile*) 1))
      (begin
        (profile-total-set! *profile* (%%fx+ (profile-total *profile*) 1))
        (let ((actual (%%table-ref (profile-calls *profile*) stack 0)))
          (%%table-set! (profile-calls *profile*) stack (%%fx+ actual 1)))))))


;;;
;;;; Stack
;;;


(define (identify-stack cont depth)
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
        (identify (%%continuation-next cont)
                  (%%cons (%%list creator location) stack)
                  (%%fx+ count 1)))))
  
  (identify cont '() 0)))
