;;; statprof.scm -- A statistical profiler for Gambit-C
;;;
;;; See the README file for license and usage information
;;;
;;; The Initial Developers are Guillaume Germain and Marc Feeley
;;;
;;; Contributor(s):
;;;   Guillaume Cartier


(module statprof


(declare (proper-tail-calls))


;;;
;;;; Profile
;;;


(jazz.define-macro (%%make-profile)
  `(%%vector 'profile 0 (make-table test: equal?)))


(jazz.define-macro (%%profile-unknown profile)
  `(%%vector-ref ,profile 1))

(jazz.define-macro (%%profile-unknown-set! profile unknown)
  `(%%vector-set! ,profile 1 ,unknown))

(jazz.define-macro (%%profile-calls profile)
  `(%%vector-ref ,profile 2))


;;;
;;;; Interruption
;;;


(define *profile*
  (%%make-profile))


(define (profile-start! profile)
  (##interrupt-vector-set! 1 profile-heartbeat!))

(define (profile-stop!)
  (##interrupt-vector-set! 1 ##thread-heartbeat!))

(define (profile-reset!)
  (set! *profile* (%%make-profile)))


;;;
;;;; Heartbeat
;;;


(define (profile-heartbeat!)
  (##continuation-capture
    (lambda (cont)
      (##thread-heartbeat!)
      (register-continuation cont))))


(define (register-continuation cont)
  (let ((call (identify-continuation cont)))
    (if (not call)
        (%%profile-unknown-set! *profile* (%%profile-unknown *profile*))
      (let ((actual (table-ref (%%profile-calls *profile*) call 0)))
        (table-set! (%%profile-calls *profile*) call (##fx+ actual 1))))))


;;;
;;;; Call
;;;


;; As an improvement, we could use ##continuation-parent and ##object->global-var->identifier
;; to identify more precisely where the code was in the ##continuation-next ... continuation
(define (identify-continuation cont)
  
  (define (continuation-location cont)
    (let ((locat (##continuation-locat cont)))
      (if locat
          (let ((file (##container->file (##locat-container locat))))
            (if file
                (let* ((filepos (##position->filepos (##locat-position locat)))
                       (line (##filepos-line filepos))
                       (col (##filepos-col filepos)))
                  (list file line col))
              #f))
        #f)))
  
  (continuation-location cont)
  #;
  (or (continuation-location cont)
      (let ((next (##continuation-next cont)))
        (if (##not next)
            #f
          (identify-continuation next)))))


;;;
;;;; Location
;;;


)
