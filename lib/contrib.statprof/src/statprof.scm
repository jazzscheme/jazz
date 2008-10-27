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


(define (make-profile)
  (##vector 'profile 0 0 (make-table test: equal?)))


(define (profile-total profile)
  (##vector-ref profile 1))

(define (profile-total-set! profile total)
  (##vector-set! profile 1 total))

(define (profile-unknown profile)
  (##vector-ref profile 2))

(define (profile-unknown-set! profile unknown)
  (##vector-set! profile 2 unknown))

(define (profile-calls profile)
  (##vector-ref profile 3))


;;;
;;;; Active
;;;


(define *profile*
  (make-profile))


(define (active-profile)
  *profile*)


(define (profile-reset!)
  (set! *profile* (make-profile)))


;;;
;;;; Interruption
;;;

(define *profile-running?*
  #f)


(define (profile-start!)
  (##interrupt-vector-set! 1 profile-heartbeat!)
  (set! *profile-running?* #t))

(define (profile-stop!)
  (##interrupt-vector-set! 1 ##thread-heartbeat!)
  (set! *profile-running?* #f))


(define (profile-running?)
  *profile-running?*)


;;;
;;;; Heartbeat
;;;


(define (profile-heartbeat!)
  (##continuation-capture
    (lambda (cont)
      (##thread-heartbeat!)
      (register-continuation cont))))


(define (register-continuation cont)
  (let ((location (identify-continuation cont)))
    (if (not location)
        (profile-unknown-set! *profile* (+ (profile-unknown *profile*) 1))
      (begin
        (profile-total-set! *profile* (+ (profile-total *profile*) 1))
        (let ((actual (table-ref (profile-calls *profile*) location 0)))
          (table-set! (profile-calls *profile*) location (##fx+ actual 1)))))))


;;;
;;;; Location
;;;


;; As an improvement, use ##continuation-creator and ##procedure-friendly-name
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
  
  (or (continuation-location cont)
      (let ((next (##continuation-next cont)))
        (if (##not next)
            #f
          (identify-continuation next))))))
