;; statprof.scm -- A statistical profiler for Gambit-C 4.0

;; See the README file for license and usage information.

;; $Id: statprof.scm,v 1.9 2005/03/14 07:35:49 guillaume Exp $


;; ----------------------------------------------------------------------------
;; Profiling & interruption handling

(module statprof

(declare (proper-tail-calls))

;; Buckets should probably be tables for better performance
(define *buckets* '())
(define *total* 0)
(define *unknown* 0)

(define (profile-start!)
  (##interrupt-vector-set! 1 profile-heartbeat!))

(define (profile-stop!)
  (##interrupt-vector-set! 1 ##thread-heartbeat!))

(define (profile-reset!)
  (set! *buckets* '())
  (set! *total* 0)
  (set! *unknown* 0))

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
  
  (or (continuation-location cont)
      (let ((next (##continuation-next cont)))
        (if (##not next)
            #f
          (identify-continuation next)))))

(define (profile-heartbeat!)
  (##continuation-capture
    (lambda (cont)
      (##thread-heartbeat!)
      (let ((id (identify-continuation cont)))
        (if (##not id)
            (set! *unknown* (##fx+ *unknown* 1))
          (let ((bucket (assoc (##car id) *buckets*)))
            (set! *total* (##fx+ *total* 1))
            (if (##not bucket)
                (begin
                  (set! *buckets* (##cons
                                    (##cons (##car id)
                                          ;; fixme: arbitrary hard limit
                                          ;; on the length of source
                                          ;; files
                                          (##make-vector 50000 0))
                                    *buckets*))
                  (set! bucket (##car *buckets*))))
            (vector-set! (##cdr bucket)
                         (##cadr id)
                         (##fx+ (vector-ref (##cdr bucket)
                                            (##cadr id))
                                1))))))))

;; ----------------------------------------------------------------------------
;; Function to generate an sexp report

(define (write-profile-report profile-name)
  (call-with-output-file profile-name
    (lambda (port)
      (pp
        (cons *total*
              (cons *unknown*
                    (map (lambda (bucket)
                           (let ((file (car bucket))
                                 (data (cdr bucket)))
                             (cons file
                                   (let iter ((n (- (vector-length data) 1))
                                              (lines '()))
                                     (if (>= n 0)
                                         (let ((count (vector-ref data n)))
                                           (if (= count 0)
                                               (iter (- n 1) lines)
                                             (iter (- n 1) (cons (list n count) lines))))
                                       lines)))))
                         *buckets*)))
        port)))))
