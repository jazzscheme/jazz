;;;
;;;; Interpreter
;;;


;(define (t)
;  (let ((src '(+ 1 2)))
;    (set! % (##compile-top ##interaction-cte (##sourcify src (##make-source #f #f))))
;    #!void))


;(define (t1)
;  (let ((src '(lambda () 2)))
;    (set! % (##compile-top ##interaction-cte (##sourcify src (##make-source #f #f))))
;    #!void))


(define c2 #f)
(define c3 #f)
(define c4 #f)


(define (lt)
  (load "test"))


(define (sc)
  (set! c2 (t2))
  (set! c3 (t3))
  (set! c4 (t4))
  (c2))


(define (cc)
  (##continuation-capture
    (lambda (cont)
      (set! % cont))))


(define (dc)
  (let loop ((cont %))
    (if cont
        (let* ((creator (##continuation-creator cont))
               (name (and creator (##procedure-name creator)))
               (ret (##continuation-ret cont))
               (locals (##continuation-locals cont)))
          (write (list name cont ret (and name (not (memq name '(cc))) locals (map car locals))))
          (newline)
          (loop (##continuation-next cont))))))
