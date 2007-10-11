(define f
  (lambda (x y)
    (+ x y)))

(f 2 3)


(define %
  #f)


(define (source-code source)
  source)


(define (compile source)
  (let ((code (source-code source)))
    code))


(define (run code . args)
  (apply (##vector-ref (##vector-ref (##vector-ref code 0) 5) 7) args))


(define (f2)
  (error "STEP")
  (list (f3 2)))

(define (f3 x)
  (let ((y (+ x x)))
    (if (= y y)
        (list (f4 x y)))))

(define (f4 x y)
  (list (cc))
  (error "REPL"))


(define (cc)
  (##continuation-capture
    (lambda (cont)
      (set! % cont))))


(define ##sub-apply2
  (let ()
    (##declare (not inline) (not interrupts-enabled) (environment-map))
    (lambda (%%code %%rte proc arg1 arg2)
      (##first-argument
       (proc arg1 arg2)
       %%code
       %%rte))))


(define (t2)
  (let ((%code 1)
        (%cte 2))
    (lambda ()
      (let ((x 2))
        (list %code %cte (##sub-apply2 111 222 c3 x (* x x)))
        (write (list '*** x))
        (newline)))))


(define (t3)
  (let ((%code 1)
        (%cte 2))
    (lambda (x y)
      (let ((z (+ x y)))
        (list %code %cte (c4 z))))))


(define (t4)
  (let ((%code 1)
        (%cte 2))
    (lambda (z)
      (let ((a (* z z)))
        (list %code %cte (cc))))))
