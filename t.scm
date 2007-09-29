(define (f1 x)
  (+ x x))

(define (f2 x)
  (fx+ x x))

(define (f3 x)
  (##fx+ x x))

(declare (block)
         (standard-bindings)
         (extended-bindings)
         (not safe))

(define (f5 x)
  (+ x x))

(define (f6 x)
  (fx+ x x))

(define (f7 x)
  (##fx+ x x))
