(module test.performance.a


(declare (block)
         (standard-bindings)
         (not safe))


(define (f-separate s n)
  (if (%%fixnum= n 0)
      0
    (g-separate s (%%fixnum- n 1)))))
