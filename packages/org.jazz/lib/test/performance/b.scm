(module test.performance.b


(declare (block)
         (standard-bindings)
         (not safe))


(define (g-separate s n)
  (if (%%fixnum= n 0)
      0
    (f-separate s (%%fixnum- n 1)))))
