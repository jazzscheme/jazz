(unit test.performance.a


(declare (block)
         (standard-bindings)
         (not safe))


(define (f-separate s n)
  (if (%%fx= n 0)
      0
    (g-separate s (%%fx- n 1)))))
