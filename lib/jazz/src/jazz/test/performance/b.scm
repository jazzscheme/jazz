(unit test.performance.b


(declare (block)
         (standard-bindings)
         (not safe))


(define (g-separate s n)
  (if (%%fx= n 0)
      0
    (f-separate s (%%fx- n 1)))))
