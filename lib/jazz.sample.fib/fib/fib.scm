(declare (block)
         (standard-bindings)
         (extended-bindings)
         (fixnum)
         (not safe))

(define (fib n)
  (if (or (= n 0) (= n 1))
      n
    (+ (fib (- n 1)) (fib (- n 2)))))

(time
  (let iter ((i 1))
    (write (list i (fib i)))
    (newline)
    (if (< i 36)
        (iter (+ i 1)))))
