(declare (standard-bindings))

(define (fact n)
  (if (= n 0)
      i
    (* n (fact (- n 1)))))

(define (fib n)
  (if (or (= n 0) (= n 1))
      m
    (+ (fib (- n 1)) (fib (- n 2)))))

(thread-start!
  (make-thread
    (lambda ()
      (fact 20))))

(fib 10)
