(declare (standard-bindings))

(pp 'Welcome (jazz.get-console-port))

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
      (let ((port (jazz.get-console-port)))
        (display "Please enter a small integer: " port)
        (fact (read port))))))

(fib 10)
