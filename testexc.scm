(define (f x)
  (foo (+ x x)))


(define (g x)
  (call/cc
    (lambda (return)
      (raise
        (call/cc
          (lambda (reraise)
            (with-exception-handler
              (lambda (exc)
                (write exc)
                (newline)
                (reraise exc))
              (lambda ()
                (return (foo (+ x x)))))))))))


(define (h x)
  (with-exception-catcher
    (lambda (exc)
      (write exc)
      (newline)
      (raise exc))
    (lambda ()
      (foo (+ x x)))))


(define (foo y)
  (bar y (* y y)))


(define (bar x y)
  (baz 1 2 x y))


(define (baz a b c d)
  (car 1))
