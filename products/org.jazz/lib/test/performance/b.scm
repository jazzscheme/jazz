(module test.performance.b

(define (g x n)
  (if (%%fixnum= n 0)
      0
    (f x (%%fixnum- n 1))))

(jazz.define-generic (gg (X x) n))

(jazz.define-specific (gg (X x) n)
  (if (%%fixnum= n 0)
      0
    (ff x (%%fixnum- n 1)))))
