(module test.performance.a

(define (a x n)
  (if (%%fixnum= n 0)
      0
    (b x (%%fixnum- n 1))))

(jazz.define-generic (ff (X x) n))

(jazz.define-specific (ff (X x) n)
  (if (%%fixnum= n 0)
      0
    (gg x (%%fixnum- n 1)))))
