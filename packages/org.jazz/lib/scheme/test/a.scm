(library scheme.test.a scheme

(import (scheme.test.p))

(display (if 1 1 2))

(display (+ 1 (* 2 (- (/ 3)))))
(newline)

(define (f1)
  (define (goo)
    (foo))
  (define (foo)
    2)
  (goo))

(define (f2)
  (letrec ((foo
             (lambda ()
               2))
           (goo
             (lambda ()
               (foo))))
    (goo)))

(display (list 'f1 '= (f1)))
(newline)

(display (list 'f2 '= (f2)))
(newline)

(define (foo)
  (toto))

(display (list 'foo '= (foo)))
(newline))
