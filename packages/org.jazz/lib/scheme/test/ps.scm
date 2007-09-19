(library scheme.test.ps scheme

(export toto)

(define-macro (toto)
  (expand-toto))

(define (expand-toto)
  `(tutu)))
