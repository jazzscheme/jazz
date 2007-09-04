(module test.performance

(require (core.generic)
         (test.performance.a)
         (test.performance.b))

(jazz.define-class X jazz.Object () jazz.Object-Class allocate-x
  ())

(jazz.define-class-runtime X jazz.Object () jazz.Object-Class
  ())

(define (new-x)
  (allocate-x X)))

