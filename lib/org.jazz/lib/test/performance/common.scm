(module test.performance.common


(require (test.performance.syntax (phase syntax)))


(declare (block)
         (standard-bindings)
         (not safe))


(define (ff n)
  (if (%%fx> n 0)
      (begin
        (%%pair? n)
        (ff (%%fx- n 1)))))


(declare (block)
         (standard-bindings)
         (extended-bindings)
         (not safe))


(define (gg n)
  (if (%%fx> n 0)
      (begin
        (%%pair? n)
        (gg (%%fx- n 1)))))


;;;
;;;; Module-Block/Standard/NotSafe
;;;


(declare (block)
         (standard-bindings)
         (extended-bindings)
         (not safe))


(define (f-module-block/standard/not-safe s n)
  (if (%%fx= n 0)
      0
    (g-module-block/standard/not-safe s (%%fx- n 1))))


(define (g-module-block/standard/not-safe s n)
  (if (%%fx= n 0)
      0
    (f-module-block/standard/not-safe s (%%fx- n 1))))


;;;
;;;; Module-Block/Runtime/Safe
;;;


(declare (block)
         (standard-bindings)
         (extended-bindings)
         (not run-time-bindings)
         (safe))


(define (f-module-block/runtime/safe s n)
  (if (%%fx= n 0)
      0
    (g-module-block/runtime/safe s (%%fx- n 1))))


(define (g-module-block/runtime/safe s n)
  (if (%%fx= n 0)
      0
    (f-module-block/runtime/safe s (%%fx- n 1))))


;;;
;;;; Module-Separate/Runtime/Safe
;;;


(declare (separate)
         (not standard-bindings)
         (not extended-bindings)
         (run-time-bindings)
         (safe))


(define (f-module-separate/runtime/safe s n)
  (if (%%fx= n 0)
      0
    (g-module-separate/runtime/safe s (%%fx- n 1))))


(define (g-module-separate/runtime/safe s n)
  (if (%%fx= n 0)
      0
    (f-module-separate/runtime/safe s (%%fx- n 1))))


;;;
;;;; Module-Block/Runtime/NotSafe
;;;


(declare (block)
         (not standard-bindings)
         (not extended-bindings)
         (run-time-bindings)
         (not safe))


(define (f-module-block/runtime/notsafe s n)
  (if (%%fx= n 0)
      0
    (g-module-block/runtime/notsafe s (%%fx- n 1))))


(define (g-module-block/runtime/notsafe s n)
  (if (%%fx= n 0)
      0
    (f-module-block/runtime/notsafe s (%%fx- n 1))))


;;;
;;;; Module
;;;


(declare (block)
         (standard-bindings)
         (extended-bindings)
         (not safe))


(define (f-module s n)
  (if (%%fx= n 0)
      0
    (g-module s (%%fx- n 1))))


(define (g-module s n)
  (if (%%fx= n 0)
      0
    (f-module s (%%fx- n 1))))


;;;
;;;; Generic
;;;


(declare (block)
         (standard-bindings)
         (extended-bindings)
         (not safe))


(jazz.define-class X jazz.Object () jazz.Object-Class allocate-x
  ())

(X-implement)

(jazz.encapsulate-class X)


(define (new-x)
  (allocate-x X))


(jazz.define-generic (f-generic (X x) n))

(jazz.define-specific (f-generic (X x) n)
  (if (%%fx= n 0)
      0
    (g-generic x (%%fx- n 1))))


(jazz.define-generic (g-generic (X x) n))

(jazz.define-specific (g-generic (X x) n)
  (if (%%fx= n 0)
      0
    (f-generic x (%%fx- n 1))))


;;;
;;;; VTable
;;;


(Z-implement)


(jazz.define-virtual (f-vtable (Z z) n))
(jazz.define-virtual (g-vtable (Z z) n))


(define (new-z)
  (allocate-z Z))


(jazz.define-method (f-vtable (Z z) n)
  (if (%%fx= n 0)
      0
    (g-vtable z (%%fx- n 1))))


(jazz.define-method (g-vtable (Z z) n)
  (if (%%fx= n 0)
      0
    (f-vtable z (%%fx- n 1))))


(jazz.encapsulate-class Z)


(W-implement)


(jazz.define-method (f-vtable (W w) n)
  #f)


(jazz.define-virtual (h (W w)))


(jazz.define-method (h (W w))
  #f)


(jazz.encapsulate-class W)


;;;
;;;; Class-of
;;;


;;;
;;;; Is?
;;;
)
