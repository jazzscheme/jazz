(declare (run-time-bindings)
         (safe))

(define (f-run-time-bindings/safe x)
  (car x))


(declare (not run-time-bindings)
         (safe))

(define (f-not-run-time-bindings/safe x)
  (car x))


(declare (run-time-bindings)
         (not safe))

(define (f-run-time-bindings/not-safe x)
  (car x))


(declare (not run-time-bindings)
         (not safe))

(define (f-not-run-time-bindings/not-safe x)
  (car x))


(declare (standard-bindings)
         (not run-time-bindings)
         (safe))

(define (f-standard-bindings/not-run-time-bindings/not-safe x)
  (car x)
  (##pair? x))


(declare (standard-bindings)
         (run-time-bindings)
         (safe))

(define (f-standard-bindings/run-time-bindings/not-safe x)
  (car x)
  (##pair? x))


(declare (standard-bindings)
         (extended-bindings)
         (safe))

(define (f-standard-bindings/extended-bindings/not-safe x)
  (car x)
  (##pair? x))


(declare (block))

(define (f-block)
  (g-block))

(define (g-block)
  (f-block))


(declare (separate))

(define (f-separate)
  (g-separate))

(define (g-separate)
  (f-separate))
