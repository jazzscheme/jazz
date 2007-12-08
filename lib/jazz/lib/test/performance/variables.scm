(declare (standard-bindings)
         (extended-bindings)
         (safe))

(define SEPARATE
  2)

(define (inc-separate cnt)
  (let iter ((n cnt))
    (if (> n 0)
        (begin
          (set! SEPARATE (+ SEPARATE 1))
          (iter (- n 1))))))

(declare (block)
         (standard-bindings)
         (extended-bindings)
         (safe))

(define BLOCK
  3)

(define (inc-block cnt)
  (let iter ((n cnt))
    (if (> n 0)
        (begin
          (set! BLOCK (+ BLOCK 1))
          (iter (- n 1))))))
