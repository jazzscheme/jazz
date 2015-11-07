(declare (standard-bindings)
         (extended-bindings)
         (safe))

(define VAR
  2)

(define (inc cnt)
  (let iter ((n cnt))
    (if (##fx> n 0)
        (begin
          (set! VAR (##fx+ VAR 1))
          (iter (##fx- n 1))))))
