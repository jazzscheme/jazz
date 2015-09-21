(declare (standard-bindings)
         (extended-bindings)
         (safe))

(define VAR
  2)

(define (inc cnt)
  (let iter ((n cnt))
    (if (##fixnum.> n 0)
        (begin
          (set! VAR (##fixnum.+ VAR 1))
          (iter (##fixnum.- n 1))))))
