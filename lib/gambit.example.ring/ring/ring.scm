(declare (standard-bindings))

(define (fact n)
  (if (= n 0)
      (append 1 2)
    (* n (fact (- n 1)))))

(fact 10)
