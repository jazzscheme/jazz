
(define (arithmetic-shift n k)
  (if (negative? k)
      (quotient n (expt 2 (- k)))
      (* n (expt 2 k))))

(define (bitwise-and n m)
  (let lp ((i 0) (n n) (m m) (res 0))
    (if (and (zero? n) (zero? m))
        res
        (lp (+ i 1)
            (quotient n 2)
            (quotient m 2)
            (if (and (odd? n) (odd? m))
                (+ res (expt 2 i))
                res)))))

(define (bitwise-ior n m)
  (let lp ((i 0) (n n) (m m) (res 0))
    (if (and (zero? n) (zero? m))
        res
        (lp (+ i 1)
            (quotient n 2)
            (quotient m 2)
            (if (or (odd? n) (odd? m))
                (+ res (expt 2 i))
                res)))))

