;;;
;;;; Internal
;;;


(define (ttt)
  (define a 1)
  (define b (+ a a))
  b)


(define (uuu)
  (define a #!void)
  (define b #!void)
  (set! a 1)
  (set! b (+ a a))
  b)


(define (vvv)
  (define (f1)
    (define (f11)
      (define (f111)
        (list 'f111 (f112) (f12) (f2)))
      (define (f111bad)
        (list 'f111 (f112) (f12) (f2) (f21)))
      (define (f112)
        'f112)
      (f111))
    (define (f12)
      'f12)
    (f11))
  (define (f2)
    (define (f21)
      'f21)
    'f2)
  (f1))


(define www
  (lambda ()
    (letrec ((f1
              (lambda ()
                (letrec ((f11
                          (lambda ()
                            (letrec ((f111
                                      (lambda ()
                                        (list 'f111 (f112) (f12) (f2))))
                                     (f111bad
                                      (lambda ()
                                        (list 'f111 (f112) (f12) (f2) (f21))))
                                     (f112
                                      (lambda ()
                                        'f112)))
                              (f111))))
                         (f12
                          (lambda ()
                            'f12)))
                  (f11))))
             (f2
              (lambda ()
                (letrec ((f21
                          (lambda ()
                            'f21)))
                  'f2))))
      (f1))))
