

;;;
;;;; Load Stress Tests
;;;


(define cnt
  1000)


(define (cp)
  (let iter ((n 0))
    (write (list 'copy n))
    (newline)
    (copy-file "f.scm" (string-append "f" (number->string n) ".scm"))
    (if (< n cnt)
        (iter (+ n 1))))
  (display 'copy-done)
  (newline))


(define (co)
  (let iter ((n 0))
    (write (list 'compile n))
    (newline)
    (compile-file (string-append "f" (number->string n) ".scm"))
    (rename-file (string-append "f" (number->string n) ".o1") (string-append "default/f" (number->string n) ".o1"))
    (if (< n cnt)
        (iter (+ n 1))))
  (display 'compile-done)
  (newline))


(define (ld)
  (time
    (let iter ((n 0))
         (load (string-append "default/f" (number->string n)))
         (if (< n cnt)
             (iter (+ n 1)))))
  (display 'load-done)
  (newline))
