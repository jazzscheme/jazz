
(module fmt.implementation.string-ports scheme

(export call-with-output-string2)

;; quick rename to test
(define (call-with-output-string2 proc)
  (let ((p (open-output-string)))
    (proc p)
    (get-output-string p))))
