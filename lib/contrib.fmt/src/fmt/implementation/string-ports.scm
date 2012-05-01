
(define (call-with-output-string proc)
  (let ((p (open-output-string)))
    (proc p)
    (get-output-string p)))
