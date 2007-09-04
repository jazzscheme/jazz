
(define-macro (echo-macro text)
  (let ((zz text))
    (write zz)
    (newline)
    zz))

(define-macro (comment-out . rest)
  '(begin))