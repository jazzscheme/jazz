
(jazz.define-macro (echo-macro text)
  (let ((zz text))
    (write zz)
    (newline)
    zz))

(jazz.define-macro (comment-out . rest)
  '(begin))