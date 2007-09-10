(include "debug-macro.scm")
(include "cotype-macro.scm")
(include "costruct-macro.scm")
(include "coexternal-macro.scm")


(jazz.define-macro (c-pass body)
  (if (eq? copass 'c)
      body
    '(begin)))


(jazz.define-macro (s-pass body)
  (if (eq? copass 's)
      body
    '(begin)))


(jazz.define-macro (c-pass-define name . body)
  (if (eq? copass 'c)
      `(define ,name
         ,@body)
    '(begin)))


(jazz.define-macro (s-pass-define name . body)
  (if (eq? copass 's)
      `(define ,name
         ,@body)
    '(begin)))
