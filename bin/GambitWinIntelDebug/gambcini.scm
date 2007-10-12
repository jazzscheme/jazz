;;;
;;;===============
;;;  Jazz System
;;;===============
;;;
;;;; gambcini
;;;


(generate-proper-tail-calls #f)
(display-environment-set! #t)


;;;
;;;; Features
;;;


(cond-expand
  (gambit
    (define-macro (jazz.define-feature feature)
      `(define-cond-expand-feature ,feature)))
  (else))


;;;
;;;; System
;;;


;; defined by the underlying Scheme system


;;;
;;;; Platform
;;;


(jazz.define-feature windows)


;;;
;;;; Processor
;;;


(jazz.define-feature intel)


;;;
;;;; Safety
;;;


(jazz.define-feature debug)


;;;
;;;; Boot
;;;


(load "../../kernel/boot")
