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


(define jazz.safety-level
  (make-parameter 'debug))


;;;
;;;; Boot
;;;


(load "../../kernel/boot")
