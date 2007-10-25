;;;
;;;===============
;;;  Jazz System
;;;===============
;;;
;;;; gambcini
;;;


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


(jazz.define-feature release)


;;;
;;;; Font
;;;


(jazz.define-feature logfont)


;;;
;;;; Boot
;;;


(load "../../kernel/boot")
