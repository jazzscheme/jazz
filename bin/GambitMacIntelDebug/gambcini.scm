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


(jazz.define-feature mac)


;;;
;;;; Processor
;;;


(jazz.define-feature intel)


;;;
;;;; Safety
;;;


(jazz.define-feature debug)


;;;
;;;; Font
;;;


(jazz.define-feature freetype)


;;;
;;;; Boot
;;;


(load "../../kernel/boot")
