;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Termite
;;;


(module termite scheme


#; ( ;; experimental
(require (termite.implementation))

(export (termite.syntax (phase syntax)))


(native me)
(native send)
(native retrieve)

(native |termite#spawn|)
(native |termite#spawn-link|)

;; private
(native |termite#handle-exception-message|)
(native |termite#termite-exception?|)
(native |termite#pid?|)))
