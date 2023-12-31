;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Termite Test
;;;


(module termite.test scheme


#; ( ;; experimental
(import (termite))


(let ((me (me)))
  (spawn
    (lambda ()
      (send me "Hello, world!"))))

(debug (retrieve))

(define better-pong-server
  (spawn
    (lambda ()
      (let loop ()
           (recv
             ((from 'ping) ; pattern to match
              (where (pid? from)) ; constraint
              (send from 'pong))) ; action
           (loop)))))))
