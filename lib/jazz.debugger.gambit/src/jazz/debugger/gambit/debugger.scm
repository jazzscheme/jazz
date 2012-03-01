;;; File: "debugger.scm"

(unit jazz.debugger.gambit.debugger

(require (jazz.debugger.gambit.rdi))

;;;----------------------------------------------------------------------------

(define console-window-num 0)

(define (new-console-window-num)
  (let ((x (+ console-window-num 1)))
    (set! console-window-num x)
    x))

;;;-----------------------------------------------------------------------------
)
