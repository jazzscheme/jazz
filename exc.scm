(define (cartier-catcher handler thunk)
  ;; Calls "thunk" and returns whatever "thunk" returns, unless
  ;; it raises an exception. In that case the handler is called
  ;; with 3 arguments:
  ;; 1 - the exception that was raised
  ;; 2 - the propagate procedure
  ;; 3 - the debug procedure
  (##continuation-capture
    (lambda (cartier-catcher-cont)
      (with-exception-handler
        (lambda (exc)
          (##continuation-capture
            (lambda (raise-cont)
              (##continuation-graft
                cartier-catcher-cont
                (lambda ()
                  (handler exc
                           (lambda ()
                             (let ((eh (current-exception-handler)))
                               (##continuation-graft
                                 raise-cont
                                 (lambda () (eh exc)))))
                           (lambda ()
                             (##display-exception-in-context
                               exc
                               raise-cont
                               (repl-output-port))
                             (##continuation-graft
                               raise-cont
                               ##repl))))))))
        thunk))))

(define (f x)
  (cartier-catcher
    (lambda (exc propagate debug)
      (debug)) ;; debug any uncaught exception
    (lambda ()
      (+ 111 (g x)))))

(define (g x)
  (cartier-catcher
    (lambda (exc propagate debug)
      (if (divide-by-zero-exception? exc)
          (begin
            (pp "divide by zero!")
            1000000)
        (propagate)))
    (lambda ()
      (+ 222 (h x)))))

(define (h x)
  (cartier-catcher
    (lambda (exc propagate debug)
      (if (unbound-global-exception? exc)
          (begin
            (pp "unbound global!")
            2000000)
        (propagate)))
    (lambda ()
      (+ 444 (x)))))

;; test it:

(pp (f (lambda () 0)))
(pp (f (lambda () xxx)))
(pp (f (lambda () (/ 1 0))))
(pp (f (lambda () (car 5))))
