(module fmt.implementation.let-optionals scheme

(import scheme.syntax-rules)

(native private jazz:error)

;; (define-syntax let-optionals*
;;   (syntax-rules ()
;;     ((_ opt-ls () body ...)
;;      (let () body ...))
;;     ((_ (expr ...) vars body ...)
;;      (let ((tmp (expr ...)))
;;        (let-optionals* tmp vars body ...)))
;;     ((_ tmp ((var default) . rest) body ...)
;;      (let ((var (if (pair? tmp) (car tmp) default))
;;            (tmp2 (if (pair? tmp) (cdr tmp) '())))
;;        (let-optionals* tmp2 rest body ...)))
;;     ((_ tmp tail body ...)
;;      (let ((tail tmp))
;;        body ...))
;;     ))

(define-syntax public let-optionals*
  (syntax-rules ()
    ((_ opt-ls () . body)
     (let () . body))
    ((_ (op . args) vars . body)
     (let ((tmp (op . args)))
       (let-optionals* tmp vars . body)))
    ((_ tmp ((var default) . rest) . body)
     (let ((var (if (pair? tmp) (car tmp) default))
           (tmp2 (if (pair? tmp) (cdr tmp) '())))
       (let-optionals* tmp2 rest . body)))
    ((_ tmp tail . body)
     (let ((tail tmp))
       . body))
    )))
