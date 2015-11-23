;;;
;;;; Exception
;;;

(define (with-exception-filter filter catcher thunk)
  (let ((previous-handler (current-exception-handler)))
    (continuation-capture
      (lambda (catcher-cont)
        (with-exception-handler
          (lambda (exc)
            (if (with-exception-handler
                  (lambda (filter-exc)
                    (continuation-graft catcher-cont
                      (lambda ()
                        (previous-handler filter-exc))))
                  (lambda ()
                    (filter exc)))
                (continuation-graft catcher-cont
                  (lambda ()
                    (catcher exc)))
              (previous-handler exc)))
          thunk)))))

(define (make-exception)
  (list 'exception))

(define (exception? obj)
  (equal? obj '(exception)))

(define-macro (catch filter/catcher . body)
  `(with-exception-filter ,(car filter/catcher) (lambda (,(cadr filter/catcher)) ,@(cddr filter/catcher))
     (lambda ()
       ,@body)))

(define (throw exception)
  (raise exception))

;;;
;;;; Processing
;;;

(c-declare #<<c-end
    bool exit_processing = false;
c-end
)

(define set-exit-processing
  (c-lambda (bool) void
    #<<c-end
    exit_processing = ___arg1;
c-end
))

(define catched-exception
  #f)

(define-macro (c-processing signature result-type #!optional (c-name-or-code #f))
  (let ((name (car signature))
        (parameters (cdr signature)))
    (let ((c-name (string->symbol (string-append (symbol->string name) "$c"))))
      `(begin
         (define ,c-name
           (c-lambda ,parameters ,result-type ,(or c-name-or-code (symbol->string name))))
         (define (,name . rest)
           (with-processing ',name
             (lambda ()
               (apply ,c-name rest))))))))

(define inside-processing
  (make-parameter #f))

(define (with-processing name thunk)
  (let ((result (parameterize ((inside-processing name))
                  (thunk))))
    (if catched-exception
        (throw catched-exception)
      result)))

;;;
;;;; Callback
;;;

(define-macro (c-callback signature parameter-types result-type c-name scope . body)
  `(c-define ,signature ,parameter-types ,result-type ,c-name ,scope
     (with-callback ',(car signature)
       (lambda ()
         ,@body))))

(define (with-callback name thunk)
  (cond ((inside-processing)
         (let ((returned? #f))
           (dynamic-wind
             (lambda ()
               #f)
             (lambda ()
               (set! catched-exception #f)
               (set-exit-processing #f)
               (let ((result (catch (exception? exc
                                      (set! catched-exception exc)
                                      (set-exit-processing #t))
                               (parameterize ((inside-processing #f))
                                 (thunk)))))
                 (set! returned? #t)
                 result))
             (lambda ()
               (if (not returned?)
                   (pp (list name 'did 'not 'return)))))))
        (else
         (pp (list "Callback called outside processing:" name))
         (exit))))
