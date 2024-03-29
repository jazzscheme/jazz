(module srfi-13.syntax scheme

(export let-string-start+end
        let-string-start+end2)

(import (jazz.dialect.syntax (phase syntax)))

(native private gensym)

;;; Support for START/END substring specs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This macro parses optional start/end arguments from arg lists, defaulting
;;; them to 0/(string-length s), and checks them for correctness.

#;
(define-syntax let-string-start+end
  (syntax-rules ()
    ((let-string-start+end (start end) proc s-exp args-exp body ...)
     (receive (start end) (string-parse-final-start+end proc s-exp args-exp)
       body ...))
    ((let-string-start+end (start end rest) proc s-exp args-exp body ...)
     (receive (rest start end) (string-parse-start+end proc s-exp args-exp)
       body ...))))

(define-syntax let-string-start+end
  (lambda (expr usage-env macro-env)
    (bind (_ variables proc s-exp args-exp . body) (desourcify-all expr)
      (case (length variables)
        ((2)
         (bind (start end) variables
           `(receive (,start ,end) (string-parse-final-start+end ,proc ,s-exp ,args-exp)
              ,@body)))
        ((3)
         (bind (start end rest) variables
           `(receive (,rest ,start ,end) (string-parse-start+end ,proc ,s-exp ,args-exp)
              ,@body)))))))

;;; This one parses out a *pair* of final start/end indices. 
;;; Not exported; for internal use.
#;
(define-syntax let-string-start+end2
  (syntax-rules ()
    ((l-s-s+e2 (start1 end1 start2 end2) proc s1 s2 args body ...)
     (let ((procv proc)) ; Make sure PROC is only evaluated once.
       (let-string-start+end (start1 end1 rest) procv s1 args
         (let-string-start+end (start2 end2) procv s2 rest
           body ...))))))

(define-syntax let-string-start+end2
  (lambda (expr usage-env macro-env)
    (bind (_ (start1 end1 start2 end2) proc s1 s2 args . body) (desourcify-all expr)
      (let ((procv (gensym 'procv))
            (rest (gensym 'rest)))
        `(let ((,procv ,proc)) ; Make sure PROC is only evaluated once.
           (let-string-start+end (,start1 ,end1 ,rest) ,procv ,s1 ,args
             (let-string-start+end (,start2 ,end2) ,procv ,s2 ,rest
               ,@body))))))))
