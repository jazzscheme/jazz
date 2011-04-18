(module fmt.test jazz

(export test-begin
        test-end)

(import scheme.syntax-rules)

(define (test-begin title)
  (write title)
  (newline))

(define (test-end)
  #f)

(syntax public (test form-src)
  (let ((value (second (source-code form-src)))
        (expr (third (source-code form-src))))
    (let ((message (->string (desourcify expr))))
      `(begin
         (display ,message)
         (newline)
         (if (not ,expr)
             (error ,message))))))

;; pretty printing

(define-macro (test-pretty str)
  (let ((sexp (with-input-from-string str read)))
    `(test ,str (fmt #f (pretty ',sexp)))))

#;
(define-syntax public test-pretty
  (syntax-rules ()
    ((test-pretty str)
     (let ((sexp (with-input-from-string str read)))
       (test str (fmt #f (pretty sexp))))))))
