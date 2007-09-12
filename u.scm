(define-macro (lib . body)
  `(begin
     ,@body))

(lib
  
  (define (f)
    (g 2))
  
  (define (g x)
    (h x (+ x x)))
  
  (define (h x y)
    (car x)))

(##define-syntax lib2
  (lambda (src)
    (let ((code (##source-code src)))
      (let ((body (cdr code)))
        `(begin
           ,@(map (lambda (form-src)
                    (let ((form (##desourcify form-src)))
                      (##sourcify form form-src)))
                  body))))))

(lib2
  
  (define (f2)
           (g2 2))
  
  (define (g2 x)
    (h2 x (+ x x)))
  
  (define (h2 x y)
    (car x)))
