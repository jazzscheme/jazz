(include "debug-macro.scm")


(define-macro (s-define-if name test body)
  `(define (,name type subtype subname subsize)
     (if (and (eq? copass 's) ,test)
         ,body
       '(begin))))


(define-macro (c-define-if name test body)
  `(define (,name type subtype subname subsize)
     (if (and (eq? copass 'c) ,test)
         ,body
       '(begin))))


(define (generate-member-accessors type members)
  (apply append
         (map
          (lambda (declaration)
            (let ((subtype (car declaration))
                  (subname (cadr declaration))
                  (subsize (if (null? (cddr declaration)) #f (caddr declaration))))
              (map
               (lambda (proc) (proc type subtype subname subsize))
               (list MEMBER-get MEMBER-set! MEMBER-ref MEMBER-size))))
          members)))


;;;
;;;; MEMBER generator
;;;


(c-define-if MEMBER-get (not subsize)
 (let ((c-string (string-append "___result = ___arg1->" (symbol->string subname) ";")))
   `(define ,(symbol-append type '- subname '-get)
      (c-lambda (,type) ,subtype ,c-string))))


(c-define-if MEMBER-set! (not subsize)
 (let ((c-string (string-append "___arg1->" (symbol->string subname) " = ___arg2;")))
   `(define ,(symbol-append type '- subname '-set!)
      (c-lambda (,type ,subtype) void ,c-string))))


(c-define-if MEMBER-ref subsize
 (let ((c-string (string-append "___result = ___arg1->" (symbol->string subname) ";")))
   `(define ,(symbol-append type '- subname '-ref)
      (c-lambda (,type) ,(symbol-append subtype '+) ,c-string))))


(s-define-if MEMBER-size subsize
 `(define ,(symbol-append type '- subname '-size)
    ,subsize))


;;;
;;;; costruct-macro
;;;


(define (costruct-macro type options members)
  (let ((ctype-default (if (memq ctype: options)
                           '()
                         (list ctype: (symbol->string type))))
        (generate-foreign-default (if (memq generate-foreign?: options)
                           '()
                         (list generate-foreign?: #t))))
    `(begin
       (cotype ,type ,(append options ctype-default generate-foreign-default))
       ,@(generate-member-accessors type members))))


(define (csmt)
  (costruct-macro
   'GUID
   '(ctype: "GUID")
   '((DWORD Data1)
     (WORD Data2)
     (WORD Data3)
     (BYTE Data4 8))))
