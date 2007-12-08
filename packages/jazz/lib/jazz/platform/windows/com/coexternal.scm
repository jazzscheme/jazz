(include "debug-macro.scm")


(define (generate-in n)
  (string->symbol (string-append "in" (number->string n))))


(define (generate-p n)
  (string->symbol (string-append "p" (number->string n))))


(define (coparam-direction coparam)
  (car coparam))


(define (coparam-compiled-type coparam)
  (cadr coparam))


(define (coparam-type coparam)
  ((if (eq? 'in (coparam-direction coparam))
       cadr
     caddr)
   coparam))


(define (remove-null list)
  (cond ((null? list)
         '())
        ((null? (car list))
         (remove-null (cdr list)))
        (else
         (cons (car list) (remove-null (cdr list))))))


;;;
;;;; coexternal generation
;;;


(define (coexternal-input-parameter coparam n)
  (if (eq? 'out (coparam-direction coparam))
      '()
    (generate-in n)))


(define (coexternal-binding coparam n)
  (let* ((in-name (generate-in n))
         (p-name (generate-p n))
         (type (coparam-type coparam))
         (direction (coparam-direction coparam))
         (enreffer (symbol-append type '-enref))
         (encoder (symbol-append type '-encode))
         (foreign? (type-generate-foreign? (find-type-options type)))
         (inner (if foreign?
                    `(,encoder ,in-name)
                  in-name)))
    (case direction
      ((in)
       `(,p-name ,inner))
      ((in-out)
       `(,p-name (,enreffer ,inner)))
      ((out)
       `(,p-name (,enreffer #f))))))


(define (coexternal-input-cleanup coparam n)
  (if (not (eq? 'in (coparam-direction coparam)))
      '()
    (let* ((p-name (generate-p n))
           (type (coparam-type coparam))
           (freer (symbol-append type '-free))
           (foreign? (type-generate-foreign? (find-type-options type))))
      (if foreign?
          `(,freer ,p-name)
        '()))))


(define (coexternal-output-result coparam n)
  (if (eq? 'in (coparam-direction coparam))
      '()
    (let* ((p-name (generate-p n))
           (type (coparam-type coparam))
           (dereffer (symbol-append type '-deref))
           (decoder (symbol-append type '-decode))
           (foreign? (type-generate-foreign? (find-type-options type))))
      (if foreign?
          `(,decoder (,dereffer ,p-name))
        `(,dereffer ,p-name)))))


(define (generate-coexternal result-type signature)
  (let ((function (car signature))
        (coparams (cdr signature))
        (hresult? (eq? result-type 'VT_HRESULT)))
    (let* ((count (naturals 1 (+ 1 (length coparams))))
           (proc (lambda (proc) (map proc coparams count)))
           (output-result (remove-null (proc coexternal-output-result))))
      `(define (,function coptr ,@(remove-null (proc coexternal-input-parameter)))
         (let ,(proc coexternal-binding)
           (let ((res (,(symbol-append '%% function) coptr ,@(map generate-p count))))
             ,@(remove-null (proc coexternal-input-cleanup))
             ,(if hresult?
                  '(validate-hresult res)
                '(begin))
             ,(if hresult?
                  (if (= 1 (length output-result))
                      (car output-result)
                    `(list ,@output-result))
                (if (= 0 (length output-result))
                    'res
                  `(list res ,@output-result)))))))))


;;;
;;;; compiled generation
;;;


(define (compiled-coparam-prototype coparam)
  (let* ((cotype (coparam-compiled-type coparam))
         (options (find-type-options cotype))
         (pass-by (type-pass-by options))
         (used-ctype (if (eq? pass-by 'value)
                         (type-pure-ctype options)
                       (type-ctype options))))
    (string-append ", " used-ctype)))


(define (compiled-coparam-argument coparam n)
  (let* ((cotype (coparam-compiled-type coparam))
         (options (find-type-options cotype))
         (pass-by (type-pass-by options))
         (deref-string (if (eq? pass-by 'value) "*" "")))
    (string-append ", " deref-string "___arg" (number->string (+ n 2)))))


(define (generate-compiled-coexternal offset result-type signature)
  (let ((function (car signature))
        (coparams (cdr signature)))
  `(define ,(symbol-append '%% function)
     (c-lambda
      ,(cons 'VT_UNKNOWN (map coparam-compiled-type coparams))
      ,result-type
      ,(string-append
        "{typedef "
        (type-ctype (find-type-options result-type))
        " (*ProcType)(IUnknown*"
        (apply string-append (map compiled-coparam-prototype coparams))
        "); ProcType fn = (*(ProcType**)___arg1)["
        (number->string offset)
        "]; ___result = (*fn)(___arg1"
        (apply string-append (map compiled-coparam-argument coparams (naturals 0 (length coparams))))
        ");}")))))

;;;
;;;; coexternal-macro
;;;


(define (coexternal-macro offset result-type signature)
  (case copass
    ((c)
     (generate-compiled-coexternal offset result-type signature))
    ((s)
     (generate-coexternal result-type signature))
    (else
     '(begin))))


(define (cemt n)
  (case n
    ((0)
     (coexternal-macro 28 'VT_HRESULT '(foo-VT_BSTR (in VT_BSTR) (in-out VT_PTR VT_BSTR) (out VT_PTR VT_BSTR))))
    ((1)
     (coexternal-macro 15 'VT_HRESULT '(get-Workspaces (out VT_PTR VT_UNKNOWN))))
    ((2)
     (coexternal-macro 12 'VT_HRESULT '(get-Item (in VT_VARIANT) (out VT_PTR VT_UNKNOWN))))
    ((3)
     (coexternal-macro 22 'VT_HRESULT '(OpenDatabase (in VT_BSTR) (in VT_VARIANT) (in VT_VARIANT) (in VT_VARIANT) (out VT_PTR VT_UNKNOWN))))
    ((4)
     (coexternal-macro 22 'VT_HRESULT '(Close)))))
