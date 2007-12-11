(define type-table
  '((VOID ctype: "void" generate-pointer?: #t generate-array?: #t)
    (UNTYPED ctype: "void" generate-pointer?: #t generate-array?: #t untyped?: #t)))


(define type-table-valid-options '(stype: stype*: ctype: ctype*: ctype+: generate-foreign?: generate-pointer?: generate-array?: untyped?: pass-by:))


(define (add-type-options type options)
  (let ((cotype (assoc type type-table))
        (options (apply list options)))
    (if cotype
        (set-cdr! cotype options)
      (set! type-table (cons (cons type options) type-table)))
    cotype))


(define (find-type-options type)
  (let ((cotype (assoc type type-table)))
    cotype))


(define (validate-type-options options)
  (cond ((null? options)
         #t)
        ((memq (car options) type-table-valid-options)
         (validate-type-options (cddr options)))
        (else
         (error "invalid cotype option" options))))


(define (type-type options)
  (car options))


(define (type-stype options)
  (let ((stype (keyword-value stype: options (car options))))
    (or stype (error "invalid type" options))))


(define (type-stype* options)
  (let ((type (type-type options)))
    (keyword-value stype*: options (symbol-append type '*))))


(define (type-stype+ options)
  (let ((type (type-type options)))
    (symbol-append type '+)))


(define (type-pure-ctype options)
  (keyword-value ctype: options (symbol->string (type-type options))))


(define (type-csizeof options)
  (string-append "sizeof(" (type-pure-ctype options) ")"))


(define (type-ctype options)
  (if (type-native? options)
      (type-pure-ctype options)
    (string-append (type-pure-ctype options) "*")))


(define (type-ctype* options)
  (let ((default (string-append (type-ctype options) "*")))
    (keyword-value ctype*: options default)))


(define (type-ctype+ options)
  (let ((default (string-append (type-ctype options) "*")))
    (keyword-value ctype+: options default)))


(define (type-foreign? options)
  (not (keyword-value stype: options #f)))


(define (type-native? options)
  (keyword-value stype: options #f))


(define (type-generate-foreign? options)
  (keyword-value generate-foreign?: options #f))


(define (type-generate-pointer? options)
  (keyword-value generate-pointer?: options #f))


(define (type-generate-array? options)
  (keyword-value generate-array?: options #f))


(define (type-untyped? options)
  (keyword-value untyped?: options #f))


(define (type-pass-by options)
  (keyword-value pass-by: options #f))
