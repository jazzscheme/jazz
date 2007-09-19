(include "debug-macro.scm")


(jazz.define-macro (s-define-if name extra . body)
  `(define (,name options)
     (if (and (eq? copass 's) ,extra)
         (let ((type (car options)))
           ,@body)
       '(begin))))


(jazz.define-macro (c-define-if name extra . body)
  `(define (,name options)
     (if (and (eq? copass 'c) ,extra)
         (let ((type (car options)))
           ,@body)
       '(begin))))


(define (cotype-cast stype ctype-string)
  `(c-lambda (UNTYPED*) ,stype ,(string-append "___result = (" ctype-string ")___arg1;")))


(define (generate-declarative-type-options options)
  (map
   (lambda (proc) (proc options))
   (list TYPE-declare TYPE*-declare TYPE+-declare)))


(define (generate-imperative-type-options options)
  (map
   (lambda (proc) (proc options))
   (list TYPE-register TYPE-sizeof
         TYPE-cast TYPE-alloc TYPE-free TYPE-get TYPE-set! TYPE-encode TYPE-decode
         TYPE*-cast TYPE*-alloc TYPE*-free TYPE*-get TYPE*-set! TYPE-enref TYPE-deref
         TYPE+-cast TYPE+-alloc TYPE+-free TYPE+-get TYPE+-set! TYPE+-ref)))


;;;
;;;; TYPE generator
;;;


(s-define-if TYPE-register #t
 `(add-type-options ',type ',(cdr options)))


(c-define-if TYPE-declare #t
 (let ((native (type-native? options))
       (found (keyword-value ctype: options (symbol->string (type-type options)))))
   (cond (native
          `(c-define-type ,type ,native))
         ((type-untyped? options)
          `(c-define-type ,type (pointer ,found #f)))
         (else
          `(c-define-type ,type (pointer ,found))))))


(c-define-if TYPE-sizeof (or (type-generate-array? options) (not (type-native? options)))
 (let ((csizeof-string (type-csizeof options)))
   `(define ,(symbol-append type '-sizeof)
      ((c-lambda () unsigned-int ,(string-append "___result = " csizeof-string ";"))))))


(c-define-if TYPE-cast (type-generate-foreign? options)
 (let ((stype (type-stype options))
       (ctype-string (type-ctype options)))
   `(define ,(symbol-append type '-cast)
      ,(cotype-cast stype ctype-string))))


(s-define-if TYPE-alloc (eq? #t (type-generate-foreign? options))
 (let ((sizeof (symbol-append type '-sizeof))
       (cast (symbol-append type '-cast)))
   `(define ,(symbol-append type '-alloc)
      (lambda () (,cast (UNTYPED*-alloc ,sizeof))))))


(s-define-if TYPE-free (eq? #t (type-generate-foreign? options))
 `(define ,(symbol-append type '-free)
    UNTYPED*-free))


(s-define-if TYPE-get #f ;must-be-manually-defined-for-foreign->native-conversion
 `(define ,(symbol-append type '-get)
    (c-lambda (TYPE) NATIVE "___result = ___arg1;")))


(s-define-if TYPE-set! #f ;must-be-manually-defined-for-foreign->native-conversion
 `(define ,(symbol-append type '-set!)
    (c-lambda (TYPE NATIVE) void "___arg1 = ___arg2;")))


(s-define-if TYPE-encode (eq? #t (type-generate-foreign? options))
 (let ((alloc (symbol-append type '-alloc))
       (setter (symbol-append type '-set!)))
   `(define ,(symbol-append type '-encode)
      (lambda (data) (let ((foreign (,alloc))) (,setter foreign data) foreign)))))


(s-define-if TYPE-decode (eq? #t (type-generate-foreign? options))
 (let ((free (symbol-append type '-free))
       (getter (symbol-append type '-get)))
   `(define ,(symbol-append type '-decode)
      (lambda (foreign) (let ((data (,getter foreign))) (,free foreign) data)))))


(c-define-if TYPE*-declare (type-generate-pointer? options)
 `(c-define-type ,(symbol-append type '*) (pointer ,type)))


(c-define-if TYPE*-cast (type-generate-pointer? options)
 (let ((stype* (type-stype* options))
       (ctype*-string (type-ctype* options)))
   `(define ,(symbol-append type '*-cast)
      ,(cotype-cast stype* ctype*-string))))


(s-define-if TYPE*-alloc (type-generate-pointer? options)
 (let ((cast (symbol-append type '*-cast)))
   `(define ,(symbol-append type '*-alloc)
      (lambda () (,cast (VOID*-alloc))))))


(s-define-if TYPE*-free (type-generate-pointer? options)
  `(define ,(symbol-append type '*-free)
     UNTYPED*-free))


(c-define-if TYPE*-get (type-generate-pointer? options)
 (let ((stype (type-stype options))
       (stype* (type-stype* options))
       (ctype-string (type-ctype options)))
   `(define ,(symbol-append type '*-get)
      (c-lambda (,stype*) ,stype ,(string-append "___result = *(" ctype-string "*)___arg1;")))))


(c-define-if TYPE*-set! (type-generate-pointer? options)
 (let ((stype (type-stype options))
       (stype* (type-stype* options))
       (ctype-string (type-ctype options)))
   `(define ,(symbol-append type '*-set!)
      (c-lambda (,stype* ,stype) void ,(string-append "*(" ctype-string "*)___arg1 = ___arg2;")))))


(s-define-if TYPE-enref (type-generate-pointer? options)
 (let ((alloc (symbol-append type '*-alloc))
       (setter (symbol-append type '*-set!)))
   `(define ,(symbol-append type '-enref)
      (lambda (data) (let ((ptr (,alloc))) (,setter ptr data) ptr)))))


(s-define-if TYPE-deref (type-generate-pointer? options)
 (let ((free (symbol-append type '*-free))
       (getter (symbol-append type '*-get)))
   `(define ,(symbol-append type '-deref)
      (lambda (ptr) (let ((data (,getter ptr))) (,free ptr) data)))))


(c-define-if TYPE+-declare (type-generate-array? options)
 `(c-define-type ,(symbol-append type '+) (pointer ,type)))


(c-define-if TYPE+-cast (type-generate-array? options)
 (let ((stype+ (type-stype+ options))
       (ctype+-string (type-ctype+ options)))
   `(define ,(symbol-append type '+-cast)
      ,(cotype-cast stype+ ctype+-string))))


(s-define-if TYPE+-alloc (type-generate-array? options)
 (let ((sizeof (symbol-append type '-sizeof))
       (cast (symbol-append type '+-cast)))
   `(define ,(symbol-append type '+-alloc)
      (lambda (n) (,cast (UNTYPED+-alloc n ,sizeof))))))


(s-define-if TYPE+-free (type-generate-array? options)
  `(define ,(symbol-append type '+-free)
     UNTYPED+-free))


(define (TYPE+-get options)
  (let* ((type (car options))
         (getter+ (symbol-append type '+-get)))
    (cond
     ((and (type-generate-array? options) (eq? copass 'c) (type-native? options))
      (let ((stype+ (type-stype+ options)))
        `(define ,getter+
           (c-lambda (,stype+ unsigned-int) ,type "___result = ___arg1[___arg2];"))))
     ((and (type-generate-array? options) (eq? copass 's) (not (type-native? options)))
      (let ((reffer+ (symbol-append type '+-ref))
            (getter (symbol-append type '-get)))
        `(define ,getter+
           (lambda (array n) (,getter (,reffer+ array n))))))
     (else
      '(begin)))))


(define (TYPE+-set! options)
  (let* ((type (car options))
         (setter+ (symbol-append type '+-set!)))
    (cond
     ((and (type-generate-array? options) (eq? copass 'c) (type-native? options))
      (let ((stype+ (type-stype+ options)))
        `(define ,setter+
           (c-lambda (,stype+ unsigned-int ,type) void "___arg1[___arg2] = ___arg3;"))))
     ((and (type-generate-array? options) (eq? copass 's) (not (type-native? options)))
      (let ((reffer+ (symbol-append type '+-ref))
            (setter (symbol-append type '-set!)))
        `(define ,setter+
           (lambda (array n data) (,setter (,reffer+ array n) data)))))
     (else
      '(begin)))))


(c-define-if TYPE+-ref (and (type-generate-array? options) (not (type-native? options)))
 (let ((stype+ (type-stype+ options)))
   `(define ,(symbol-append type '+-set!)
      (c-lambda (,stype+ unsigned-int) ,type "___result = &___arg1[___arg2];"))))


;;;
;;;; cotype-macro
;;;


(define (cotype-macro type base-options)
  (let ((previously-declared-options (find-type-options type)))
    (add-type-options type base-options)
    (let ((options (find-type-options type)))
      `(begin
         ,@(if (not previously-declared-options)
               (generate-declarative-type-options options)
             '())
         ,@(generate-imperative-type-options options))
      )))


(define (codeclare-macro)
  `(begin
     ,@(map
        (lambda (options)
          `(begin
             ,@(generate-declarative-type-options options)))
        type-table)))
