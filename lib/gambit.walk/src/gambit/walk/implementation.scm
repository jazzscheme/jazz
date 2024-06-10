;;; File: walk.scm

;;; Compile and run with: gsc walk;gsi walk

;;;
;;;; Walk
;;;

(unit protected gambit.walk.implementation

(declare (standard-bindings)
         (extended-bindings)
         (not safe)
         (proper-tail-calls)
         (optimize-dead-local-variables))

(include "~~lib/_gambit#.scm")

;;;
;;;; Symbol
;;;

(##define-macro (macro-symbol-name s)        `(macro-slot 0 ,s))
(##define-macro (macro-symbol-name-set! s x) `(macro-slot 0 ,s ,x))
(##define-macro (macro-symbol-hash s)        `(macro-slot 1 ,s))
(##define-macro (macro-symbol-hash-set! s x) `(macro-slot 1 ,s ,x))
(##define-macro (macro-symbol-next s)        `(macro-slot 2 ,s))
(##define-macro (macro-symbol-next-set! s x) `(macro-slot 2 ,s ,x))

;;;
;;;; Keyword
;;;

(##define-macro (macro-keyword-name k)        `(macro-slot 0 ,k))
(##define-macro (macro-keyword-name-set! k x) `(macro-slot 0 ,k ,x))
(##define-macro (macro-keyword-hash k)        `(macro-slot 1 ,k))
(##define-macro (macro-keyword-hash-set! k x) `(macro-slot 1 ,k ,x))
(##define-macro (macro-keyword-next k)        `(macro-slot 2 ,k))
(##define-macro (macro-keyword-next-set! k x) `(macro-slot 2 ,k ,x))

;;;
;;;; Foreign
;;;

(##define-macro (macro-foreign-tags f)         `(macro-slot 0 ,f))
(##define-macro (macro-foreign-tags-set! f x)  `(macro-slot 0 ,f ,x))

;;;
;;;; Kernel
;;;

;;; guard for unimplemented functions (will be integrated to _kernel.scm)

(define-prim (##continuation-frame cont)
  (let ((frame (##vector-ref cont 0)))
    (if (or (##eq? frame 0) (##frame? frame))
      frame
      (begin
        (##gc)
        (##continuation-frame cont)))))

(define (##frame-ret-set! frame val)
  (if (##not (##eq? (##frame-ret frame) val))
      (error "unimplemented ##frame-ret-set!")))

(define-prim (##frame-set! frame i val)
  (##declare (not interrupts-enabled))
  (##c-code #<<end-of-code

   int i = ___INT(___ARG2);
   ___SCMOBJ ra = ___FIELD(___ARG1,0);
   int fs;
   int link;

   if (ra == ___GSTATE->internal_return)
     ___RETI_GET_FS_LINK(___BODY_AS(___ARG1,___tSUBTYPED)[___FRAME_RETI_RA],fs,link)
   else
     ___RETN_GET_FS_LINK(ra,fs,link)

   ___BODY_AS(___ARG1,___tSUBTYPED)[fs-i+1] = ___ARG3;  /* what if i==link and frame is first in section???? */

   ___RESULT = ___VOID;

end-of-code

   frame
   i
   val))

;;;
;;;; Register
;;;

(define (##new-register)
  (##make-table 0 #f #f #f ##eq?))

(define (##register-ref table key default)
  (##table-ref table key default))

(define (##register-set! table key value)
  (##table-set! table key value))

(define (##register-length table)
  (##table-length table))

(define (##iterate-register table proc)
  (##table-for-each proc table))

(define (##register->table table)
  table)

;;;
;;;; Dispatcher
;;;

(define-macro (macro-case-type obj)
  `(let ((obj ,obj))
     (if (##not (##mem-allocated? obj))

         (macro-handle-type-atomic)

         (let ((subtype (##subtype obj)))

           (cond ((##fx= subtype (macro-subtype-pair))
                  ;; This case overlaps with ovector so it must be tested
                  ;; before ovector.
                  (macro-handle-type-simple
                   ##cons
                   (##car ##set-car!)
                   (##cdr ##set-cdr!)))
                    
                 ((macro-subtype-ovector? subtype)
                  (macro-handle-type-object-vector ovector))

                 ((##fx= subtype (macro-subtype-foreign))
                  ;; This case overlaps with bvector so it must be tested
                  ;; before bvector.
                  (macro-handle-type-mixed-vector
                   foreign
                    (##foreign-tags macro-foreign-tags-set!)))

                 ((macro-subtype-bvector? subtype)
                  (macro-handle-type-mixed-vector
                   bvector))

                 ((##fx= subtype (macro-subtype-symbol))
                  (macro-handle-type-mixed-vector
                   symbol
                   ;(macro-symbol-name macro-symbol-name-set!)
                   ;(macro-symbol-hash macro-symbol-hash-set!)
                   ;(macro-symbol-next macro-symbol-next-set!)
                   ))

                 ((##fx= subtype (macro-subtype-keyword))
                  (macro-handle-type-mixed-vector
                   keyword
                   (macro-keyword-name macro-keyword-name-set!)
                   (macro-keyword-hash macro-keyword-hash-set!)
                   (macro-keyword-next macro-keyword-next-set!)))

                 ((##fx= subtype (macro-subtype-frame))
                  ;; quick hack don't walk frames for now
                  (macro-handle-type-atomic)
                  
                  #;
                  (macro-handle-type-frame))

                 ((##fx= subtype (macro-subtype-continuation))
                  ;; quick hack as ##continuation-frame crashes
                  (macro-handle-type-atomic)
                  
                  #;
                  (
                  (##continuation-frame obj) ;; force frame into heap

                  (macro-handle-type-simple
                   macro-make-continuation
                   (macro-continuation-frame macro-continuation-frame-set!)
                   (macro-continuation-denv macro-continuation-denv-set!)))
                 )

                 ((##fx= subtype (macro-subtype-weak))
                  (if (##will? obj)

                      (macro-handle-type-simple
                       macro-make-will
                       (macro-will-testator macro-will-testator-set!)
                       (macro-will-action macro-will-action-set!))

                      ;;TODO: walking a gc-hash-table may be unreliable
                      (macro-handle-type-object-vector gc-hash-table)))

                 ((##fx= subtype (macro-subtype-procedure))
                  (if (##closure? obj)

                      (macro-handle-type-object-vector closure)

                      (macro-handle-type-atomic)))

                 ((##fx= subtype (macro-subtype-return))
                  (macro-handle-type-atomic))

                 ((##fx= subtype (macro-subtype-promise))
                  (macro-handle-type-object-vector promise))

                 (else
                  ;;TODO: this case is impossible (handle like error?)
                  (macro-handle-type-atomic)))))))

(define-macro (macro-walk-seq expr1 expr2)
  `(or ,expr1 ,expr2))

(define-macro (macro-walk-continue)
  `#f)

(define-macro (macro-walk-no-recursive-scan)
  `(macro-absent-obj))

(define-macro (macro-walk-object obj)
  `(let ()

     (define (walk-object container i obj)

       (define-macro (macro-handle-type-atomic)
         `(handle-type-atomic))

       (define (handle-type-atomic)
         (macro-walk-continue))

       (define-macro (macro-handle-type-object-vector type)
         `(handle-type-object-vector))

       (define (handle-type-object-vector)
         (let ((len (##vector-length obj)))
           (let loop ((i 0))
             (if (##fx< i len)
                 (macro-walk-object-seq
                  obj i (##vector-ref obj i) (##vector-set! obj i new-subobject)
                  (loop (##fx+ i 1)))
                 (macro-walk-continue)))))

       (define-macro (macro-handle-type-simple constructor . fields)
         `(macro-handle-fields 0 ,@fields))

       (define-macro (macro-handle-type-mixed-vector type . fields)
         `(macro-handle-fields 0 ,@fields))

       (define-macro (macro-handle-fields i . fields)
         (if (pair? fields)
             (let ((field (car fields)))
               `(macro-walk-object-seq
                 obj ,i (,(car field) obj) (,(cadr field) obj new-subobject)
                 (macro-handle-fields ,(+ i 1) ,@(cdr fields))))
             `(macro-walk-continue)))

       (define-macro (macro-handle-type-frame)
         `(handle-type-frame))

       (define (handle-type-frame)
         (macro-walk-object-seq
          obj 0 (##frame-ret obj) (##frame-ret-set! obj new-subobject)
          (let ((fs (##frame-fs obj)))
            (let loop ((i 1))
              (if (##fx< fs i)
                  (macro-walk-continue)
                  (if (##frame-slot-live? obj i)
                      (macro-walk-object-seq
                       obj i (##frame-ref obj i) (##frame-set! obj i new-subobject)
                       (loop (##fx+ i 1)))
                      (loop (##fx+ i 1))))))))

       (macro-walk-visit
        (macro-case-type obj)))

     (walk-object #t 0 obj)))

;;;
;;;; Walk
;;;

(define (##walk-interned-symbols proc)
  (let ((tbl (##symbol-table)))
    (let loop1 ((i (##fx- (##vector-length tbl) 1)))
      (if (##fx> i 0)
          (let loop2 ((obj (##vector-ref tbl i)))
            (if (##null? obj)
                (loop1 (##fx- i 1))
                (macro-walk-seq
                 (proc obj)
                 (loop2 (macro-symbol-next obj)))))
          (macro-walk-continue)))))

(define (##walk-interned-keywords proc)
  (let ((tbl (##keyword-table)))
    (let loop1 ((i (##fx- (##vector-length tbl) 1)))
      (if (##fx> i 0)
          (let loop2 ((obj (##vector-ref tbl i)))
            (if (##null? obj)
                (loop1 (##fx- i 1))
                (macro-walk-seq
                 (proc obj)
                 (loop2 (macro-keyword-next obj)))))
          (macro-walk-continue)))))

(define (##walk-from-object! obj visit substitute)

  (define-macro (macro-walk-visit recursive-scan)
    `(let ((result (visit container i obj)))
       (if (##eq? result (macro-walk-no-recursive-scan))
           (macro-walk-continue)
           (macro-walk-seq
            result
            ,recursive-scan))))

  (define-macro (macro-walk-object-seq container i subobject update-subobject! continue)
    `(let* ((container ,container)
            (i ,i)
            (subobject ,subobject))
       (macro-walk-seq
        (walk-object container i subobject)
        (let ((new-subobject (if substitute
                                 (substitute container i subobject)
                               subobject)))
          ,update-subobject!
          ,continue))))

  (macro-walk-object obj))

(define (##walk-from-roots! visit substitute)

  (define (scan-symbol-and-global-var obj)
    (macro-walk-seq
     (##walk-from-object! obj visit substitute)
     (if (##global-var? obj)
         (let* ((var (##make-global-var obj))
                (val (##global-var-ref var)))
           (macro-walk-seq
            (##walk-from-object! val visit substitute)
            (let ((new-val (if substitute
                               (substitute var 0 val)
                             val)))
              (##global-var-set! var new-val)
              (macro-walk-continue))))
         (macro-walk-continue))))

  (define (scan-keyword obj)
    (##walk-from-object! obj visit substitute))

  (##walk-interned-symbols scan-symbol-and-global-var)
  (##walk-interned-keywords scan-keyword))

(define (##walk-object! walk substitute #!key (root #f) (seen #f) (feedback? #f))

  (let ((seen (or seen (##new-register))))
  
  (define (visit container i obj)
    (if (##register-ref seen obj #f)
        (macro-walk-no-recursive-scan)
      (begin
        (if (##mem-allocated? obj)
            (begin
              (if feedback?
                  (let ((count (register-length seen)))
                    (if (= 0 (modulo count 10000))
                        (display "." (console-port)))))
              (##register-set! seen obj #t)))
        (if walk
            (walk container i obj)
          (macro-walk-continue)))))
  
  ;(define seen (##make-table 0 #f #f #f ##eq?))
  ;
  ;(define (visit container i obj)
  ;  (if (##table-ref seen obj #f)
  ;      (macro-walk-no-recursive-scan)
  ;    (begin
  ;      (if (##mem-allocated? obj)
  ;          (begin
  ;            (if (= 0 (modulo (table-length seen) 1000))
  ;                (pp (table-length seen)))
  ;            (##table-set! seen obj #t)))
  ;      (macro-walk-continue))))

  (if root
   (##walk-from-object! root visit substitute)
   (##walk-from-roots! visit substitute))))

(define walk-test
  #f)

(define (walk-test-set! obj)
  (set! walk-test obj))


;;;
;;;; Alloc
;;;

;;; procedures to create deep copies of objects

(define (##alloc-pair kind)
  ((c-function ##alloc-pair (int) scheme-object
             "___SCMOBJ r = ___EXT(___make_pair) (___arg1 == ___STILL ? ___ps : NULL, ___FIX(0), ___FIX(0));
              ___EXT(___release_scmobj)(r);
              ___return(r);")
   kind))

(define (##alloc-ovector len kind)
  ((c-function ##alloc-ovector (scheme-object int) scheme-object
             "___SCMOBJ r = ___EXT(___make_vector) (___arg2 == ___STILL ? ___ps : NULL, ___INT(___arg1), ___FIX(0));
              ___EXT(___release_scmobj)(r);
              ___return(r);")
   len
   kind))

(define (##alloc-bvector subtype len kind)
  ((c-function ##alloc-bvector (scheme-object scheme-object int) scheme-object
             "___SCMOBJ r = ___EXT(___alloc_scmobj) (___arg3 == ___STILL ? ___ps : NULL, ___INT(___arg1), ___INT(___arg2));
              ___EXT(___release_scmobj)(r);
              ___return(r);")
   subtype
   len
   kind))

;;;
;;;; Domain
;;;

(define-type domain
  constructor: make-initialized-domain
  copies
  bytes-copied
)

(define (##make-domain)
  (make-initialized-domain
   ;(##make-table 0 #f #f #f ##eq?)
   (##new-register)
   0))

;;;
;;;; Copy
;;;

(define (##copy-object obj kind domain #!key (copy? #f) (visit #f))
  
  (define (register-copy! obj copy)
    (##register-set! (domain-copies domain) obj copy))
  
  (define (existing-copy-of obj)
    ;;TODO: check if the existing copy is of the kind we want
    (##register-ref (domain-copies domain) obj #f))
  
  ;(define (register-copy! obj copy)
  ;  (##table-set! (domain-copies domain) obj copy))
  ;
  ;(define (existing-copy-of obj)
  ;  ;;TODO: check if the existing copy is of the kind we want
  ;  (##table-ref (domain-copies domain) obj #f))
  
  (define (count-bytes-copied! obj)
    (domain-bytes-copied-set!
     domain
     (##fx+ (##u8vector-length obj)
            (##u8vector-length '#(1))
            (domain-bytes-copied domain)))
    obj)

  (define (new-pair)
    (count-bytes-copied! (##alloc-pair kind)))

  (define (new-ovector len)
    (count-bytes-copied! (##alloc-ovector len kind)))

  (define (new-bvector st len)
    (count-bytes-copied! (##alloc-bvector st len kind)))

  (define (copy-pair obj)
    (if visit
        (visit obj))
    (let ((copy (new-pair)))
      (register-copy! obj copy)
      (##set-car! copy (copy-object (##car obj)))
      (##set-cdr! copy (copy-object (##cdr obj)))
      copy))

  (define (copy-ovector obj st)
    (if visit
        (visit obj))
    (let* ((len (##vector-length obj))
           (copy (new-ovector len)))
      (##subtype-set! copy st)
      (register-copy! obj copy)
      (let loop ((i 0))
        (if (##fx< i len)
            (begin
              (##vector-set! copy i (copy-object (##vector-ref obj i)))
              (loop (##fx+ i 1)))
            copy))))

  (define (copy-bvector obj st)
    (if visit
        (visit obj))
    (let* ((len (##u8vector-length obj))
           (copy (new-bvector st len)))
      (register-copy! obj copy)
      (let loop ((i 0))
        (if (##fx< i len)
            (begin
              (##u8vector-set! copy i (##u8vector-ref obj i))
              (loop (##fx+ i 1)))
            copy))))

  (define (copy-object obj)
    (if (or (##not (##mem-allocated? obj))
            (and copy? (not (copy? obj))))

        obj

        (or (existing-copy-of obj)
            (let ((st (##subtype obj)))

              ;;(println "copying: " (wr obj)) ;; debugging trace

              (cond ((##fx= st (macro-subtype-pair))
                     (copy-pair obj))
                    
                    ((or (##fx= st (macro-subtype-vector))
                         (##fx= st (macro-subtype-ratnum))
                         (##fx= st (macro-subtype-cpxnum))
                         (##fx= st (macro-subtype-structure))
                         (##fx= st (macro-subtype-boxvalues))
                         (##fx= st (macro-subtype-promise))
                         (##fx= st (macro-subtype-meroon))
                         (%%jazz-subtype? st)
                         (%%jazzstruct-subtype? st))
                     (copy-ovector obj st))

                    ((##fx= st (macro-subtype-symbol))
                     #;
                     (if (not (eq? (jazz:memory-allocated-kind obj) 'PERM))
                         (error "symbol is not PERM:" obj))
                     obj)

                    ((##fx= st (macro-subtype-keyword))
                     #;
                     (if (not (eq? (jazz:memory-allocated-kind obj) 'PERM))
                         (error "keyword is not PERM:" obj))
                     obj)

                    ((##fx= st (macro-subtype-frame))
                     (error "cannot copy frame:" obj))

                    ((##fx= st (macro-subtype-continuation))
                     #;
                     (error "cannot copy continuation:" obj)
                     obj)

                    ((##fx= st (macro-subtype-weak))
                     (if (##will? obj)
                         (error "cannot copy will:" obj)
                         (copy-ovector obj st)))

                    ((##fx= st (macro-subtype-procedure))
                     (if (##closure? obj)
                         (copy-ovector obj st)
                         obj))

                    ((##fx= st (macro-subtype-return))
                     (error "cannot copy return:" obj))

                    ((##fx= st (macro-subtype-foreign))
                     #;
                     (error "cannot copy foreign:" obj)
                     obj)

                    ((or (##fx= st (macro-subtype-string))
                         (##fx= st (macro-subtype-s8vector))
                         (##fx= st (macro-subtype-u8vector))
                         (##fx= st (macro-subtype-s16vector))
                         (##fx= st (macro-subtype-u16vector))
                         (##fx= st (macro-subtype-s32vector))
                         (##fx= st (macro-subtype-u32vector))
                         (##fx= st (macro-subtype-f32vector))
                         (##fx= st (macro-subtype-s64vector))
                         (##fx= st (macro-subtype-u64vector))
                         (##fx= st (macro-subtype-f64vector))
                         (##fx= st (macro-subtype-flonum))
                         (##fx= st (macro-subtype-bignum)))
                     (copy-bvector obj st))

                    (else
                     (error "cannot copy object:" obj)))))))

  (copy-object obj))

;;;
;;;; Debug
;;;

(define (wr obj)
  (parameterize ((current-readtable
                  (readtable-sharing-allowed?-set
                   (current-readtable)
                   #t)))
    (object->string obj 79)))

;;;
;;;; Interface
;;;

(define new-register ##new-register)
(define register-ref ##register-ref)
(define register-set! ##register-set!)
(define register-length ##register-length)
(define iterate-register ##iterate-register)
(define register->table ##register->table)

(define make-domain ##make-domain)
(define copy-to ##copy-object)
(define walk-object! ##walk-object!)
(define walk-continue (macro-walk-continue))
(define walk-prune (macro-walk-no-recursive-scan))

(define (table-gcht table) (macro-table-gcht table))

(define walk-interned-symbols ##walk-interned-symbols)
(define walk-interned-keywords ##walk-interned-keywords)

(define alloc-pair ##alloc-pair)
(define alloc-ovector ##alloc-ovector)
(define alloc-bvector ##alloc-bvector)

(define (symbol-name s)
  (macro-symbol-name s))

;;;
;;;; Test
;;;

(define make-adder ;; a classic closure creator
  (lambda (x) (lambda (y) (+ x y))))

(define foobar
  (list (make-adder 10)
        (make-adder 20)
        (list (vector 30 31) (string #\a #\b))
        (list (vector 40 41) (string #\c #\d))))

(define (test-walk)
  (let ((domain (##make-domain)))

    ;; choose some parts of foobar to copy
    (##copy-object (list-ref foobar 2) STILL domain)
    (##copy-object (list-ref foobar 1) PERM domain)

    ;; foobar does not yet contain copies
    (pp (map jazz:memory-allocated-kind foobar))
    (pp (map jazz:memory-allocated-kind (list-ref foobar 2)))

    ;; update foobar to point to the permanent copies
    (##walk-object!
      (lambda (container i obj)
        walk-continue)
      (lambda (container i obj)
        (or (register-ref (domain-copies domain) obj #f)
            obj)))

    ;; foobar now contains copies
    (pp (map jazz:memory-allocated-kind foobar))
    (pp (map jazz:memory-allocated-kind (list-ref foobar 2)))

    ;; test that the copies are correct
    (pp ((list-ref foobar 1) 100))
    (pp foobar)

    (pp (list 'bytes-copied= (domain-bytes-copied domain)))
))

;(test-walk)
)
