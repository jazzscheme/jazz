;;; File: walk.scm

;;; Compile and run with: gsc walk;gsi walk

(unit gambit.walk

(include "~~lib/_gambit#.scm")

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

;;; Type dispatcher

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
                   (macro-foreign-tags macro-foreign-tags-set!)))

                 ((macro-subtype-bvector? subtype)
                  (macro-handle-type-mixed-vector
                   bvector))

                 ((##fx= subtype (macro-subtype-symbol))
                  (macro-handle-type-mixed-vector
                   symbol
                   (macro-symbol-name macro-symbol-name-set!)
                   (macro-symbol-hash macro-symbol-hash-set!)
                   (macro-symbol-next macro-symbol-next-set!)))

                 ((##fx= subtype (macro-subtype-keyword))
                  (macro-handle-type-mixed-vector
                   keyword
                   (macro-keyword-name macro-keyword-name-set!)
                   (macro-keyword-hash macro-keyword-hash-set!)
                   (macro-keyword-next macro-keyword-next-set!)))

                 ((##fx= subtype (macro-subtype-frame))
                  (macro-handle-type-frame))

                 ((##fx= subtype (macro-subtype-continuation))

                  (##continuation-frame obj) ;; force frame into heap

                  (macro-handle-type-simple
                   macro-make-continuation
                   (macro-continuation-frame macro-continuation-frame-set!)
                   (macro-continuation-denv macro-continuation-denv-set!)))

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

(define (##update-reachable-from-object! obj visit substitute)

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
        (let ((new-subobject (substitute container i subobject)))
          ,update-subobject!
          ,continue))))

  (macro-walk-object obj))

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

(define (##update-reachable-from-roots! visit substitute)

  (define (scan-symbol-and-global-var obj)
    (macro-walk-seq
     (##update-reachable-from-object! obj visit substitute)
     (if (##global-var? obj)
         (let* ((var (##make-global-var obj))
                (val (##global-var-ref var)))
           (macro-walk-seq
            (##update-reachable-from-object! val visit substitute)
            (let ((new-val (substitute val 0 val)))
              (##global-var-set! var new-val)
              (macro-walk-continue))))
         (macro-walk-continue))))

  (define (scan-keyword obj)
    (##update-reachable-from-object! obj visit substitute))

  (##walk-interned-symbols scan-symbol-and-global-var)
  (##walk-interned-keywords scan-keyword))

(define (##update-reachable! special-roots substitute)

  (define seen (##make-table 0 #f #f #f ##eq?))

  (define (visit container i obj)
    (if (##table-ref seen obj #f)
        (macro-walk-no-recursive-scan)
        (begin
          (if (##mem-allocated? obj)
              (##table-set! seen obj #t))
          (macro-walk-continue))))

  (macro-walk-seq
   (##update-reachable-from-object! special-roots visit substitute)
   (##update-reachable-from-roots! visit substitute)))

;;; procedures to create deep copies of objects

(define (##alloc-pair kind)
  ((c-lambda (scheme-object) scheme-object
             "___result = ___EXT(___make_pair) (___FIX(0), ___FIX(0), ___INT(___arg1));")
   kind))

(define (##alloc-ovector len kind)
  ((c-lambda (scheme-object scheme-object) scheme-object
             "___result = ___EXT(___make_vector) (___INT(___arg1), ___FIX(0), ___INT(___arg2));")
   len
   kind))

(define (##alloc-bvector subtype len kind)
  ((c-lambda (scheme-object scheme-object scheme-object) scheme-object
             "___result = ___EXT(___alloc_scmobj) (___INT(___arg1), ___INT(___arg2), ___INT(___arg3));")
   subtype
   len
   kind))

(define-type domain
  constructor: make-initialized-domain
  copies
  bytes-copied
)

(define (##make-domain)
  (make-initialized-domain
   (##make-table 0 #f #f #f ##eq?)
   0))

(define (##copy-object obj kind domain)

  (define (register-copy! obj copy)
    (##table-set! (domain-copies domain) obj copy))

  (define (existing-copy-of obj)
    ;;TODO: check if the existing copy is of the kind we want
    (##table-ref (domain-copies domain) obj #f))

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
    (let ((copy (new-pair)))
      (register-copy! obj copy)
      (##set-car! copy (copy-object (##car obj)))
      (##set-cdr! copy (copy-object (##cdr obj)))
      copy))

  (define (copy-ovector obj st)
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
    (if (##not (##mem-allocated? obj))

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
                         (##fx= st (macro-subtype-meroon))
                         (##fx= st (macro-subtype-jazz))
                         (##fx= st (macro-subtype-promise)))
                     (copy-ovector obj st))

                    ((##fx= st (macro-subtype-symbol))
                     #;
                     (if (not (eq? (mem-allocated-kind obj) 'PERM))
                         (error "symbol is not PERM:" obj))
                     obj)

                    ((##fx= st (macro-subtype-keyword))
                     #;
                     (if (not (eq? (mem-allocated-kind obj) 'PERM))
                         (error "keyword is not PERM:" obj))
                     obj)

                    ((##fx= st (macro-subtype-frame))
                     (error "cannot copy frame:" obj))

                    ((##fx= st (macro-subtype-continuation))
                     (error "cannot copy continuation:" obj))

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
                     (error "cannot copy foreign:" obj))

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

(define (wr obj)
  (parameterize ((current-readtable
                  (readtable-sharing-allowed?-set
                   (current-readtable)
                   #t)))
    (object->string obj 79)))

;;; Public interface

(define make-domain ##make-domain)
(define copy-to ##copy-object)
(define update-reachable! ##update-reachable!)

;;; Test it:

(define MOVABLE0 0)
(define MOVABLE1 1)
(define MOVABLE2 2)
(define STILL    5)
(define PERM     6)

(define (mem-allocated-kind obj)
  (let ((k (bitwise-and
            (if (##fx= 4 (##u8vector-length '#(0)))
                (##u32vector-ref (##type-cast obj 1) -1)
                (##u64vector-ref (##type-cast obj 1) -1))
            7)))
    (cond ((##fx= k MOVABLE0) 'MOVABLE0)
          ((##fx= k MOVABLE1) 'MOVABLE1)
          ((##fx= k MOVABLE2) 'MOVABLE2)
          ((##fx= k STILL)    'STILL)
          ((##fx= k PERM)     'PERM)
          (else '???))))

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
    (pp (map mem-allocated-kind foobar))
    (pp (map mem-allocated-kind (list-ref foobar 2)))

    ;; update foobar to point to the permanent copies
    (##update-reachable!
     '()
     (lambda (container i obj)
       (or (table-ref (domain-copies domain) obj #f)
           obj)))

    ;; foobar now contains copies
    (pp (map mem-allocated-kind foobar))
    (pp (map mem-allocated-kind (list-ref foobar 2)))

    ;; test that the copies are correct
    (pp ((list-ref foobar 1) 100))
    (pp foobar)

    (pp (list 'bytes-copied= (domain-bytes-copied domain)))
))

;(test-walk)
)
