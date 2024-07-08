;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Macro Expansion
;;;
;;;  The contents of this file are subject to the Mozilla Public License Version
;;;  1.1 (the "License"); you may not use this file except in compliance with
;;;  the License. You may obtain a copy of the License at
;;;  http://www.mozilla.org/MPL/
;;;
;;;  Software distributed under the License is distributed on an "AS IS" basis,
;;;  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
;;;  for the specific language governing rights and limitations under the
;;;  License.
;;;
;;;  The Original Code is JazzScheme.
;;;
;;;  The Initial Developer of the Original Code is Guillaume Cartier.
;;;  Portions created by the Initial Developer are Copyright (C) 1996-2018
;;;  the Initial Developer. All Rights Reserved.
;;;
;;;  Contributor(s):
;;;
;;;  Alternatively, the contents of this file may be used under the terms of
;;;  the GNU General Public License Version 2 or later (the "GPL"), in which
;;;  case the provisions of the GPL are applicable instead of those above. If
;;;  you wish to allow use of your version of this file only under the terms of
;;;  the GPL, and not to allow others to use your version of this file under the
;;;  terms of the MPL, indicate your decision by deleting the provisions above
;;;  and replace them with the notice and other provisions required by the GPL.
;;;  If you do not delete the provisions above, a recipient may use your version
;;;  of this file under the terms of any one of the MPL or the GPL.
;;;
;;;  See www.jazzscheme.org for details.


(block kernel.expansion


(declare (not safe))


(define (jazz:generate-symbol #!optional (prefix "sym"))
  (let ((for (jazz:generate-symbol-for)))
    (let ((name (string-append prefix (or for "^"))))
      (##gensym (##string->uninterned-symbol name)))))


(define (jazz:generate-global-symbol #!optional (prefix "sym"))
  (let ((for (jazz:generate-symbol-for))
        (context (jazz:generate-symbol-context)))
    (cond ((%%not for)
           (error "Invalid call to generate-global-symbol without a for"))
          ((%%not context)
           (error "Invalid call to generate-global-symbol without a context"))
          (else
           (let ((module (jazz:string-replace (symbol->string context) #\. #\/)))
             (let ((name (%%string-append module "_" prefix for)))
               (##gensym (##string->uninterned-symbol name))))))))


(define (jazz:with-uniqueness expr proc)
  (if (symbol? (jazz:source-code expr))
      (proc expr)
    (let ((value (jazz:generate-symbol "val")))
      `(let ((,value ,expr))
         ,(proc value)))))


(define (jazz:with-uniqueness-typed expr specifier proc)
  (if (symbol? (jazz:source-code expr))
      (proc expr)
    (let ((value (jazz:generate-symbol "val")))
      `(let ((,value ,specifier ,expr))
         ,(proc value)))))


(jazz:define-macro (%%force-uniqueness variables code)
  (if (null? variables)
      code
    (let ((variable (car variables)))
      `(jazz:with-uniqueness ,variable
         (lambda (,variable)
           (%%force-uniqueness ,(cdr variables) ,code))))))


(define (jazz:null/pair? obj)
  (or (%%null? obj) (%%pair? obj)))


;;;
;;;; Check
;;;


(jazz:define-macro (jazz:define-check-macro name test type)
  (let ((str (symbol->string name)))
    (let ((core (string->symbol (string-append "%%" str)))
          (debug (string->symbol (string-append "jazz:" str))))
      `(begin
         (jazz:define-macro (,core arg pos call code)
           (if jazz:debug-core?
               `(if (,',test ,arg)
                    ,code
                  (jazz:primitive-type-error ,pos ,',type ',(car call) (list ,@(cdr call))))
             code))
         (jazz:define-macro (,debug arg pos call code)
           (if jazz:debug-user?
               `(if (,',test ,arg)
                    ,code
                  (jazz:primitive-type-error ,pos ,',type ',(car call) (list ,@(cdr call))))
             code))))))


(jazz:define-check-macro check-char
  ^#char?
  "CHAR")

(jazz:define-check-macro check-closure
  ^#closure?
  "CLOSURE")

(jazz:define-check-macro check-continuation
  ^#continuation?
  "CONTINUATION")

(jazz:define-check-macro check-fixnum
  ^#fixnum?
  "FIXNUM")

(jazz:define-check-macro check-flonum
  ^#flonum?
  "FLONUM")

(jazz:define-check-macro check-foreign
  ^#foreign?
  "FOREIGN")

(jazz:define-check-macro check-f64vector
  ^#f64vector?
  "F64VECTOR")

;; not 100% correct because of Scheme's semantic for list? that is costly
(jazz:define-check-macro check-list
  jazz:null/pair?
  "LIST")

(jazz:define-check-macro check-locat
  ^#locat?
  "LOCAT")

(jazz:define-check-macro check-port
  ^#port?
  "PORT")

(jazz:define-check-macro check-procedure
  ^#procedure?
  "PROCEDURE")

(jazz:define-check-macro check-ratnum
  ^#ratnum?
  "RATNUM")

(jazz:define-check-macro check-readenv
  jazz:readenv?
  "READENV")

(jazz:define-check-macro check-readtable
  ^#readtable?
  "READTABLE")

(jazz:define-check-macro check-source
  ^#source?
  "SOURCE")

(jazz:define-check-macro check-string
  ^#string?
  "STRING")

(jazz:define-check-macro check-structure
  ^#structure?
  "STRUCTURE")

(jazz:define-check-macro check-symbol
  ^#symbol?
  "SYMBOL")

(jazz:define-check-macro check-type
  ^#type?
  "TYPE")

(jazz:define-check-macro check-table
  table?
  "TABLE")

(jazz:define-check-macro check-thread
  thread?
  "THREAD")

(jazz:define-check-macro check-values
  ^#values?
  "VALUES")

(jazz:define-check-macro check-vector
  ^#vector?
  "VECTOR")

(jazz:define-check-macro check-writeenv
  jazz:writeenv?
  "WRITEENV")


;;;
;;;; Code
;;;


(jazz:define-macro (c-code code . arguments)
  `(let ()
     (declare (extended-bindings))
     (##c-code ,code ,@arguments)))


;;;
;;;; Memory
;;;


(define MOVABLE0 0)
(define MOVABLE1 1)
(define MOVABLE2 2)
(define STILL    5)
(define PERM     6)


(define jazz:memory-allocated?
  ##mem-allocated?)


(define (jazz:memory-permanent? obj)
  (and (##mem-allocated? obj)
       (let ((k (bitwise-and
                  (##u64vector-ref (##type-cast obj 1) -1)
                  7)))
         (##fx= k PERM))))


(define (jazz:memory-allocated-kind obj)
  (let ((k (bitwise-and
             (##u64vector-ref (##type-cast obj 1) -1)
             7)))
    (cond ((##fx= k MOVABLE0) 'MOVABLE)
          ((##fx= k MOVABLE1) 'MOVABLE)
          ((##fx= k MOVABLE2) 'MOVABLE)
          ((##fx= k STILL)    'STILL)
          ((##fx= k PERM)     'PERM))))


(define (jazz:memory-allocated-size obj)
  (let ((size (##fx+ (##u8vector-length '#(1))
                     (##u8vector-length obj))))
    (if (##eq? (jazz:memory-allocated-kind obj) 'MOVABLE)
        (##fx* size 2) ;; two-space factor
      size)))


(define (jazz:memory-size obj)
  (cond ((##pair? obj)
         48)
        ((##symbol? obj)
         40)
        ((##mem-allocated? obj)
         (jazz:memory-allocated-size obj))
        (else
         ;; (pp obj) should not happen
         0)))


(define jazz:gc-hash-table?
  ##gc-hash-table?)


;;;
;;;; Tracking
;;;


(define jazz:tracking-enabled?
  (if jazz:kernel-track-memory?
      (jazz:global-ref '##tracking-enabled?)
    (lambda ()
      #f)))

(define jazz:tracking-allocations?
  (if jazz:kernel-track-memory?
      (jazz:global-ref '##tracking-allocations?)
    (lambda ()
      #f)))

(define jazz:track-allocations
  (if jazz:kernel-track-memory?
      (jazz:global-ref '##track-allocations)
    (lambda ()
      #f)))

(define jazz:untrack-allocations
  (if jazz:kernel-track-memory?
      (jazz:global-ref '##untrack-allocations)
    (lambda ()
      #f)))

(define jazz:record-tracked-hook-set!
  (if jazz:kernel-track-memory?
      (jazz:global-ref '##record-tracked-hook-set!)
    (lambda (hook)
      #f)))


;;;
;;;; Moving
;;;


(define (jazz:make-moving)
  (let ((moving (##make-f64vector 4)))
    (jazz:reset-moving moving)
    moving))

(define-macro (jazz:moving-start moving)
  `(##f64vector-ref ,moving 0))

(define-macro (jazz:moving-total moving)
  `(##f64vector-ref ,moving 1))

(define-macro (jazz:moving-rate moving)
  `(##f64vector-ref ,moving 2))

(define-macro (jazz:moving-average moving)
  `(##f64vector-ref ,moving 3))

(define-macro (jazz:moving-start-set! moving val)
  `(##f64vector-set! ,moving 0 ,val))

(define-macro (jazz:moving-total-set! moving val)
  `(##f64vector-set! ,moving 1 ,val))

(define-macro (jazz:moving-rate-set! moving val)
  `(##f64vector-set! ,moving 2 ,val))

(define-macro (jazz:moving-average-set! moving val)
  `(##f64vector-set! ,moving 3 ,val))


;; MSECTION / MSECTION - FUDGE * 2
(define jazz:fudge-price
  1.142897)


(define jazz:monotonic-time
  (##make-f64vector 1))

(define-macro (jazz:monotonic-get!)
  `(##get-current-monotonic-time! jazz:monotonic-time 0))

(define-macro (jazz:monotonic-ref)
  `(##f64vector-ref jazz:monotonic-time 0))


(define jazz:moving-elapsed
  (##make-f64vector 1))

(define-macro (jazz:elapsed-ref)
  `(##f64vector-ref jazz:moving-elapsed 0))

(define-macro (jazz:elapsed-set! val)
  `(##f64vector-set! jazz:moving-elapsed 0 ,val))


;; <<<<<<<<<<<<<<<<<<
;; the code from here


(define (jazz:update-moving moving size)
  (declare (not interrupts-enabled))
  (jazz:monotonic-get!)
  (if (##fl= (jazz:moving-start moving) -1.)
      (begin
        (jazz:moving-start-set! moving (jazz:monotonic-ref))
        (jazz:moving-total-set! moving 0.)))
  (jazz:moving-total-set! moving (##fl+ (jazz:moving-total moving) (##fl* (##fixnum->flonum size) jazz:fudge-price)))
  (jazz:elapsed-set! (##fl- (jazz:monotonic-ref) (jazz:moving-start moving)))
  (if (##fl>= (jazz:elapsed-ref) .1)
      (begin
        (jazz:moving-rate-set! moving (##fl/ (jazz:moving-total moving) (jazz:elapsed-ref)))
        (if (##fl= (jazz:moving-average moving) -1.)
            (jazz:moving-average-set! moving (jazz:moving-rate moving))
          (jazz:moving-average-set! moving (##fl/ (##fl+ (##fl* 15. (jazz:moving-average moving)) (jazz:moving-rate moving)) 16.)))
        (jazz:moving-start-set! moving (jazz:monotonic-ref))
        (jazz:moving-total-set! moving 0.))))


(define (jazz:reset-moving moving)
  (jazz:moving-start-set! moving -1.)
  (jazz:moving-total-set! moving -1.)
  (jazz:moving-rate-set! moving 0.)
  (jazz:moving-average-set! moving -1.))


;;;
;;;; Monitor
;;;


(define jazz:monitor-allocations?
  #f)


(define jazz:monitor-allocations-count
  0)

(define jazz:monitor-allocations-size
  0)


(define jazz:save-moving
  (jazz:make-moving))

(define jazz:capture-moving
  (jazz:make-moving))

(define jazz:allocation-moving
  (jazz:make-moving))


(define (jazz:monitoring-allocations?)
  jazz:monitor-allocations?)


(define (jazz:monitor-allocations-reset)
  (set! jazz:monitor-allocations-count 0)
  (set! jazz:monitor-allocations-size 0)
  (jazz:reset-moving jazz:save-moving)
  (jazz:reset-moving jazz:capture-moving)
  (jazz:reset-moving jazz:allocation-moving))


(define (jazz:save-rate-update size)
  (jazz:update-moving jazz:save-moving size))


(define (jazz:capture-rate-update size)
  (jazz:update-moving jazz:capture-moving size))


(define (jazz:allocation-rate-update size)
  (jazz:update-moving jazz:allocation-moving size))


(define (jazz:monitor-allocations-start)
  (set! jazz:monitor-allocations? #t)
  (jazz:monitor-allocations-reset)
  (jazz:track-allocations))

(define (jazz:monitor-allocations-stop)
  (set! jazz:monitor-allocations? #f)
  (jazz:untrack-allocations))


(define (jazz:monitor-tracked)
  (declare (not interrupts-enabled))
  (let ((save (##save-tracked)))
    (if (##fx> save 0)
        (jazz:save-rate-update (##fx* save 8 2))))
  (let ((capture (##capture-tracked)))
    (if (##fx> capture 0)
        (jazz:capture-rate-update (##fx* capture 8 2))))
  (let ((count (##count-tracked)))
    (let loop ((n 0) (size 0))
         (if (##fx>= n count)
             (jazz:allocation-rate-update size)
           (let ((obj (##get-tracked-object n)))
             (let ((sz (jazz:memory-size obj)))
               (set! jazz:monitor-allocations-count (##fx+ jazz:monitor-allocations-count 1))
               (set! jazz:monitor-allocations-size (##fx+ jazz:monitor-allocations-size sz))
               (loop (##fx+ n 1) (##fx+ size sz))))))))


;; to here should not allocate memory so as to not impact
;; the gc period. note that opening the generated c file and
;; scanning for BOX is a good way to check for allocations
;; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


(define (jazz:monitor-save-rate)
  (if (##fl= (jazz:moving-average jazz:save-moving) -1.)
      #f
    (jazz:moving-average jazz:save-moving)))

(define (jazz:monitor-capture-rate)
  (if (##fl= (jazz:moving-average jazz:capture-moving) -1.)
      #f
    (jazz:moving-average jazz:capture-moving)))

(define (jazz:monitor-allocation-rate)
  (if (##fl= (jazz:moving-average jazz:allocation-moving) -1.)
      #f
    (jazz:moving-average jazz:allocation-moving)))


;;;
;;;; Register
;;;


(define jazz:register-allocations?
  #f)


(define jazz:allocations
  (##make-table 0 #f #f #f ##eq?))

(define jazz:allocations-rank
  0)


(define (jazz:register-allocations)
  (set! jazz:register-allocations? #t)
  (jazz:track-allocations))

(define (jazz:unregister-allocations)
  (set! jazz:register-allocations? #f)
  (jazz:untrack-allocations))


(define (jazz:reset-allocations)
  (set! jazz:allocations (##make-table 0 #f #f #f ##eq?))
  (set! jazz:allocations-rank 0))


(define (jazz:registered-allocations)
  jazz:allocations)


(define (jazz:ordered-allocations)
  (jazz:sort-list < (##table->list (jazz:registered-allocations)) key: (lambda (pair) (jazz:allocation-rank (##cdr pair)))))


(define (jazz:register-allocation obj allocation)
  (##table-set! jazz:allocations obj allocation)
  (set! jazz:allocations-rank (##fx+ jazz:allocations-rank 1)))


(define (jazz:register-tracked)
  (declare (not interrupts-enabled))
  (##continuation-capture
    (lambda (cont)
      (let ((thread (thread-name (##current-thread)))
            (stack (jazz:track-continuation cont jazz:*track-depth*))
            (count (##count-tracked)))
        (let loop ((n 0))
             (if (##fx< n count)
                 (let ((obj (##get-tracked-object n))
                       (file (##get-tracked-file n))
                       (line (##get-tracked-line n)))
                   (let ((size (jazz:memory-size obj)))
                     (jazz:register-allocation obj (##vector jazz:allocations-rank size thread (jazz:fix-tracked-file file) line stack))
                     (loop (##fx+ n 1))))))))))


;;;
;;;; Persist
;;;


(define jazz:persist-allocations?
  #f)


(define jazz:persisted-allocations
  (##make-table 0 #f #t #f ##eq?))


(define (jazz:persist-allocations)
  (set! jazz:persist-allocations? #t)
  (jazz:track-allocations))

(define (jazz:unpersist-allocations)
  (set! jazz:persist-allocations? #f)
  (jazz:untrack-allocations))


(define (jazz:persist-allocation obj allocation)
  (##table-set! jazz:persisted-allocations obj allocation))


(define (jazz:persisted-allocations-table)
  jazz:persisted-allocations)


(define (jazz:persist-tracked)
  (declare (not interrupts-enabled))
  (let ((thread (thread-name (##current-thread)))
        (count (##count-tracked)))
    (let loop ((n 0))
         (if (##fx< n count)
             (let ((obj (##get-tracked-object n)))
               (if (##not (jazz:memory-permanent? obj))
                   (let ((file (##get-tracked-file n))
                         (line (##get-tracked-line n)))
                     (let ((size (jazz:memory-size obj)))
                       (let ((stack (and (>= size 500)
                                         (##continuation-capture
                                           (lambda (cont)
                                             (jazz:track-continuation cont jazz:*track-depth*))))))
                         (jazz:persist-allocation obj (##vector jazz:allocations-rank size thread (jazz:fix-tracked-file file) line stack))
                         (loop (##fx+ n 1)))))))))))


;;;
;;;; Tracked
;;;


(define (jazz:record-tracked)
  (if jazz:monitor-allocations?
      (jazz:monitor-tracked))
  (if jazz:register-allocations?
      (jazz:register-tracked))
  (if jazz:persist-allocations?
      (jazz:persist-tracked)))

(jazz:record-tracked-hook-set! jazz:record-tracked)


(define (jazz:track-allocations)
  (##track-allocations))


(define (jazz:untrack-allocations)
  (if (and (##not jazz:monitor-allocations?)
           (##not jazz:register-allocations?)
           (##not jazz:persist-allocations?))
      (##untrack-allocations)))


(define (jazz:fix-tracked-file file)
  ;; hack around a gambit bug fixed in latest
  (let ((len (##string-length file)))
    (if (and (##fx> len 0)
             (##eq? (##string-ref file (##fx- len 1)) #\"))
        (##substring file 0 (##fx- len 1))
      file)))


(define (jazz:allocation-rank allocation)
  (##vector-ref allocation 0))

(define (jazz:allocation-size allocation)
  (##vector-ref allocation 1))

(define (jazz:allocation-thread allocation)
  (##vector-ref allocation 2))

(define (jazz:allocation-file allocation)
  (##vector-ref allocation 3))

(define (jazz:allocation-line allocation)
  (##vector-ref allocation 4))

(define (jazz:allocation-stack allocation)
  (##vector-ref allocation 5))


(jazz:define-macro (%%tracking expr)
  (if jazz:kernel-track-memory?
      (case (jazz:walk-for)
        ((compile)
         `(c-code #<<end-of-code
___RESULT = ___UPDATE_TRACKED(___ARG1);
end-of-code

            ,expr))
        (else
         expr))
    expr))


(define jazz:*track-depth*
  8)


(define (jazz:track-depth)
  jazz:*track-depth*)

(define (jazz:track-depth-set! depth)
  (set! jazz:*track-depth* depth))


(define (jazz:track-continuation cont depth #!key (filter #f))
  (define (continuation-creator cont)
    (let ((proc (^#continuation-creator cont)))
      (if (and proc (^#closure? proc))
          (^#closure-code proc)
        proc)))
  
  (define (continuation-next-interesting cont)
    (let loop ((current-cont cont))
         (if current-cont
             (if (and (or (^#not filter) (filter current-cont))
                      (let ((creator (continuation-creator current-cont)))
                        (and (^#not (^#eq? creator ##record-tracked))
                             (^#not (^#eq? creator jazz:record-tracked))
                             (^#not (^#eq? creator jazz:persist-tracked)))))
                 current-cont
               (loop (^#continuation-next current-cont)))
           #f)))
  
  (define (continuation-next-distinct cont creator)
    (let loop ((current-cont (^#continuation-next cont)))
         (if current-cont
             (if (^#eq? creator (continuation-creator current-cont))
                 (loop (^#continuation-next current-cont))
               current-cont)
           #f)))
  
  (define (identify-location locat)
    (let ((container (and locat (^#locat-container locat))))
      (if container
          (let ((filepos (^#position->filepos (^#locat-position locat))))
            (let ((line (^#filepos-line filepos))
                  (col (^#filepos-col filepos)))
              (^#list container line col)))
        #f)))
  
  (define (identify cont d)
    (let ((cont (and cont (^#fx< d depth) (continuation-next-interesting cont))))
      (if (^#not cont)
          '()
        (let ((creator (continuation-creator cont))
              (location (identify-location (^#continuation-locat cont))))
          (^#cons (^#list creator location)
                  (identify (continuation-next-distinct cont creator)
                            (^#fx+ d 1)))))))
  
  (identify cont 0)))
