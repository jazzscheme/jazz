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


;; bongo
(define ##gensym-interned-counter
  -1)

(define (##gensym-interned #!optional (p #f))
  (let ((prefix
         (if (##eq? p #f)
             'g
             p)))
    (let ((new-count
            (##fxmodulo
              (##fx+ ##gensym-interned-counter 1)
              1000000)))
      ;; Note: it is unimportant if the increment of ##gensym-interned-counter
      ;; is not atomic; it simply means a possible close repetition
      ;; of the same name
      (set! ##gensym-interned-counter new-count)
      (##string->symbol
        (if (##eq? prefix 'g)
            new-count ;; ##symbol->string will create the string
          (##string-append (##symbol->string prefix)
            (##number->string new-count 10)))))))



(define (jazz:generate-symbol #!optional (prefix "sym"))
  (let ((for (jazz:generate-symbol-for)))
    (let ((name (string-append prefix (or for "^"))))
      ;; bongo was ##gensym and ##string->uninterned-symbol
      (##gensym-interned (##string->symbol name)))))


(define (jazz:generate-global-symbol #!optional (prefix "sym"))
  (let ((for (jazz:generate-symbol-for))
        (context (jazz:generate-symbol-context)))
    (cond ((%%not for)
           (error "Invalid call to generate-global-symbol without a for"))
          ((%%not context)
           (error "Invalid call to generate-global-symbol without a context"))
          (else
           (let ((module (jazz:string-replace (symbol->string context) #\. #\/)))
             (let ((name (##string-append module "_" prefix for)))
               ;; bongo was ##gensym and ##string->uninterned-symbol
               (##gensym-interned (##string->symbol name))))))))


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
;;;; Tracking
;;;


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

(define jazz:update-stack
  (if jazz:kernel-track-memory?
      (jazz:global-ref '##update-stack)
    (lambda (obj stack)
      #f)))

(define jazz:reset-allocations
  (if jazz:kernel-track-memory?
      (jazz:global-ref '##reset-allocations)
    (lambda ()
      #f)))

(define jazz:count-allocations
  (if jazz:kernel-track-memory?
      (jazz:global-ref '##count-allocations)
    (lambda ()
      0)))

(define jazz:all-allocations
  (if jazz:kernel-track-memory?
      (jazz:global-ref '##all-allocations)
    (lambda ()
      0)))

(define jazz:snapshot-allocations
  (if jazz:kernel-track-memory?
      (jazz:global-ref '##snapshot-allocations)
    (lambda ()
      #f)))

(define jazz:get-allocation-object
  (if jazz:kernel-track-memory?
      (jazz:global-ref '##get-allocation-object)
    (lambda (n)
      #f)))

(define jazz:get-allocation-file
  (if jazz:kernel-track-memory?
      (jazz:global-ref '##get-allocation-file)
    (lambda (n)
      #f)))

(define jazz:get-allocation-line
  (if jazz:kernel-track-memory?
      (jazz:global-ref '##get-allocation-line)
    (lambda (n)
      #f)))

(define jazz:get-allocation-stack
  (if jazz:kernel-track-memory?
      (jazz:global-ref '##get-allocation-stack)
    (lambda (n)
      #f)))


(define (jazz:get-allocation n)
  (^#list (jazz:get-allocation-object n)
          (jazz:get-allocation-file n)
          (jazz:get-allocation-line n)))


(jazz:define-macro (%%tracking expr)
  (if jazz:kernel-track-memory?
      (case (jazz:walk-for)
        ((compile)
         `(c-code #<<end-of-code
___RESULT = ___UPDATE_ALLOC(___ARG1);
end-of-code

            (jazz:track ,expr)))
        (else
         `(jazz:track ,expr)))
    expr))


(define jazz:*track-depth*
  4)


(define (jazz:track-depth)
  jazz:*track-depth*)

(define (jazz:track-depth-set! depth)
  (set! jazz:*track-depth* depth))


(define (jazz:track obj)
  (if (jazz:tracking-allocations?)
      (begin
        (jazz:untrack-allocations)
        (^#continuation-capture
          (lambda (cont)
            (let ((stack (jazz:track-continuation cont jazz:*track-depth*)))
              (jazz:update-stack obj stack))))
        (jazz:track-allocations)))
  obj)


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
                      ;; until jazz:track is a macro
                      (^#not (^#eq? (continuation-creator current-cont) jazz:track)))
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
          (let ((filepos (^#position->filepos (^#locat-start-position locat))))
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
  
  (identify cont 0))


(if jazz:kernel-track-memory?
    (set! ##track jazz:track)))
