;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Jazz Core
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


(unit protected jazz.backend.scheme.runtime.core


;;;
;;;; Continuation
;;;


(declare (proper-tail-calls))


(define (jazz:continuation-graft-no-winding cont proc)
  (jazz:check-continuation cont 1 (continuation-graft-no-winding cont values)
    (%%continuation-graft-no-winding cont proc)))


(define (jazz:continuation-parent cont)
  (jazz:check-continuation cont 1 (continuation-parent cont)
    (%%continuation-parent cont)))


(define (jazz:continuation-creator cont)
  (jazz:check-continuation cont 1 (continuation-creator cont)
    (%%continuation-creator cont)))


(define (jazz:continuation-locat cont)
  (jazz:check-continuation cont 1 (continuation-locat cont)
    (%%continuation-locat cont)))


(define (jazz:continuation-next cont)
  (jazz:check-continuation cont 1 (continuation-next cont)
    (%%continuation-next cont)))


;;;
;;;; Debug
;;;


;; inspect a Jazz object
(define (inspect obj)
  (jazz:inspect-object (if (integer? obj) (jazz:serial->object obj) obj)))


;; run the message loop
(define (run-loop)
  (let ((get-process (jazz:global-ref 'jazz.process:current-process))
        (run-loop (jazz:global-ref 'jazz.process.Process:Process:run-loop)))
    (run-loop (get-process))))


;; resume the message loop
(define (resume)
  (let ((get-process (jazz:global-ref 'jazz.process:current-process))
        (invoke-resume-loop (jazz:global-ref 'jazz.process.Process:Process:invoke-resume-loop)))
    (invoke-resume-loop (get-process))))


;; start a scheme repl
(define (start-scheme-repl #!key (select? #t))
  (jazz:load-unit 'jazz)
  (jazz:load-unit 'jazz.debuggee)
  ((jazz:module-ref 'jazz.debuggee 'set-default-context) #f)
  ((jazz:module-ref 'jazz 'start-repl) readtable: jazz:scheme-readtable select?: select?))


;;;
;;;; Devel
;;;


(define (jazz:subtype obj)
  (##subtype obj))


;;;
;;;; Global
;;;


(jazz:define-macro (jazz:define-global name)
  (let ((getter (%%string->symbol (%%string-append "get-" (%%symbol->string name))))
        (setter (%%string->symbol (%%string-append "set-" (%%symbol->string name))))
        (value (jazz:generate-symbol "value")))
    `(begin
       (define ,name
         #f)
       (define (,getter)
         ,name)
       (define (,setter ,value)
         (set! ,name ,value)))))


;;;
;;;; ? and %
;;;


(jazz:define-global ?)
(jazz:define-global ?a)
(jazz:define-global ?b)
(jazz:define-global ?c)
(jazz:define-global ?d)
(jazz:define-global ?e)
(jazz:define-global ?f)
(jazz:define-global ?g)
(jazz:define-global ?h)
(jazz:define-global ?i)
(jazz:define-global ?j)
(jazz:define-global ?k)
(jazz:define-global ?l)
(jazz:define-global ?m)
(jazz:define-global ?n)
(jazz:define-global ?o)
(jazz:define-global ?p)
(jazz:define-global ?q)
(jazz:define-global ?r)
(jazz:define-global ?s)
(jazz:define-global ?t)
(jazz:define-global ?u)
(jazz:define-global ?v)
(jazz:define-global ?w)
(jazz:define-global ?x)
(jazz:define-global ?y)
(jazz:define-global ?z)


(jazz:define-global %)
(jazz:define-global %a)
(jazz:define-global %b)
(jazz:define-global %c)
(jazz:define-global %d)
(jazz:define-global %e)
(jazz:define-global %f)
(jazz:define-global %g)
(jazz:define-global %h)
(jazz:define-global %i)
(jazz:define-global %j)
(jazz:define-global %k)
(jazz:define-global %l)
(jazz:define-global %m)
(jazz:define-global %n)
(jazz:define-global %o)
(jazz:define-global %p)
(jazz:define-global %q)
(jazz:define-global %r)
(jazz:define-global %s)
(jazz:define-global %t)
(jazz:define-global %u)
(jazz:define-global %v)
(jazz:define-global %w)
(jazz:define-global %x)
(jazz:define-global %y)
(jazz:define-global %z)


(jazz:define-global $)
(jazz:define-global $a)
(jazz:define-global $b)
(jazz:define-global $c)
(jazz:define-global $d)
(jazz:define-global $e)
(jazz:define-global $f)
(jazz:define-global $g)
(jazz:define-global $h)
(jazz:define-global $i)
(jazz:define-global $j)
(jazz:define-global $k)
(jazz:define-global $l)
(jazz:define-global $m)
(jazz:define-global $n)
(jazz:define-global $o)
(jazz:define-global $p)
(jazz:define-global $q)
(jazz:define-global $r)
(jazz:define-global $s)
(jazz:define-global $t)
(jazz:define-global $u)
(jazz:define-global $v)
(jazz:define-global $w)
(jazz:define-global $x)
(jazz:define-global $y)
(jazz:define-global $z)


;;;
;;;; Hook
;;;


(define (jazz:get-exception-hook)
  ##primordial-exception-handler-hook)

(define (jazz:set-exception-hook hook)
  (set! ##primordial-exception-handler-hook hook))


(define (jazz:invoke-exception-hook hook exc)
  (hook exc ##thread-end-with-uncaught-exception!))


;;;
;;;; System
;;;


(define (jazz:system-exception-hook exc other)
  (##repl-exception-handler-hook exc other))


;;;
;;;; Terminal
;;;


(define (jazz:set-terminal-title)
  (display "\033]0;Terminal\007" (repl-output-port)))

(define (jazz:bring-terminal-to-front)
  (display "\033[5t" (repl-output-port)))

(define (jazz:clear-terminal)
  (display "\033[H\033[J" (repl-output-port)))


;;;
;;;; Foreign
;;;


;(define (jazz:still-obj-refcount foreign)
;  (%%still-obj-refcount foreign))

(define (jazz:still-obj-refcount-dec! foreign)
  (jazz:check-foreign foreign 1 (still-obj-refcount-dec! foreign)
    (%%still-obj-refcount-dec! foreign)))

(define (jazz:still-obj-refcount-inc! foreign)
  (jazz:check-foreign foreign 1 (still-obj-refcount-inc! foreign)
    (%%still-obj-refcount-inc! foreign)))

(define jazz:still-copy
  ##still-copy)


;;;
;;;; Memory
;;;


(define (jazz:gc)
  (%%gc))


(define (jazz:gc-count)
  (%%flonum->fixnum (f64vector-ref (##process-statistics) 6)))


(define (jazz:gc-statistics)
  (let ((vec (##process-statistics)))
    (values (f64vector-ref vec 3)
            (f64vector-ref vec 4)
            (f64vector-ref vec 5)
            (%%flonum->fixnum (f64vector-ref vec 6)))))


(define (jazz:add-gc-interrupt-job! thunk)
  (##add-gc-interrupt-job! thunk))


(define (jazz:clear-gc-interrupt-jobs!)
  (##clear-gc-interrupt-jobs!))


(define (jazz:last-gc-real-time)
  (f64vector-ref (##process-statistics) 14))


;;;
;;;; Heap
;;;


(define jazz:get-min-heap
  ##get-min-heap)

(define jazz:set-min-heap!
  ##set-min-heap!)

(define jazz:get-max-heap
  ##get-max-heap)

(define jazz:set-max-heap!
  ##set-max-heap!)


(define (jazz:process-memory)
  (let ((vec (##process-statistics)))
    (let ((last_gc_heap_size  (f64vector-ref vec 15))
          (last_gc_alloc      (f64vector-ref vec 16))
          (last_gc_live       (f64vector-ref vec 17))
          (last_gc_movable    (f64vector-ref vec 18))
          (last_gc_nonmovable (f64vector-ref vec 19)))
      (values last_gc_heap_size
              last_gc_alloc
              last_gc_live
              last_gc_movable
              last_gc_nonmovable))))


(define (jazz:symbols-memory)
  (let ((count 0)
        (chars 0))
    (for-each (lambda (lst)
                (set! count (%%fx+ count (%%length lst)))
                (for-each (lambda (s)
                            (set! chars (%%fx+ chars (%%string-length (%%symbol->string s)))))
                          lst))
              (map (lambda (s)
                     (let loop ((s s) (lst '()))
                          (if (%%symbol? s)
                              (loop (%%vector-ref s 2) (%%cons s lst))
                            (%%reverse lst))))
                   (%%vector->list (##symbol-table))))
    (values count chars)))


(define jazz:symbol-table
  ##symbol-table)

(define jazz:keyword-table
  ##keyword-table)


(define jazz:bytes-allocated!
  ##get-bytes-allocated!)


(define (jazz:get-live-percent)
  (##get-live-percent))


(define (jazz:raise-heap-overflow-exception)
  (##raise-heap-overflow-exception))


#; ;; wait as this forces module to be compiled
(define (jazz:get-heap-pointer)
  (let ()
    (declare (extended-bindings))
    (##c-code "___RESULT = ___CAST(___SCMOBJ, ___hp);")))


;;;
;;;; Classes
;;;


(define (jazz:classes-statistics)
  (let ((nb-classes 0) (sz-classes 0)
        (nb-interfaces 0) (sz-interfaces 0)
        (nb-slots 0) (sz-slots 0)
        (nb-methods 0) (sz-methods 0))
    (define (process-class class)
      (set! nb-classes (%%fx+ nb-classes 1))
      (set! sz-classes (%%fx+ sz-classes (fx+ (jazz:vector-size class)
                                              (jazz:table-size (%%get-category-fields class))
                                              (jazz:vector-size (%%get-category-ancestors class))
                                              (jazz:list-size (%%get-category-descendants class))
                                              (jazz:list-size (%%get-class-interfaces class))
                                              (jazz:list-size (%%get-class-instance-slots class))
                                              (jazz:vector-vector-size (%%get-class-class-table class))
                                              (jazz:vector-vector-size (%%get-class-interface-table class)))))
      (jazz:iterate-table (%%get-category-fields class)
        (lambda (name field)
          (cond ((jazz:is? field jazz:Slot) (process-slot field))
                ((jazz:is? field jazz:Method) (process-method field)))))
      (for-each process-class (%%get-category-descendants class)))
    
    (define (process-slot slot)
      (set! nb-slots (%%fx+ nb-slots 1))
      (set! sz-slots (%%fx+ sz-slots (jazz:vector-size slot))))
    
    (define (process-method method)
      (set! nb-methods (%%fx+ nb-methods 1))
      (set! sz-methods (%%fx+ sz-methods (jazz:vector-size method))))
    
    (process-class jazz:Object)
    
    (values
      nb-classes sz-classes
      nb-interfaces sz-interfaces
      nb-slots sz-slots
      nb-methods sz-methods)))


;;;
;;;; Sizes
;;;


(define jazz:word-bytes   4)
(define jazz:f64-bytes    8)
(define jazz:pair-bytes  12)
(define jazz:table-bytes 32)


(define (jazz:vector-size v)
  (%%fx+ jazz:word-bytes (%%fx* jazz:word-bytes (%%vector-length v))))

(define (jazz:safe-vector-size v)
  (if (%%vector? v)
      (jazz:vector-size v)
    0))

(define (jazz:f64vector-size v)
  (%%fx+ jazz:word-bytes (%%fx* jazz:f64-bytes (f64vector-length v))))

(define (jazz:vector-vector-size v)
  (if (%%not v)
      0
    (%%fx+ (jazz:vector-size v)
           (let iter ((n 0) (size 0))
                (if (%%fx< n (%%vector-length v))
                    (let ((v (%%vector-ref v n)))
                      (iter (%%fx+ n 1) (%%fx+ size (if v (jazz:vector-size v) 0))))
                  size)))))

(define (jazz:list-size l)
  (%%fx* jazz:pair-bytes (%%length l)))

(define (jazz:table-size t)
  (let ((gcht1 (%%vector-ref t 3))
        (gcht2 (%%vector-ref t 5))
        (floats (%%vector-ref t 4)))
    (%%fx* jazz:word-bytes
           (fx+ 1 (%%vector-length t)
             ;; 1 (%%vector-length floats)
             (if (##gc-hash-table? gcht1) (%%fx+ 1 (%%vector-length gcht1)) 0)
             (if (##gc-hash-table? gcht2) (%%fx+ 1 (%%vector-length gcht2)) 0)))))


;;;
;;;; Network
;;;


(define (jazz:call-with-tcp-client settings proc)
  (let ((port #f))
    (dynamic-wind
      (lambda ()
        (set! port (open-tcp-client settings)))
      (lambda ()
        (proc port))
      (lambda ()
        (if port
            (close-port port))))))


;;;
;;;; Fixnum
;;;


;; not very efficient but have to test for now

(define (jazz:fixnum->flonum n)
  (if (%%fixnum? n)
      (%%fixnum->flonum n)
    (jazz:type-error n jazz:Fixnum)))

(define (jazz:flonum->fixnum n)
  (if (%%flonum? n)
      (%%flonum->fixnum n)
    (jazz:type-error n jazz:Flonum)))


(define (jazz:arithmetic-shift-left x y)
  (arithmetic-shift x y))

(define (jazz:arithmetic-shift-right x y)
  (arithmetic-shift x (- y)))


;;;
;;;; Flownum
;;;


(define (jazz:flalloc)
  (##subtype-set! (%%f64vector .0) 30))

(define (jazz:flref fl ignore)
  (%%unsafe-f64vector-ref fl 0))

(define (jazz:flset! fl ignore val)
  (%%unsafe-f64vector-set! fl 0 val))


;;;
;;;; Infinity
;;;


(define jazz:+infinity
  +inf.0)

(define jazz:-infinity
  -inf.0)


;;;
;;;; Random
;;;


(define jazz:random-integer-65536
  (let* ((rs (make-random-source))
         (ri (random-source-make-integers rs)))
    (random-source-randomize! rs)
    (lambda ()
      (ri 65536))))


;;;
;;;; Pathname
;;;


(define (jazz:current-directory . rest)
  (if (%%null? rest)
      (let ((dir (current-directory)))
        (jazz:pathname-normalize
          (if (jazz:pathname-exists? dir)
              dir
            (jazz:home-directory))))
    (let ((dir (%%car rest)))
      (current-directory dir))))

(define (jazz:with-current-directory dir thunk)
  (parameterize ((current-directory dir))
    (thunk)))


;;;
;;;; Port
;;;


(define jazz:write-string
  ##write-string)


;;;
;;;; Terminal
;;;


(cond-expand
  (windows
   (define (jazz:controlling-terminal?)
     #t))
  (else
   (define jazz:cached-controlling-terminal?
     (jazz:unspecified))
   
   (define (jazz:controlling-terminal?)
     (if (jazz:unspecified? jazz:cached-controlling-terminal?)
         (set! jazz:cached-controlling-terminal?
               (jazz:with-exception-filter
                 os-exception?
                 (lambda (exception)
                   #f)
                 (lambda ()
                   (close-port (open-file "/dev/tty"))
                   #t))))
     jazz:cached-controlling-terminal?)))


;;;
;;;; Console
;;;


(define (jazz:get-console-port)
  (if (jazz:global-bound? 'jazz.language.runtime.debug:current-console-port)
      ((jazz:global-ref 'jazz.language.runtime.debug:current-console-port))
    #f))


;;;
;;;; Readtable
;;;


(define (jazz:make-standard-readtable)
  (%%make-standard-readtable))

(define (jazz:readtable-copy readtable)
  (%%readtable-copy readtable))

(define (jazz:readtable-char-delimiter? readtable c)
  (%%readtable-char-delimiter? readtable c))

(define (jazz:readtable-char-delimiter?-set! readtable c delimiter?)
  (%%readtable-char-delimiter?-set! readtable c delimiter?))

(define (jazz:readtable-char-handler readtable c)
  (%%readtable-char-handler readtable c))

(define (jazz:readtable-char-handler-set! readtable c handler)
  (%%readtable-char-handler-set! readtable c handler))

(define (jazz:readtable-char-sharp-handler readtable c)
  (%%readtable-char-sharp-handler readtable c))

(define (jazz:readtable-char-sharp-handler-set! readtable c handler)
  (%%readtable-char-sharp-handler-set! readtable c handler))

(define (jazz:readtable-char-class-set! readtable c delimiter? handler)
  (%%readtable-char-class-set! readtable c delimiter? handler))

(define (jazz:readtable-escaped-char-table readtable)
  (%%readtable-escaped-char-table readtable))

(define (jazz:readtable-escaped-char-table-set! readtable table)
  (%%readtable-escaped-char-table-set! readtable table))


(define (jazz:char-symbol char)
  (let ((table (jazz:readtable-named-char-table jazz:jazz-readtable)))
    (let ((res (jazz:rassq char table)))
      (and res (%%car res)))))


;;;
;;;; General
;;;


(define (jazz:eof-object)
  #!eof)

(define (jazz:read-proper-line port)
  (let ((line (read-line port #\newline #t)))
    (if (eof-object? line)
        (values #f #f)
      (let ((len (%%string-length line)))
        (if (and (%%fx> len 0) (%%eqv? (%%string-ref line (%%fx- len 1)) #\newline))
            (values (%%string-shrink! line (%%fx- len 1)) #t)
          (values line #f))))))

(define (jazz:with-readtable readtable thunk)
  (parameterize ((current-readtable readtable))
    (thunk)))


(define (jazz:six-types)
  ##six-types)

(define (jazz:six-types-set! lst)
  (set! ##six-types lst))

(define (jazz:print-marker)
  #f)


(define (jazz:skip-whitespace port)
  (%%while (char-whitespace? (peek-char port))
    (read-char port)))


(define (jazz:read-delimited port delimiter)
  (let ((queue (jazz:new-queue)))
    (jazz:skip-whitespace port)
    (%%while (%%not (%%eqv? (peek-char port) delimiter))
      (jazz:enqueue queue (read port))
      (jazz:skip-whitespace port))
    (read-char port)
    (jazz:queue-list queue)))


(define (jazz:read-until test port)
  (let ((expr '())
        (queue (jazz:new-queue))
        (done? #f))
    (%%while (%%not done?)
      (let ((expr (read port)))
        (if (test expr)
            (set! done? #t)
          (jazz:enqueue queue expr))))
    (jazz:queue-list queue)))


(define (jazz:read-content port)
  (jazz:read-until eof-object? port))


;;;
;;;; Repository
;;;


(define (jazz:repository-name repository)
  (%%get-repository-name repository))

(define (jazz:repository-title repository)
  (%%symbol->string (%%get-repository-name repository)))

(define (jazz:repository-directory repository)
  (%%get-repository-directory repository))

(define (jazz:repository-library-root repository)
  (%%get-repository-library-root repository))

(define (jazz:repository-library-directory repository)
  (%%get-repository-library-directory repository))

(define (jazz:repository-binary? repository)
  (%%get-repository-binary? repository))

(define (jazz:repository-dependencies repository)
  (%%get-repository-dependencies repository))


;;;
;;;; Package
;;;


(define (jazz:package-repository package)
  (%%get-package-repository package))

(define (jazz:package-name package)
  (%%get-package-name package))

(define (jazz:package-directory package)
  (jazz:package-root-pathname package ""))

(define (jazz:package-products package)
  (%%get-package-products package))

(define (jazz:package-profiles package)
  (%%get-package-profiles package))

(define (jazz:package-profiles-set! package profiles)
  (%%set-package-profiles package profiles))

(define (jazz:package-project package)
  (%%get-package-project package))

(define (jazz:package-title package)
  (%%get-package-title package))

(define (jazz:package-description package)
  (%%get-package-description package))

(define (jazz:package-authors package)
  (%%get-package-authors package))

(define (jazz:package-stage package)
  (%%get-package-stage package))


;;;
;;;; Resource
;;;


(define (jazz:resource-package resource)
  (%%get-resource-package resource))

(define (jazz:resource-path resource)
  (%%get-resource-path resource))

(define (jazz:resource-extension resource)
  (%%get-resource-extension resource))


;;;
;;;; Stack
;;;


(define (jazz:procedure-name procedure)
  (jazz:check-procedure procedure 1 (procedure-name procedure)
    (%%procedure-name procedure)))

(define (jazz:procedure-name-set! proc)
  (set! jazz:procedure-name proc))

(define (jazz:procedure-locat procedure)
  (jazz:check-procedure procedure 1 (procedure-locat procedure)
    (%%procedure-locat procedure)))


(define (jazz:closure? obj)
  (and (%%procedure? obj)
       (%%closure? obj)))

(define (jazz:closure-code closure)
  (jazz:check-closure closure 1 (closure-code closure)
    (%%closure-code closure)))

(define (jazz:closure-length closure)
  (jazz:check-closure closure 1 (closure-length closure)
    (%%closure-length closure)))

(define (jazz:closure-ref closure n)
  (jazz:check-closure closure 1 (closure-ref closure)
    (%%closure-ref closure n)))

(define (jazz:closure-environment closure)
  ;; to do interpreted
  (if (##interp-procedure? closure)
      '()
    (let ((len (%%closure-length closure)))
      (let iter ((n 1) (env '()))
           (if (%%fx>= n len)
               env
             (iter (%%fx+ n 1) (%%cons (%%closure-ref closure n) env)))))))


(define jazz:hidden-frames
  (%%list
    ##dynamic-env-bind))

(define (jazz:hidden-frame? frame)
  (%%memq frame jazz:hidden-frames))

(define (jazz:hidden-frame?-set! predicate)
  (set! jazz:hidden-frame? predicate))


(define (jazz:get-continuation-stack cont depth)
  (let ((queue (jazz:new-queue)))
    (let iter ((d 0)
               (cont cont))
         (if (or (%%not depth) (%%fx< d depth))
             (and cont
                  (begin
                    (jazz:enqueue queue cont)
                    (iter (%%fx+ d 1)
                          (%%continuation-next-frame cont #f))))))
    (jazz:queue-list queue)))


(define (jazz:collect-var-val var val-or-box cte queue)
  (cond ((##var-i? var)
         (jazz:collect-var-val-aux (##var-i-name var)
                                   val-or-box
                                   #t
                                   cte
                                   queue))
        ((##var-c-boxed? var)
         (jazz:collect-var-val-aux (##var-c-name var)
                                   (##unbox val-or-box)
                                   #t
                                   cte
                                   queue))
        (else
         (jazz:collect-var-val-aux (##var-c-name var)
                                   val-or-box
                                   #f
                                   cte
                                   queue))))


(define (jazz:collect-var-val-aux var val mutable? cte queue)
  
  (define (remove-quote val)
    (if (and (pair? val)
             (eq? (##car val) 'quote)
             (##not (##null? (##cdr val))))
        (##cadr val)
      val))
  
  (jazz:enqueue queue
                (%%list (##object->string var)
                        (cond ((jazz:absent-object? val)
                               '<absent>)
                              ((jazz:unbound-object? val)
                               '<unbound>)
                              ((##procedure? val)
                               (remove-quote
                                 (if (##cte-top? cte)
                                     (##inverse-eval-in-env val cte)
                                   (##inverse-eval-in-env val (##cte-parent-cte cte)))))
                              (else
                               val))
                        mutable?)))


(define (jazz:get-continuation-dynamic-environment cont)
  
  (define (collect-parameters lst cte queue)
    (let iter ((lst lst))
         (if (%%pair? lst)
             (let* ((param-val (%%car lst))
                    (param (%%car param-val))
                    (val (%%cdr param-val)))
               (if (%%not (##hidden-parameter? param))
                   (let ((name (jazz:reference-name (##inverse-eval-in-env param cte))))
                     (jazz:collect-var-val-aux (%%list name) val #t cte queue)))
               (iter (%%cdr lst))))))
  
  (let ((queue (jazz:new-queue)))
    (and cont
         (collect-parameters
           (##dynamic-env->list (jazz:continuation-denv cont))
           (if (%%interp-continuation? cont)
               (let (($code (##interp-continuation-code cont)))
                 (jazz:code-cte $code))
             ##interaction-cte)
           queue))
    (jazz:queue-list queue)))


(define (jazz:get-continuation-lexical-environment cont)
  
  (define (collect-rte cte rte queue)
    (let loop1 ((c cte)
                (r rte))
         (cond ((##cte-top? c))
               ((##cte-frame? c)
                (let loop2 ((vars (##cte-frame-vars c))
                            (vals (%%cdr (%%vector->list r))))
                     (if (%%pair? vars)
                         (let ((var (%%car vars)))
                           (if (%%not (##hidden-local-var? var))
                               (jazz:collect-var-val var (%%car vals) c queue))
                           (loop2 (%%cdr vars)
                                  (%%cdr vals)))
                       (loop1 (##cte-parent-cte c)
                              (jazz:rte-up r)))))
               (else
                (loop1 (##cte-parent-cte c)
                       r)))))
  
  (define (collect-vars lst cte queue)
    (let iter ((lst lst))
         (if (%%pair? lst)
             (let* ((var-val (%%car lst))
                    (var (%%car var-val))
                    (val (%%cdr var-val)))
               (jazz:collect-var-val var val cte queue)
               (iter (%%cdr lst))))))
  
  (define (collect-locals lst cte queue)
    (and lst
         (collect-vars lst cte queue)))
  
  (let ((queue (jazz:new-queue)))
    (and cont
         (if (%%interp-continuation? cont)
             (let (($code (##interp-continuation-code cont))
                   (rte (##interp-continuation-rte cont)))
               (collect-rte (jazz:code-cte $code) rte queue)
               (jazz:code-cte $code))
           (begin
             (collect-locals (%%continuation-locals cont) ##interaction-cte queue)
             ##interaction-cte)))
    (jazz:queue-list queue)))


(define (jazz:get-continuation-location cont)
  (jazz:locat->container/line/col (%%continuation-locat cont)))


(define (jazz:interpreted-continuation? cont)
  (%%interp-continuation? cont))


(define (jazz:with-repl-context cont thunk)
  (let ((prev-context (%%thread-repl-context-get!)))
    (let ((context
            (jazz:make-repl-context
              (%%fx+ (jazz:repl-context-level prev-context) 1)
              0
              cont
              cont
              #f
              prev-context
              #f)))
      (jazz:repl-context-bind
        context
        thunk))))


(define (jazz:repl #!optional (write-reason #f))
  (begin
    (%%repl write-reason)
    #f))


(define (jazz:repl-debug #!optional (write-reason #f) (toplevel? #f))
  (begin
    (%%repl-debug write-reason toplevel?)
    #f))


;;;
;;;; Eval
;;;


;; copy-pasted from gambit's repl and changed to no-windind
(define (eval-within-no-winding runner src cont repl-context receiver)
  
  (define (run c rte)
    (%%continuation-graft-no-winding
      cont
      (lambda ()
        (jazz:repl-context-bind
          repl-context
          (lambda ()
            (receiver
              (let ((rte rte))
                (runner
                  (lambda ()
                    (jazz:code-run c rte))))))))))
  
  (##define-macro (macro-make-rte-from-list rte lst)
    `(##list->vector (##cons ,rte ,lst)))
  
  (let ((src2 (##sourcify src (##make-source #f #f))))
    (cond ((##interp-continuation? cont)
           (let* (($code (##interp-continuation-code cont))
                  (cte (jazz:code-cte $code))
                  (rte (##interp-continuation-rte cont)))
             (run (##compile-inner cte src2) rte)))
          ((##with-no-result-expected-toplevel-continuation? cont)
           (run (##compile-top ##interaction-cte src2) #f))
          (else
           (let* ((locals (##continuation-locals cont))
                  (cte (##cte-frame (##cte-top-cte ##interaction-cte)
                                    (##map ##car locals)))
                  (rte (macro-make-rte-from-list #f (##map ##cdr locals))))
             (run (##compile-inner cte src2) rte))))))


(define (jazz:eval-within-no-winding runner expr cont)
  (continuation-capture
    (lambda (return)
      (eval-within-no-winding
        runner
        expr
        cont
        (jazz:current-repl-context)
        (lambda (results)
          (call-with-values
            (lambda ()
              results)
            (lambda results
              (%%continuation-return-no-winding return (%%car results)))))))))


(define (jazz:repl-result-history-add result)
  (let ((channel (%%thread-repl-channel-get! (%%current-thread))))
    (%%repl-channel-result-history-add channel result)))


;;;
;;;; Debugging
;;;


(define (jazz:inspect-repl-context context)
  `(:repl-context
    ,(jazz:repl-context-level context)
    ,(jazz:repl-context-depth context)
    ,(jazz:repl-context-cont context)
    ,(jazz:repl-context-initial-cont context)
    ,(jazz:repl-context-prev-level context)
    ,(jazz:repl-context-prev-depth context)))


;;;
;;;; Step
;;;


(define (jazz:install-step-handler proc)
  (declare (proper-tail-calls))
  (define (handler leapable? $code rte execute-body . other)
    (##step-off)
    (jazz:process-step
      proc
      $code
      (lambda ()
        (##apply execute-body (##cons $code (##cons rte other))))))
  
  (let ((cs (##current-stepper)))
    (vector-set! cs 0 (vector handler handler handler handler handler handler handler))
    (void)))


(define (jazz:process-step proc $code execute)
  (declare (proper-tail-calls))
  (proc
    (##code-locat $code)
    (lambda (cmd)
      (case cmd
        ((step)
         (##step-on)
         (execute))
        ((leap) ;; does this really work????
         (let ((result (execute)))
           (##step-on)
           result))
        ((continue)
         (execute))))))


;;;
;;;; Structure
;;;


(define (jazz:kind? obj)
  (##type? obj))

(define (jazz:kind-id type)
  (##type-id type))

(define (jazz:kind-name type)
  (##type-name type))

(define (jazz:kind-flags type)
  (##type-flags type))

(define (jazz:kind-super type)
  (##type-super type))

(define (jazz:kind-length type)
  (##type-field-count type))

(define (jazz:kind-fields type)
  (let loop ((index 1)
             (fields (##type-all-fields type))
             (alist '()))
       (if (%%pair? fields)
           (let* ((name (%%car fields))
                  (rest (%%cdr fields))
                  (options (%%car rest))
                  (rest (%%cdr rest))
                  (val (%%car rest))
                  (rest (%%cdr rest)))
             (loop (%%fx+ index 1)
                   rest
                   (%%cons (%%list name index options val)
                           alist)))
         (jazz:reverse! alist))))


(define (jazz:structure? obj)
  (##structure? obj))

(define (jazz:structure-kind obj)
  (##structure-type obj))

(define (jazz:structure-ref obj i type)
  (##structure-ref obj i type #f))

(define (jazz:structure-set! obj val i type)
  (##structure-set! obj val i type #f))


;;;
;;;; Process
;;;


(define (jazz:exit-no-jobs #!optional (status 1))
  (##clear-exit-jobs!)
  (##exit status))


(define (jazz:switch? arg)
  (and (%%fx> (%%string-length arg) 0)
       (%%eqv? (%%string-ref arg 0) #\-)))


(define (jazz:switch-name arg)
  (let ((len (%%string-length arg)))
    (let ((start (if (and (%%fx>= len 2) (%%equal? (%%substring arg 0 2) "--"))
                     2
                   1)))
      (%%substring arg start len))))


(define (jazz:command-argument name #!key (error? #t))
  (define (skip-scripts arguments)
    (let iter ((arguments arguments))
      (if (or (%%null? arguments)
              (jazz:switch? (%%car arguments)))
          arguments
        (iter (%%cdr arguments)))))
  
  (if (eq? jazz:image 'executable)
      (let ((all (jazz:command-arguments)))
        (let iter ((arguments (skip-scripts all)))
          (if (%%null? arguments)
              #f
            (let ((arg (%%car arguments)))
              (cond ((%%member (jazz:switch-name arg) jazz:kernel-runtime-switches)
                     (iter (%%cdr arguments)))
                    ((or (%%not (jazz:switch? arg))
                         (%%null? (%%cdr arguments)))
                     (if error?
                         (jazz:error "Unable to parse command line: {a}" all)
                       #f))
                    ((%%equal? name (jazz:switch-name arg))
                     (%%cadr arguments))
                    (else
                     (iter (%%cddr arguments))))))))
    #f))


(define (jazz:command-argument? name)
  (if (eq? jazz:image 'executable)
      (let ((all (jazz:command-arguments)))
        (let iter ((arguments all))
          (if (%%null? arguments)
              #f
            (let ((arg (%%car arguments)))
              (if (and (jazz:switch? arg)
                       (%%equal? name (jazz:switch-name arg)))
                  #t
                (iter (%%cdr arguments)))))))
    #f))


;;;
;;;; Serialized
;;;


(define-type serialized
  id: 16E8E4BA-FD59-4E7A-A219-DFE934B6CC18

  (class read-only:)
  (content read-only:))


;;;
;;;; Table
;;;


(define (jazz:table-clear table key)
  (%%debug-assert (%%table? table)
    (%%table-clear table key)))


(define (jazz:table-keys table)
  (%%debug-assert (%%table? table)
    (%%table-keys table)))


(define (jazz:map-table table proc)
  (%%debug-assert (%%table? table)
    (let ((queue (jazz:new-queue)))
      (jazz:iterate-table table
        (lambda (key value)
          (jazz:enqueue queue (proc key value))))
      (jazz:queue-list queue))))


(define (jazz:list->table alist #!key (test equal?))
  (%%list->table alist test: test))


(define (jazz:table->list table)
  (%%debug-assert (%%table? table)
    (%%table->list table)))


(define (jazz:table-entries table)
  (%%debug-assert (%%table? table)
    (%%table-entries table)))


;;;
;;;; Interrupts
;;;


(define jazz:*interrupts-enabled?*
  #t)

(define (jazz:interrupts-enabled?)
  jazz:*interrupts-enabled?*)

(define (jazz:disable-interrupts!)
  (##disable-interrupts!)
  (set! jazz:*interrupts-enabled?* #f))

(define (jazz:enable-interrupts!)
  (##enable-interrupts!)
  (set! jazz:*interrupts-enabled?* #t))


;;;
;;;; Heartbeat
;;;


(define jazz:get-heartbeat-interval
  (let ((vec (%%f64vector 0.)))
    (lambda ()
      (declare (not interrupts-enabled))
      (##get-heartbeat-interval! vec 0)
      (%%f64vector-ref vec 0))))

(define jazz:set-heartbeat-interval! ##set-heartbeat-interval!)


;;;
;;;; Thread
;;;


;; need to fix this correctly
(define jazz:thread-interrupt! (lambda rest
                                 (jazz:with-exception-filter
                                   (lambda (exc)
                                     #t)
                                   (lambda (exc)
                                     #t)
                                   (lambda ()
                                     (apply thread-interrupt! rest))))
        #; thread-interrupt!
        )

(define (jazz:thread-state-active? state)
  (or (thread-state-running? state)
      (thread-state-waiting? state)))


(define (jazz:thread-continuation thread)
  (declare (not interrupts-enabled))
  (if (jazz:thread-state-active? (thread-state thread))
      (let ((cont (jazz:thread-cont thread)))
        ;; hack - gambit thread init with dummy continuation
        (if (%%not (%%eq? (%%vector-ref cont 0) #!void))
            cont
          #f))
    #f))


(define (jazz:thread-mutexes thread)
  (declare (not interrupts-enabled))
  (let ((mutexes '()))
    (let iter ((mutex (%%vector-ref thread 2)) (mutexes '()))
         (if (%%eq? mutex thread)
             mutexes
           (iter (%%vector-ref mutex 2) (cons mutex mutexes))))))


;; copied from _repl
(define (jazz:write-timeout to now port)
  (##write-string " " port)
  (let* ((expiry (##fl- to now))
         (e (##fl/ (##flround (##fl* 10.0 expiry)) 10.0)))
    (##write (if (##integer? e) (##inexact->exact e) e) port))
  (##write-string "s" port))


;;;
;;;; Thread With Stack
;;;


(define jazz:word-size
  (%%unsafe-u8vector-length '#(0)))


(define (jazz:header-get obj)
  (if (= jazz:word-size 4)
      (%%u32vector-ref obj -1)
    (%%u64vector-ref obj -1)))

(define (jazz:header-set! obj h)
  (if (= jazz:word-size 4)
      (%%u32vector-set! obj -1 h)
    (%%u64vector-set! obj -1 h)))


(define (jazz:make-thread-with-stack stack-len thunk)
  (let* ((pt ##primordial-thread)
         (thread-len (%%vector-length pt))
         (total-len (+ thread-len stack-len))
         (thread (make-vector total-len #f)))
    (%%unsafe-vector-set! thread 0 (%%vector-ref pt 0))
    (jazz:header-set! thread
                      (+ (bitwise-and (jazz:header-get pt) -4)
                         (bitwise-and (jazz:header-get thread) 3)))
    (thread-init! thread thunk)
    (thread-specific-set! thread (+ thread-len 1))
    thread))


;; work around PERM not being mutable
(define jazz:force-f64vector-set!
  ##f64vector-set!)


;;;
;;;; Mutex
;;;


(define (jazz:mutex-owner mutex)
  (jazz:btq-owner mutex))

(define (jazz:mutex-wait mutex)
  (mutex-lock! mutex)
  (mutex-unlock! mutex))


;;;
;;;; Time
;;;


(define jazz:current-systime current-time)
(define jazz:current-seconds! ##get-current-time!)
(define jazz:systime? time?)
(define jazz:systime->seconds time->seconds)
(define jazz:seconds->systime seconds->time))
